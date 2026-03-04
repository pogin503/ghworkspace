# パケットフロー

→ [index.md](index.md) | [アーキテクチャ](architecture.md) | [チェックサム](checksum.md)

---

## イベントループ

メインループは単一スレッドのブロッキング I/O で動作します (`src/main.c`)。

```c
while (running) {
    ssize_t n = tun_read(&dev, buf, sizeof(buf));  // ブロッキング読み取り
    eth_recv(&dev, buf, (size_t)n);                 // 同期処理
}
```

フレームを 1 つ受信するたびに同期的にプロトコル処理を行い、応答を送信します。
並列処理・非同期処理・再送タイマーはありません。

---

## 受信フロー

```
tun_read(&dev, buf, 1518)
  │  TAP デバイスから raw Ethernet フレームを読み取る
  ▼
eth_recv(&dev, frame, len)                         [src/ethernet.c]
  │  ① フレーム長 ≥ 14 バイトを確認
  │  ② ethertype で振り分け
  │
  ├─ 0x0806 (ARP) ──────────────────────────────────────────────┐
  │                                                              ▼
  │                                           arp_recv(&dev, payload, len) [src/arp.c]
  │                                             ① hw_type=1, proto=0x0800 を確認
  │                                             ② 送信元 IP/MAC をキャッシュに登録
  │                                             ③ target_ip == dev->ip の場合
  │                                                → arp_send_reply() で Reply 送信
  │
  └─ 0x0800 (IP) ────────────────────────────────────────────────┐
                                                                  ▼
                                              ip_recv(&dev, pkt, len)  [src/ip.c]
                                                ① 長さ ≥ 20 バイトを確認
                                                ② version == 4 を確認
                                                ③ チェックサム検証
                                                ④ dst_ip == dev->ip を確認 ※1
                                                ⑤ total_len の整合性確認
                                                ⑥ protocol で振り分け
                                                │
                    ┌───────────────────────────┤
                    │ 1 (ICMP)                  │ 6 (TCP)
                    ▼                           ▼
        icmp_recv(&dev, src_ip,     tcp_recv(&dev, src_ip, dst_ip,
                  payload, len)               segment, seg_len)
         [src/icmp.c]                [src/tcp.c]
          ① チェックサム検証          ① チェックサム検証 (擬似ヘッダ込み)
          ② type=8, code=0 のみ処理   ② 4タプルで既存接続を検索
          ③ Echo Request なら          ③ なければ LISTEN ソケットを検索
             → icmp_send_echo_reply()  ④ tcp_state_machine() を呼び出す
```

**※1** `dst_ip != dev->ip` のパケット (他ホスト宛) はここでサイレントドロップします。

---

## 送信フロー

TCP 送信を例に、スタック内部から TAP デバイスへ至る流れを示します。

```
tcp_send_segment(&dev, conn, flags, data, data_len)  [src/tcp.c]
  │  ① malloc でセグメントバッファを確保
  │  ② TCP ヘッダを組み立て
  │     src_port, dst_port, seq_num, ack_num, flags, window
  │  ③ データをヘッダの後ろにコピー
  │  ④ チェックサムを計算して埋め込み (擬似ヘッダ + セグメント全体)
  │     → 詳細は checksum.md 参照
  │  ⑤ snd_nxt を更新 (data_len + SYN フラグ消費分 + FIN フラグ消費分)
  ▼
ip_send(&dev, remote_ip, IP_PROTO_TCP, seg, seg_len)  [src/ip.c]
  │  ① malloc で IP パケットバッファを確保
  │  ② IP ヘッダを組み立て
  │     version=4, ihl=5, total_len, id (自動インクリメント), ttl=64, protocol=6
  │     src_ip = dev->ip, dst_ip = remote_ip
  │  ③ IP ヘッダチェックサムを計算して埋め込み (ip_checksum_fill)
  │  ④ TCP セグメントをヘッダの後ろにコピー
  │  ⑤ arp_cache_lookup(remote_ip) で宛先 MAC を取得
  │     → キャッシュミスなら arp_send_request() を送信してパケットをドロップ ※2
  ▼
eth_send(&dev, dst_mac, ETHERTYPE_IP, pkt, ip_len)  [src/ethernet.c]
  │  ① malloc で Ethernet フレームバッファを確保
  │  ② Ethernet ヘッダを組み立て
  │     dst_mac, src_mac = dev->mac, ethertype = 0x0800
  │  ③ IP パケットをヘッダの後ろにコピー
  ▼
tun_write(&dev, frame, total_len)  [src/tun.c]
     write(dev->fd, buf, len) で TAP デバイスに書き込む
     → カーネルの tap0 インタフェース経由でホストに届く
```

**※2** ARP キャッシュミス時は再送機構がないため、そのパケットは失われます。
実際には相手からの次のパケット受信時に ARP キャッシュが更新されます。

---

## ICMP 送信フロー

```
icmp_send_echo_reply(&dev, dst_ip, id, seq, data, data_len)  [src/icmp.c]
  │  ① ICMP ヘッダ組み立て (type=0, code=0)
  │  ② データをコピー
  │  ③ チェックサムを計算して埋め込み (ヘッダ+データ全体)
  ▼
ip_send(&dev, dst_ip, IP_PROTO_ICMP, buf, total_len)
  └→ eth_send() → tun_write()  (TCP 送信フローと同様)
```

---

## 各層でのフィルタ条件まとめ

| 層 | 条件 | 不合格時の動作 |
|----|------|-------------|
| Ethernet | フレーム長 ≥ 14 バイト | エラーログ・ドロップ |
| Ethernet | EtherType が ARP または IP | サイレントドロップ |
| IP | 長さ ≥ 20 バイト、version=4 | エラーログ・ドロップ |
| IP | チェックサム正当 | エラーログ・ドロップ |
| IP | dst_ip == dev->ip | サイレントドロップ |
| ICMP | チェックサム正当 | エラーログ・ドロップ |
| ICMP | type=8 (Echo Request) | サイレントドロップ |
| TCP | チェックサム正当 (擬似ヘッダ込み) | エラーログ・ドロップ |
| TCP | 対応するコネクションまたは LISTEN ソケットが存在 | エラーログ・ドロップ |
