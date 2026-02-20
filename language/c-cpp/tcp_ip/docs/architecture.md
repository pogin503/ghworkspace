# アーキテクチャ

ユーザー空間TCP/IPスタックの実装概要と各モジュールの設計を説明します。

## 全体像

LinuxのTAPデバイス (`/dev/net/tun`) を介してEthernetフレームを直接読み書きし、カーネルのネットワークスタックを完全に迂回してプロトコル処理をユーザー空間で行います。

```
ホストのカーネル
  ┌─────────────────┐
  │  tap0 インターフェース  │  ← ip addr add 10.0.0.1/24
  └────────┬────────┘
           │ /dev/net/tun (IFF_TAP | IFF_NO_PI)
           │ rawフレームの読み書き
           ▼
┌──────────────────────────────────────────────┐
│           ユーザー空間スタック (tcp_stack)          │
│                                              │
│  tun_read/tun_write                          │
│         ↕                                    │
│  [ ethernet.c ]  EtherTypeで振り分け           │
│      ↙        ↘                              │
│  [ arp.c ]   [ ip.c ]  IPプロトコルで振り分け     │
│  ARPキャッシュ   ↙      ↘                      │
│          [ icmp.c ] [ tcp.c ]                │
│           ping応答    RFC 793状態機械           │
└──────────────────────────────────────────────┘
```

## ディレクトリ構成

```
tcp_ip/
├── Makefile            # gcc ビルド
├── src/                # 実装ファイル
│   ├── main.c          # エントリーポイント・イベントループ
│   ├── tun.c           # TAPデバイスI/O
│   ├── ethernet.c      # Layer 2: Ethernetフレーム処理
│   ├── arp.c           # ARP (IPアドレス→MACアドレス解決)
│   ├── ip.c            # Layer 3: IPv4パケット処理
│   ├── icmp.c          # ICMP (ping応答)
│   ├── tcp.c           # Layer 4: TCP状態機械
│   └── utils.c         # チェックサム・ログユーティリティ
├── include/            # ヘッダファイル
├── fuzz/               # libFuzzerによるfuzz testing
│   ├── Makefile        # clang + ASAN/UBSAN ビルド
│   ├── tun_mock.c      # TAPデバイスのモック (no-op)
│   ├── fuzz_eth.c      # eth_recv() ファズターゲット
│   ├── fuzz_arp.c      # arp_recv() ファズターゲット
│   ├── fuzz_ip.c       # ip_recv() ファズターゲット
│   ├── fuzz_icmp.c     # icmp_recv() ファズターゲット
│   ├── fuzz_tcp.c      # tcp_recv() ファズターゲット
│   ├── generate_seeds.c # シードコーパス生成
│   └── corpus/         # シードファイル
└── docs/               # このドキュメント
```

## イベントループ

メインループは単一スレッドのブロッキングI/Oで動作します。

```c
// src/main.c
while (running) {
    ssize_t n = tun_read(&dev, buf, sizeof(buf));  // ブロッキング
    eth_recv(&dev, buf, (size_t)n);                 // 同期処理
}
```

フレームを受信するたびに同期的にプロトコル処理を行い、応答パケットを送信します。

## データフロー: 受信

```
tun_read()
  └→ eth_recv(frame, len)
       ├─ EtherType = 0x0806 (ARP)
       │    └→ arp_recv(payload, len)
       │         ├─ ARPキャッシュを更新
       │         └─ ARP Requestなら arp_send_reply()
       │
       └─ EtherType = 0x0800 (IP)
            └→ ip_recv(pkt, len)
                 ├─ プロトコル = 1 (ICMP)
                 │    └→ icmp_recv(src_ip, payload, len)
                 │         └─ Echo Requestなら icmp_send_echo_reply()
                 │
                 └─ プロトコル = 6 (TCP)
                      └→ tcp_recv(src_ip, dst_ip, segment, len)
                           └→ tcp_state_machine(conn, hdr, data, ...)
```

## データフロー: 送信

```
tcp_send_segment(conn, flags, data, len)
  └→ ip_send(dst_ip, IP_PROTO_TCP, payload, len)
       ├─ arp_cache_lookup(dst_ip) → dst_mac
       │   (MACが不明なら arp_send_request() してドロップ)
       └→ eth_send(dst_mac, ETHERTYPE_IP, pkt, len)
            └→ tun_write(frame, len)
```

## モジュール詳細

### tun.c — TAPデバイス管理

`IFF_TAP | IFF_NO_PI` フラグで `/dev/net/tun` をオープンします。

- `IFF_TAP`: Layer 2 (Ethernet) フレームを扱う (TUNはLayer 3のIPパケット)
- `IFF_NO_PI`: `struct tun_pi` プレフィックスなしでrawフレームを読み書きする

これによりread/writeで得られるバイト列がそのままEthernetフレームになります。

```c
struct netdev {
    int      fd;        // /dev/net/tunのファイルディスクリプタ
    char     name[16];  // "tap0" など
    uint8_t  mac[6];    // スタックのMACアドレス
    uint32_t ip;        // スタックのIPアドレス (ネットワークバイトオーダー)
    uint32_t netmask;
};
```

### ethernet.c — Layer 2

EtherTypeで受信フレームをARPまたはIPにディスパッチします。

```c
struct eth_hdr {
    uint8_t  dst_mac[6];
    uint8_t  src_mac[6];
    uint16_t ethertype;  // 0x0800=IP, 0x0806=ARP
} __attribute__((packed));
```

`__attribute__((packed))` によりパディングなしでメモリに配置し、受信フレームを直接キャストして解釈します。

### arp.c — アドレス解決

ARPキャッシュは32エントリの静的配列で、線形探索で管理します。

```c
struct arp_entry {
    uint32_t ip;   // ネットワークバイトオーダー、0 = 空きスロット
    uint8_t  mac[6];
};
```

動作フロー:
1. ARP Requestを受信 → 送信元をキャッシュに登録
2. 自分のIPへのRequestなら ARP Replyを送信
3. `ip_send()` が宛先MACを必要とするとき `arp_cache_lookup()` を呼び出す

### ip.c — Layer 3

IPv4ヘッダの検証順序:

1. 最小長チェック (≥ 20バイト)
2. バージョンフィールド (必ず4)
3. チェックサム検証
4. 宛先IP確認 (自スタックのIPと一致するか)
5. total_len の整合性チェック
6. プロトコルフィールドでICMP/TCPにディスパッチ

```c
struct ip_hdr {
    uint8_t  version_ihl;  // 上位4bit=version, 下位4bit=IHL
    uint8_t  dscp_ecn;
    uint16_t total_len;
    uint16_t id;
    uint16_t flags_frag;
    uint8_t  ttl;
    uint8_t  protocol;
    uint16_t checksum;
    uint32_t src_ip;
    uint32_t dst_ip;
} __attribute__((packed));
```

### icmp.c — ping応答

ICMP Echo Requestを受け取り、Echo Replyを返すだけのシンプルな実装です。
IDとシーケンス番号を保持して、データペイロードをそのままコピーして返します。

### tcp.c — RFC 793 TCP状態機械

TCP接続テーブルは16スロットの静的配列で管理します。

```c
struct tcp_conn {
    tcp_state_t state;
    uint32_t local_ip, remote_ip;
    uint16_t local_port, remote_port;
    uint32_t snd_una;  // 未ACKの最古シーケンス番号
    uint32_t snd_nxt;  // 次に送信するシーケンス番号
    uint16_t snd_wnd;
    uint32_t rcv_nxt;  // 次に期待するシーケンス番号
    uint16_t rcv_wnd;
    uint8_t  recv_buf[8192];
    size_t   recv_len;
};
```

#### 状態遷移図

```
                    CLOSED
                      │ tcp_listen()
                      ▼
                    LISTEN ←────────────────────────────┐
                      │ SYN受信                          │
                      │ → SYN+ACK送信                    │
                      ▼                                  │
                SYN_RECEIVED                             │
                      │ ACK受信                          │ (新しい接続は
                      ▼                                  │  LISTENソケットを
                ESTABLISHED ───── RST受信 ──→ CLOSED     │  複製して作成)
                      │                                  │
          ┌───────────┤                                  │
          │ FIN受信   │                                  │
          ▼           │                                  │
      CLOSE_WAIT      │ (アクティブクローズは未実装)       │
          │           │                                  │
          ▼           │                                  │
      LAST_ACK        │                                  │
          │ ACK受信   │                                  │
          ▼           │                                  │
        CLOSED ───────┴──────────────────────────────────┘
```

#### TCPチェックサム計算

TCPチェックサムはTCPセグメント単体ではなく、IP擬似ヘッダを含めて計算します (RFC 793)。

```c
struct ip_pseudo_hdr {
    uint32_t src_ip;
    uint32_t dst_ip;
    uint8_t  zero;
    uint8_t  protocol;  // = 6 (TCP)
    uint16_t tcp_len;
} __attribute__((packed));

// チェックサム計算
uint32_t csum = checksum_partial(&pseudo, sizeof(pseudo), 0);
csum          = checksum_partial(tcp_segment, seg_len, csum);
hdr->checksum = checksum_finish(csum);
```

### utils.c — RFC 1071 チェックサム

```c
// 16ビット語の和 (ビッグエンディアン)
uint32_t checksum_partial(const void *data, size_t len, uint32_t init);

// 32ビット→16ビットへの折り畳みと1の補数
uint16_t checksum_finish(uint32_t partial);

// 検証: checksum(data_including_checksum_field) == 0 なら正当
uint16_t checksum(const void *data, size_t len);
```

複数バッファにまたがるチェックサム (TCP擬似ヘッダ + セグメント) は `checksum_partial` を2回呼ぶことで計算できます。

## Fuzz Testing のアーキテクチャ

### 設計方針

各ファズターゲットは実際のTAPデバイスを一切使わず、`tun_mock.c` で `tun_write` をno-op関数に置き換えます。これにより root権限なしで任意のバイト列をパーサーに直接入力できます。

```
libFuzzer
  │ 変異した入力バイト列
  ▼
LLVMFuzzerTestOneInput(data, size)
  │ (1) チェックサムを再計算して埋め込む
  │ (2) パーサーに渡す
  ▼
tcp_recv() / ip_recv() / etc.
  │ 内部で ip_send() → eth_send() を呼ぶが
  ▼
tun_write() [mock: no-op, 即座に戻る]
```

### チェックサム補正の理由

プロトコルパーサーはチェックサムが不正なパケットを入口で弾きます。ランダム入力をそのまま渡すとほぼすべてが弾かれ、パーサーの深い部分を探索できません。

各ファズターゲットでは入力バッファのチェックサムフィールドを毎回再計算することで、ファザーが状態機械やヘッダフィールドの境界条件を探索できるようにしています。

### TCP用のイテレーション間リセット

TCP接続テーブルはプロセス内で状態を持ち続けます。毎イテレーションで `tcp_reset_for_fuzz()` を呼ぶことで接続テーブルをクリアし、LISTENソケットを再作成します。これにより毎回 `LISTEN → SYN_RECEIVED → ESTABLISHED` の遷移から探索できます。

```c
// FUZZING マクロが定義されている場合のみコンパイルされる
void tcp_reset_for_fuzz(uint32_t local_ip, uint16_t port) {
    memset(tcp_conns, 0, sizeof(tcp_conns));
    tcp_listen(local_ip, port);
}
```

## 設計上の簡略化

教育目的の実装のため、以下は省略・簡略化しています。

| 機能 | 説明 |
|------|------|
| TIME_WAIT状態 | FIN受信後に即CLOSEDへ遷移 |
| TCPオプション | 全て無視 (ウィンドウスケーリング、SACK等) |
| 再送タイマー | 実装なし (パケットロスは考慮しない) |
| 輻輳制御 | 実装なし |
| IPv6 | 未対応 (IPv4のみ) |
| IPフラグメント | 未対応 |
| ARP TTL/LRU | なし (上書き方式) |
| マルチスレッド | 単一スレッドのみ |
