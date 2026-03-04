# アーキテクチャ

→ [index.md](index.md)

---

## レイヤー構造

Linux の TAP デバイスを介して Ethernet フレームをユーザ空間で直接処理します。
カーネルのネットワークスタックを完全に迂回し、以下のレイヤー構成で実装されています。

```
┌─────────────────────────────────────────────────────────┐
│  アプリケーション  src/main.c                            │
│  select() ループで TAP フレームを監視・処理             │
└─────────────────────┬───────────────────────────────────┘
                      │ tun_read() / tun_write()
          ┌───────────▼──────────────────────┐
          │  TAP デバイス /dev/net/tun        │
          │  (IFF_TAP | IFF_NO_PI)           │
          └───────────┬──────────────────────┘
                      │ raw Ethernet フレーム
┌─────────────────────▼───────────────────────────────────┐
│  Ethernet  src/ethernet.c                               │
│  EtherType でプロトコルを判別してディスパッチ            │
└────────────┬────────────────────────────────────────────┘
             │ 0x0806 (ARP)           │ 0x0800 (IP)
  ┌──────────▼──────────┐   ┌─────────▼────────────────────┐
  │  ARP  src/arp.c     │   │  IP  src/ip.c               │
  │  キャッシュ 32 エントリ │   │  protocol 番号でディスパッチ │
  └─────────────────────┘   └──────┬─────────────┬──────────┘
                                   │ 1 (ICMP)    │ 6 (TCP)
                        ┌──────────▼──┐  ┌───────▼──────────┐
                        │ ICMP icmp.c │  │ TCP  src/tcp.c   │
                        │ Echo のみ   │  │ RFC 793 状態機械 │
                        └─────────────┘  └──────────────────┘
```

---

## モジュール一覧

| ファイル | 行数 | 役割 | 公開関数 |
|---------|------|------|---------|
| `src/tun.c` | 74 | TAP デバイス I/O | `tun_alloc`, `tun_read`, `tun_write`, `tun_close` |
| `src/ethernet.c` | 57 | Ethernet II フレームのパース・組立 | `eth_recv`, `eth_send` |
| `src/arp.c` | 153 | ARP リクエスト/リプライ・キャッシュ管理 | `arp_recv`, `arp_cache_lookup`, `arp_cache_update`, `arp_send_request` |
| `src/ip.c` | 120 | IPv4 ヘッダ検証・組立・チェックサム | `ip_recv`, `ip_send`, `ip_checksum_valid`, `ip_checksum_fill` |
| `src/icmp.c` | 75 | ICMP Echo Request 処理・Reply 送信 | `icmp_recv`, `icmp_send_echo_reply` |
| `src/tcp.c` | 359 | RFC 793 TCP 状態機械・接続テーブル | `tcp_recv`, `tcp_send_segment`, `tcp_listen`, `tcp_conn_lookup` |
| `src/utils.c` | 87 | RFC 1071 チェックサム・ログ出力 | `checksum_partial`, `checksum_finish`, `checksum` |
| `src/main.c` | 129 | イベントループ・初期化 | `main` |

テスト専用ファイル (`src/unity.c`, `src/unity.h`) はメインバイナリから除外されています。

---

## 主要データ構造

### `struct netdev` — デバイス情報 (`include/tun.h`)

```c
struct netdev {
    int      fd;         // TAP デバイスの fd (/dev/net/tun)
    char     name[16];   // インタフェース名 (例: "tap0")
    uint8_t  mac[6];     // スタックの MAC アドレス
    uint32_t ip;         // スタックの IP (ネットワークバイトオーダー)
    uint32_t netmask;    // サブネットマスク
};
```

---

### ワイヤーフォーマット構造体

全て `__attribute__((packed))` で受信バッファを直接キャストして解釈します。
全フィールドはネットワークバイトオーダー (big-endian) で格納されています。

#### `struct eth_hdr` — Ethernet II ヘッダ (14 バイト, `include/ethernet.h`)

```
 0               6              12      14
 ┌───────────────┬──────────────┬───────┐
 │  dst_mac[6]   │  src_mac[6]  │ etype │
 └───────────────┴──────────────┴───────┘
```

| フィールド | 値の例 |
|----------|------|
| `dst_mac[6]` | 宛先 MAC |
| `src_mac[6]` | 送信元 MAC |
| `ethertype` | `0x0800`=IP, `0x0806`=ARP |

#### `struct arp_hdr` — ARP パケット (28 バイト, `include/arp.h`)

| フィールド | 説明 |
|----------|------|
| `hw_type` | `1` = Ethernet |
| `proto_type` | `0x0800` = IPv4 |
| `hw_len` / `proto_len` | `6` / `4` |
| `opcode` | `1`=Request, `2`=Reply |
| `sender_mac[6]` + `sender_ip` | 送信元の MAC・IP |
| `target_mac[6]` + `target_ip` | 対象の MAC・IP |

#### `struct ip_hdr` — IPv4 ヘッダ (20 バイト, `include/ip.h`)

```
 0      1      2      3      4      8     12     16     20
 ┌──────┬──────┬──────────────┬──────────────┬──────┬──────┐
 │v/ihl │dscp  │  total_len   │      id      │fl/fr │ ttl  │
 ├──────┴──────┴──────────────┴──────────────┴──────┴──────┤
 │ proto│  checksum   │      src_ip         │    dst_ip    │
 └──────┴─────────────┴─────────────────────┴──────────────┘
```

| フィールド | 説明 |
|----------|------|
| `version_ihl` | 上位 4bit=バージョン(4), 下位 4bit=IHL (最小 `5`=20B) |
| `total_len` | IP ヘッダ + ペイロード長 |
| `id` | 送信ごとにインクリメント |
| `ttl` | 送信時固定値 `64` |
| `protocol` | `1`=ICMP, `6`=TCP |
| `checksum` | IP ヘッダのみを対象とするチェックサム |

#### `struct icmp_hdr` — ICMP ヘッダ (8 バイト, `include/icmp.h`)

| フィールド | 説明 |
|----------|------|
| `type` | `8`=Echo Request, `0`=Echo Reply |
| `code` | Echo の場合 `0` |
| `checksum` | ヘッダ + データ全体を対象 |
| `id` / `seq` | Echo の識別子・シーケンス番号 |

#### `struct tcp_hdr` — TCP ヘッダ (20 バイト, `include/tcp.h`)

| フィールド | 説明 |
|----------|------|
| `src_port` / `dst_port` | ポート番号 |
| `seq_num` / `ack_num` | シーケンス・確認応答番号 |
| `data_off` | 上位 4bit = ヘッダ長 (32bit 語単位, 最小 `0x50`=20B) |
| `flags` | `FIN`=0x01, `SYN`=0x02, `RST`=0x04, `PSH`=0x08, `ACK`=0x10 |
| `window` | 受信ウィンドウサイズ (`TCP_WINDOW_SIZE` = 4096) |
| `checksum` | 擬似ヘッダ + セグメント全体を対象 |

#### `struct ip_pseudo_hdr` — TCP チェックサム用擬似ヘッダ (12 バイト, `include/ip.h`)

TCP チェックサム計算にのみ使用し、実際には送受信しません。

| フィールド | 説明 |
|----------|------|
| `src_ip` / `dst_ip` | 送受信 IP アドレス |
| `zero` | 常に `0` |
| `protocol` | `6` (TCP) |
| `tcp_len` | TCP セグメント長 |

#### `struct tcp_conn` — TCP 接続制御ブロック (`include/tcp.h`)

```c
struct tcp_conn {
    tcp_state_t state;          // RFC 793 状態 (CLOSED=0 が空きスロット)
    uint32_t local_ip;          // 自側 IP
    uint16_t local_port;        // 自側ポート (ホストバイトオーダー)
    uint32_t remote_ip;         // 相手側 IP
    uint16_t remote_port;       // 相手側ポート
    uint32_t snd_una;           // 未確認の最古シーケンス番号
    uint32_t snd_nxt;           // 次に送信するシーケンス番号
    uint16_t snd_wnd;           // 相手の受信ウィンドウ
    uint32_t rcv_nxt;           // 次に期待する受信シーケンス番号
    uint16_t rcv_wnd;           // 自側の受信ウィンドウ (常に 4096)
    uint8_t  recv_buf[8192];    // 受信バッファ
    size_t   recv_len;          // バッファ内データ長
};
```

接続テーブル: `static struct tcp_conn tcp_conns[16]` (最大 16 エントリ)

---

## 関連ドキュメント

- [パケットフロー](data-flow.md) — 各モジュールが呼ばれる順序と処理内容
- [TCP 状態機械](tcp-state-machine.md) — `state` フィールドの遷移詳細
- [チェックサム](checksum.md) — `checksum` フィールドの計算方法
- [テスト](testing.md) — ユニットテスト・ファズテストの構成
