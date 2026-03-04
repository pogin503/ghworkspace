# 用語集

→ [index.md](index.md)

このプロジェクトで使用する略語・専門用語をまとめます。

---

## プロトコル略語

| 用語 | 正式名称 | 関連 RFC | 概要 |
|------|---------|---------|------|
| **ARP** | Address Resolution Protocol | RFC 826 | IP アドレスから MAC アドレスを解決するプロトコル。`src/arp.c` |
| **ICMP** | Internet Control Message Protocol | RFC 792 | ネットワーク診断・エラー通知。ping (Echo Request/Reply) に使用。`src/icmp.c` |
| **IP / IPv4** | Internet Protocol version 4 | RFC 791 | パケット転送・アドレッシング。32bit アドレス空間。`src/ip.c` |
| **TCP** | Transmission Control Protocol | RFC 793 | 信頼性のある双方向ストリーム通信。コネクション管理・フロー制御。`src/tcp.c` |

---

## TCP 固有の用語

### 3WHS (3-Way Handshake)
TCP コネクション確立の手順。

```
クライアント          サーバ
     │──── SYN ────►│  (seq=ISN_c)
     │◄── SYN+ACK ──│  (seq=ISN_s, ack=ISN_c+1)
     │──── ACK ────►│  (ack=ISN_s+1)
     │  ESTABLISHED  │
```

### ISN (Initial Sequence Number)
コネクション開始時のシーケンス番号。
本実装では `rand()` でランダム生成します (`tcp.c: new_conn->snd_nxt = (uint32_t)rand()`)。
ISN をランダムにすることで、旧コネクションのパケットとの混同を防ぎます。

### TCB (Transmission Control Block)
コネクションごとの状態を保持する構造体。本実装では `struct tcp_conn` に対応します。
最大 16 エントリの静的配列 (`TCP_MAX_CONNS = 16`) で管理します。

### シーケンス番号変数

RFC 793 では送受信の状態を管理するために以下の変数を定義しています。
変数名は全て RFC 793 の原文の略称をそのまま使用しています。

| 変数 | 略称の展開 | 意味 |
|------|----------|------|
| `snd_una` | **SND.UNA** — Send **UN**acknowledged | 送信済みだがまだ ACK を受け取っていない最古のシーケンス番号 |
| `snd_nxt` | **SND.NXT** — Send **N**ex**t** | 次に送信するシーケンス番号 |
| `snd_wnd` | **SND.WND** — Send **W**i**nd**ow | 相手から通知された受信ウィンドウサイズ |
| `rcv_nxt` | **RCV.NXT** — Receive **N**ex**t** | 次に受信を期待するシーケンス番号 |
| `rcv_wnd` | **RCV.WND** — Receive **W**i**nd**ow | 自側の受信ウィンドウサイズ (相手に広告する値) |

```
送信バッファのイメージ:

     snd_una        snd_nxt
        │               │
        ▼               ▼
────────┬───────────────┬──────────────────►
        │  送信済み・   │  未送信データ
        │  ACK 待ち     │
        └───────────────┘
         ↑ この範囲が "unacknowledged"
```

ACK を受信するたびに `snd_una` が前進します。
`snd_una == snd_nxt` になれば送信したデータが全て確認済みの状態です。

### TCP フラグ

| フラグ | 値 | 意味 |
|-------|-----|------|
| `FIN` | 0x01 | コネクション終了要求 (1 バイト消費) |
| `SYN` | 0x02 | コネクション確立要求 (1 バイト消費) |
| `RST` | 0x04 | コネクション強制リセット |
| `PSH` | 0x08 | データを即座にアプリケーションに渡すよう要求 |
| `ACK` | 0x10 | ack_num フィールドが有効 |
| `URG` | 0x20 | 緊急ポインタが有効 (本実装では未使用) |

### TCP 状態

| 状態 | 意味 |
|------|------|
| `CLOSED` | コネクションなし (空きスロット) |
| `LISTEN` | SYN 待ち受け中 |
| `SYN_RECEIVED` | SYN を受信し SYN+ACK を送信済み |
| `ESTABLISHED` | コネクション確立済み (データ送受信可能) |
| `CLOSE_WAIT` | 相手から FIN を受信、ACK を送信済み |
| `LAST_ACK` | FIN+ACK を送信済み、最後の ACK 待ち |
| `FIN_WAIT_1` | こちらから FIN を送信済み (アクティブクローズ) |
| `FIN_WAIT_2` | FIN への ACK を受信済み、相手の FIN 待ち |
| `TIME_WAIT` | 遅延パケット対処のための待機 (本実装では省略) |

### 擬似ヘッダ (Pseudo Header)
TCP チェックサム計算に使用する仮想的な 12 バイトのヘッダ (RFC 793)。
実際には送受信しない。IP アドレスを含めることで IP 層の誤配送を検出します。

```
┌──────────────┬──────────────┬──────┬──────┬──────────┐
│   src_ip     │   dst_ip     │  0   │proto │ tcp_len  │
│   (4B)       │   (4B)       │ (1B) │ (1B) │  (2B)    │
└──────────────┴──────────────┴──────┴──────┴──────────┘
```

### 受信ウィンドウ (Window)
フロー制御のためにバッファの空き容量を相手に通知する値。
本実装では常に `TCP_WINDOW_SIZE = 4096` バイトを広告します。

---

## ネットワーク基礎用語

### Ethernet II
最も広く使われる Ethernet フレーム形式。
`dst_mac (6B) + src_mac (6B) + EtherType (2B) + payload` の構造を持ちます。

### EtherType
Ethernet フレームのペイロードが何のプロトコルかを示す 2 バイトフィールド。

| 値 | プロトコル |
|----|---------|
| `0x0800` | IPv4 |
| `0x0806` | ARP |

### ネットワークバイトオーダー (Network Byte Order)
big-endian (上位バイトが先)。RFC 1700 で定義。
C の標準関数でホストバイトオーダーと変換します。

| 関数 | 変換方向 | サイズ |
|------|---------|------|
| `htons()` | ホスト → ネットワーク | 16bit |
| `htonl()` | ホスト → ネットワーク | 32bit |
| `ntohs()` | ネットワーク → ホスト | 16bit |
| `ntohl()` | ネットワーク → ホスト | 32bit |

x86 はリトルエンディアンのため、ネットワークへ送出するフィールドは必ず変換が必要です。
(→ [チェックサム](checksum.md) で `htons()` 漏れによるバグの詳細を解説)

### 4 タプル
TCP コネクションを一意に識別する 4 つの値の組。

```
(src_ip, src_port, dst_ip, dst_port)
```

### チェックサム (RFC 1071)
16bit 語の 1 の補数和によるエラー検出。IP・ICMP・TCP で使用。
詳細は [チェックサム](checksum.md) を参照。

### TTL (Time To Live)
IP パケットが通過できるルータの最大数。
転送のたびに 1 減算され、0 になると廃棄されます。
本実装では送信時に `64` を固定で設定します。

### IHL (Internet Header Length)
IPv4 ヘッダ長を 32bit 語単位で示す 4bit フィールド (`version_ihl` の下位 4bit)。
オプションなしの最小値は `5` (= 20 バイト)。

### MTU (Maximum Transmission Unit)
1 フレームで送れるペイロードの最大サイズ。
Ethernet の標準 MTU は **1500 バイト**。
本実装ではフラグメントに非対応のため、パケットが MTU を超えないことを前提とします。

---

## Linux / ツール用語

### TUN デバイス
Layer 3 (IP パケット) を扱う仮想 NIC。
`/dev/net/tun` を `IFF_TUN` フラグで開くと、`read()`/`write()` で IP パケットを直接読み書きできます。
VPN (WireGuard, OpenVPN 等) で広く使われます。

### TAP デバイス
Layer 2 (Ethernet フレーム) を扱う仮想 NIC。
`/dev/net/tun` を `IFF_TAP` フラグで開くと、`read()`/`write()` で Ethernet フレームを読み書きできます。
**本プロジェクトでは TAP を使用します** (`src/tun.c`)。

```
TUN: IP パケットのみ   ← VPN 向き
TAP: Ethernet フレーム ← このプロジェクト (ARP なども処理できる)
```

### IFF_NO_PI
TAP/TUN を開く際のフラグ。
`struct tun_pi` (4 バイトのプレフィックス) を付けない設定。
これにより `read()` の結果がそのまま Ethernet フレームになります。

### TUNSETIFF
TAP/TUN デバイスを設定するための `ioctl` コマンド。
インタフェース名・フラグ (`IFF_TAP | IFF_NO_PI`) を指定して実際のデバイスに紐付けます。

### CAP_NET_ADMIN
TAP デバイスの作成・設定に必要な Linux ケーパビリティ。
`sudo` で実行するか、このケーパビリティをプロセスに付与する必要があります。

### libFuzzer
LLVM に内蔵されたカバレッジガイド付きファザー。
`-fsanitize=fuzzer` でリンクし、`LLVMFuzzerTestOneInput()` を実装するだけで使用できます。
本プロジェクトでは `fuzz/` ディレクトリの各ターゲットで使用します。

### ASAN (AddressSanitizer)
メモリ安全性を実行時に検査するサニタイザ (`-fsanitize=address`)。
バッファオーバーフロー・use-after-free・ヒープ破壊などを検出します。

### UBSAN (UndefinedBehaviorSanitizer)
C/C++ の未定義動作を実行時に検出するサニタイザ (`-fsanitize=undefined`)。
整数オーバーフロー・null 参照・不正シフトなどを検出します。

### Unity
C 言語向けの軽量ユニットテストフレームワーク (ThrowTheSwitch/Unity)。
単一ヘッダ (`unity.h`) で使え、`TEST_ASSERT_*` マクロでアサーションを記述します。
本プロジェクトでは `src/unity.c` に配置し、`test/` のテストで使用します。

### コーパス (Corpus)
ファザーが探索の出発点として使う入力ファイルの集合。
本プロジェクトでは `fuzz/generate_seeds.c` で生成し `fuzz/corpus/` に保存します。
代表的なパケット形式をシードにすることで、ファザーが深い分岐を早期に探索できます。

---

## 参照 RFC 一覧

| RFC | タイトル | 関連モジュール |
|-----|---------|-------------|
| RFC 791 | Internet Protocol (IPv4) | `src/ip.c` |
| RFC 792 | Internet Control Message Protocol (ICMP) | `src/icmp.c` |
| RFC 793 | Transmission Control Protocol (TCP) | `src/tcp.c` |
| RFC 826 | An Ethernet Address Resolution Protocol (ARP) | `src/arp.c` |
| RFC 1071 | Computing the Internet Checksum | `src/utils.c` |
| RFC 9293 | Transmission Control Protocol (TCP) — RFC 793 の改訂版 | 参考 |
