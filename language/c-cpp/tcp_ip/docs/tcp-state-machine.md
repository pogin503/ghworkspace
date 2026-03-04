# TCP 状態機械

→ [index.md](index.md) | [パケットフロー](data-flow.md)

---

## 状態遷移図

RFC 793 の状態機械を実装しています。
**パッシブオープン (サーバ側)** のみサポートし、アクティブオープン (クライアント側) は未実装です。

```
                     CLOSED
                       │
               tcp_listen() を呼び出す
                       │
                       ▼
             ┌──────LISTEN──────────────────────────────┐
             │       │                                  │
             │    SYN 受信                              │ (他の接続からの
             │  新コネクション alloc                    │  SYN も受け付ける)
             │  rcv_nxt = seq + 1                       │
             │  snd_nxt = rand() (ISN)                  │
             │  SYN+ACK 送信                            │
             │       │                                  │
             │       ▼                                  │
             │   SYN_RECEIVED                           │
             │       │                                  │
             │    ACK 受信                              │
             │  かつ ack == snd_nxt                     │
             │       │                                  │
             │       ▼                                  │
             │   ESTABLISHED ◄──── データ受信 (ACK 返送)│
             │       │                                  │
             │  ┌────┤                                  │
             │  │    │ RST 受信                         │
             │  │    └──────────────→ CLOSED            │
             │  │                                       │
             │  │ FIN 受信                              │
             │  │ rcv_nxt = seq + 1                     │
             │  │ ACK 送信                              │
             │  │                                       │
             │  ▼                                       │
             │ CLOSE_WAIT ─── (即座に遷移)              │
             │  │                                       │
             │  │ FIN+ACK 送信                          │
             │  ▼                                       │
             │ LAST_ACK                                 │
             │  │                                       │
             │  │ ACK 受信                              │
             │  ▼                                       │
             └─CLOSED ──────────────────────────────────┘
```

アクティブクローズ (こちらから FIN を送る場合) の遷移:

```
ESTABLISHED
  │ tcp_send_segment(FIN) を呼び出す (未実装: 外部からトリガーなし)
  ▼
FIN_WAIT_1
  │ ACK 受信
  ▼
FIN_WAIT_2
  │ FIN 受信 → ACK 送信
  ▼
CLOSED  ← TIME_WAIT を省略して即 CLOSED
```

---

## 状態ごとの動作詳細

| 状態 | 遷移トリガー | 処理内容 |
|------|------------|---------|
| `LISTEN` | SYN 受信 | 新コネクション alloc、SYN+ACK 送信、`SYN_RECEIVED` へ |
| `SYN_RECEIVED` | ACK 受信 (ack == snd_nxt) | `snd_una` 更新、`ESTABLISHED` へ |
| `ESTABLISHED` | データ受信 | `rcv_nxt` 更新、受信バッファに格納、ACK 返送 |
| `ESTABLISHED` | FIN 受信 | `rcv_nxt` 更新、ACK 送信、`CLOSE_WAIT` → `LAST_ACK`、FIN+ACK 送信 |
| `ESTABLISHED` | RST 受信 | コネクション解放 (`CLOSED`) |
| `ESTABLISHED` | 純粋 ACK 受信 | `snd_una` を更新のみ |
| `LAST_ACK` | ACK 受信 | コネクション解放 (`CLOSED`) |
| `FIN_WAIT_1` | ACK 受信 | `FIN_WAIT_2` へ |
| `FIN_WAIT_2` | FIN 受信 | ACK 送信、`CLOSED` へ (TIME_WAIT 省略) |

---

## 接続テーブル仕様

```c
// src/tcp.c (内部実装)
static struct tcp_conn tcp_conns[TCP_MAX_CONNS];  // TCP_MAX_CONNS = 16
```

| 仕様 | 値 |
|------|---|
| 最大同時接続数 | 16 (`TCP_MAX_CONNS`) |
| 空きスロット判定 | `state == TCP_STATE_CLOSED` |
| 受信バッファ | 8192 バイト/コネクション (`TCP_RECV_BUF_SIZE`) |
| 受信ウィンドウ | 4096 バイト (`TCP_WINDOW_SIZE`) |
| 初期送信シーケンス番号 (ISN) | `rand()` でランダム生成 |

### コネクション検索の優先順位 (`tcp_recv` 内)

1. **4タプル一致検索** (`tcp_conn_lookup`): `src_ip`, `src_port`, `dst_ip`, `dst_port` が全て一致
2. **LISTEN ソケット検索** (`tcp_listen_lookup`): `dst_port` が一致する LISTEN 状態のスロット

### LISTEN ソケットの扱い

LISTEN ソケット自体は状態を変えません。SYN を受信すると **新しいスロットを alloc** して
新コネクションとして管理します。これにより LISTEN ソケットは常に新しい接続を受け付けられます。

---

## シーケンス番号の管理

### 送信側

```
snd_una  ─── 未確認の最古シーケンス番号
snd_nxt  ─── 次に送信するシーケンス番号

SYN 送信時  : snd_nxt += 1  (SYN は 1 バイト消費)
データ送信時: snd_nxt += data_len
FIN 送信時  : snd_nxt += 1  (FIN は 1 バイト消費)
ACK 受信時  : snd_una = ack (確認済みシーケンス番号を更新)
```

### 受信側

```
rcv_nxt  ─── 次に期待するシーケンス番号

SYN 受信時  : rcv_nxt = seq + 1
データ受信時: rcv_nxt = seq + data_len
FIN 受信時  : rcv_nxt = seq + 1
```

---

## 既知の省略事項

| 機能 | 現状 |
|------|------|
| TIME_WAIT 状態 | FIN 受信後に即 CLOSED へ遷移 |
| TCP オプション | 全て無視 (ウィンドウスケーリング、SACK、MSS など) |
| 再送タイマー | 実装なし (パケットロスは考慮しない) |
| 輻輳制御 | 実装なし |
| アクティブクローズ | FIN_WAIT_1/2 の状態遷移コードはあるが、外部からのトリガーなし |
| シーケンス番号の検証 | 受信 seq が `rcv_nxt` と一致するか確認していない |
| マルチスレッド | 単一スレッドのみ (接続テーブルに排他制御なし) |
