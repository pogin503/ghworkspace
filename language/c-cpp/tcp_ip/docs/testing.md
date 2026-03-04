# テスト

→ [index.md](index.md) | [TCP 状態機械](tcp-state-machine.md)

---

## ユニットテスト (Unity / gcc)

### 実行方法

```bash
make test
# または
make -C test
```

コンパイルには `-DFUZZING` フラグが必要です (TCP 接続テーブルのリセット関数を有効化するため)。

### テストケース一覧

#### `test/test_utils.c` — チェックサム関数 (5 ケース)

| テスト名 | 検証内容 |
|---------|---------|
| `test_checksum_all_zeros` | `checksum({0x00,0x00,0x00,0x00}, 4)` = `0xffff` |
| `test_checksum_known_value` | `checksum({0x00,0x01,0x00,0x02}, 4)` = `0xfffc` |
| `test_checksum_partial_chain` | 2 バッファに分割した計算結果 = 一括計算の結果 |
| `test_checksum_odd_length` | 奇数バイト `{0x00,0x01,0x02}` → `0xfdfe` (上位バイトパディング) |
| `test_checksum_verify` | データ + チェックサムフィールドを合わせた計算結果 = `0x0000` |

#### `test/test_arp.c` — ARP キャッシュ (4 ケース)

テスト間の干渉を避けるため、各テストで異なる IP アドレスを使用しています
(ARP キャッシュには外部リセット関数がないため)。

| テスト名 | 検証内容 |
|---------|---------|
| `test_arp_cache_lookup_unknown` | 未登録 IP → `arp_cache_lookup()` が `NULL` を返す |
| `test_arp_cache_update_and_lookup` | 登録後に検索して MAC が一致する |
| `test_arp_cache_update_overwrite` | 同じ IP で MAC を上書きできる |
| `test_arp_cache_multiple_entries` | 異なる IP の複数エントリが共存できる |

#### `test/test_tcp.c` — TCP 状態機械 (3 ケース)

`setUp()` で `tcp_reset_for_fuzz(10.0.0.2, 8080)` を呼び出し、毎テストでクリーンな状態から開始。
`build_tcp_segment()` ヘルパーで正しいチェックサム付きの TCP セグメントを組み立てます。

| テスト名 | 検証内容 |
|---------|---------|
| `test_tcp_listen_socket_exists` | リセット後に `tcp_listen_lookup(8080)` が `TCP_STATE_LISTEN` を返す |
| `test_tcp_syn_to_syn_received` | SYN 送信後、コネクションが `TCP_STATE_SYN_RECEIVED` に遷移する |
| `test_tcp_three_way_handshake` | SYN → ACK (`ack = conn->snd_nxt`) で `TCP_STATE_ESTABLISHED` に遷移する |

### ファイル構成

```
test/
├── Makefile          # gcc ビルド (-DFUZZING)
├── test_utils.c      # utils.c のテスト
├── test_arp.c        # arp.c のテスト
└── test_tcp.c        # tcp.c のテスト

# 依存関係
Unity フレームワーク : src/unity.c, src/unity.h, include/unity_internals.h
TAP モック          : fuzz/tun_mock.c  (tun_write = no-op)
```

---

## ファズテスト (libFuzzer / clang + ASAN + UBSAN)

### 概要

libFuzzer が生成した入力バイト列を直接プロトコルパーサーに渡し、
クラッシュ・メモリ不正アクセス・未定義動作を検出します。

実際の TAP デバイスは使用せず、`fuzz/tun_mock.c` で `tun_write` を no-op に置き換えています。

### ビルドと実行

```bash
make fuzz              # clang でファズターゲットをビルド
make -C fuzz seeds     # シードコーパスを生成
make -C fuzz run-tcp   # TCP ファズを 60 秒実行
FUZZ_RUNS="-max_total_time=300" make -C fuzz run-tcp  # 5 分実行
```

### ファズターゲット一覧

| ターゲット | 入力 | チェックサム補正 | 主な探索範囲 |
|-----------|------|-----------------|------------|
| `fuzz/fuzz_eth.c` | Ethernet フレーム (14+ バイト) | なし | EtherType ディスパッチ |
| `fuzz/fuzz_arp.c` | ARP ペイロード (28+ バイト) | なし | opcode 分岐・キャッシュ |
| `fuzz/fuzz_ip.c` | IPv4 パケット (20+ バイト) | IP ヘッダ再計算 | protocol ディスパッチ |
| `fuzz/fuzz_icmp.c` | ICMP ペイロード (8+ バイト) | ICMP 再計算 | type/code 分岐 |
| `fuzz/fuzz_tcp.c` | TCP セグメント (20+ バイト) | TCP 再計算 + dst_port=8080 固定 | 状態遷移全体 |

### チェックサム補正の理由

プロトコルパーサーはチェックサム不正なパケットを入口で弾きます。
ランダム入力をそのまま渡すとほぼ全てが弾かれ、深い分岐を探索できません。
各ファズターゲットではチェックサムを毎回再計算してから渡します。

```c
// fuzz_tcp.c の例
hdr->dst_port = htons(8080);      // LISTEN ポートに強制 (先にポートを設定)
hdr->checksum = 0;
uint32_t csum = checksum_partial(&pseudo, sizeof(pseudo), 0);
csum          = checksum_partial(buf, size, csum);
hdr->checksum = htons(checksum_finish(csum));  // htons() 必須 (checksum.md 参照)
```

### TCP のイテレーション間リセット

TCP 接続テーブルはプロセス内で状態を持ち続けます。
毎イテレーションで `tcp_reset_for_fuzz()` を呼び出すことで、
常に `LISTEN → SYN_RECEIVED → ESTABLISHED` の遷移から探索できます。

```c
// fuzz_tcp.c
int LLVMFuzzerTestOneInput(...) {
    tcp_reset_for_fuzz(g_dst_ip, 8080);  // 毎回リセット
    // ...
    tcp_recv(&g_dev, g_src_ip, g_dst_ip, buf, size);
    return 0;
}
```

### シードコーパス (`fuzz/generate_seeds.c`)

代表的なパケット形式を事前に生成し、ファザーの初期探索を誘導します。

| ディレクトリ | ファイル | 内容 |
|------------|---------|------|
| `corpus/eth/` | `arp_request.bin` (42B) | Ethernet + ARP Request |
| `corpus/eth/` | `icmp_echo_request.bin` (56B) | Ethernet + IP + ICMP Echo |
| `corpus/arp/` | `arp_request.bin`, `arp_reply.bin` | ARP Request/Reply |
| `corpus/ip/` | `icmp_echo.bin`, `tcp_syn.bin` | IP + ICMP, IP + TCP SYN |
| `corpus/icmp/` | `echo_request_min.bin` (8B) | 最小 ICMP Echo |
| `corpus/tcp/` | `syn.bin`, `ack.bin`, `psh_ack.bin`, `fin_ack.bin`, `rst.bin` | 各フラグのセグメント |

### クラッシュが見つかったら

```bash
# クラッシュを再現する
./fuzz/fuzz_tcp crash-<hash>

# AddressSanitizer のスタックトレースが表示される
# ==ERROR: AddressSanitizer: heap-buffer-overflow on address ...
```

### サニタイザ設定

```makefile
# fuzz/Makefile
FUZZ_FLAGS := -fsanitize=fuzzer,address,undefined -g -O1
# AddressSanitizer  : OOB アクセス、use-after-free 等を検出
# UndefinedBehaviorSanitizer: 整数オーバーフロー、NULL 参照等を検出
```
