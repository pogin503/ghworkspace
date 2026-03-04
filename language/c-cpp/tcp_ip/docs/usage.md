# 使用方法

ユーザー空間で動作するTCP/IPプロトコルスタックの実装です。
LinuxのTAPデバイスを経由してEthernetフレームを直接読み書きし、ARP・IP・ICMP・TCPを自前で処理します。

## 前提条件

| 要件 | 説明 |
|------|------|
| OS | Linux (WSL2を含む) |
| gcc | 通常のビルドに使用 |
| clang 11以降 | fuzz testingに使用 |
| root または CAP_NET_ADMIN | TAPデバイスのオープンに必要 |

```bash
# clang が未インストールの場合
sudo apt install clang
```

## ビルド

```bash
cd language/c-cpp/tcp_ip

make          # bin/tcp_stack をビルド
make debug    # debug/tcp_stack をビルド (デバッグシンボル付き)
make clean    # ビルド成果物を削除
```

## 実行

### 1. ホスト側のTAPデバイスを設定する

スタックを起動する前に、`scripts/tap.sh` でTAPインターフェースを作成します。

```bash
sudo scripts/tap.sh up        # tap0 を作成して 10.0.0.1/24 を設定
scripts/tap.sh status         # 現在の状態を確認
sudo scripts/tap.sh down      # 後片付け (tap0 を削除)
```

別のデバイス名を使う場合はデバイス名を第2引数に渡します。

```bash
sudo scripts/tap.sh up tap1
```

手動で設定する場合は以下と同等です。

```bash
sudo ip tuntap add dev tap0 mode tap
sudo ip addr add 10.0.0.1/24 dev tap0
sudo ip link set tap0 up
```

### 2. スタックを起動する

```bash
sudo bin/tcp_stack        # デフォルト: tap0 を使用
sudo bin/tcp_stack tap0   # デバイス名を明示指定
```

起動すると以下のような出力が表示されます:

```
[INFO] TAP device 'tap0' opened (fd=3)
[INFO] TCP LISTEN on port 8080
[INFO] === Userspace TCP/IP Stack ===
[INFO] Interface : tap0
[INFO] MAC       : 00:0c:29:00:00:01
[INFO] IP        : 10.0.0.2
[INFO] TCP LISTEN: port 8080
[INFO] Event loop started (Ctrl+C to stop)
```

スタックは IP `10.0.0.2` として動作します。ホスト側は `10.0.0.1` です。

### 3. 終了する

`Ctrl+C` を押すとシャットダウンします。

```bash
# TAPデバイスを削除する場合
sudo ip tuntap del dev tap0 mode tap
```

## 動作確認

スタックを起動した状態で、別のターミナルから以下を実行します。

### ARP の確認

ホストが `10.0.0.2` のMACアドレスを解決できるか確認します。

```bash
ping -I tap0 10.0.0.2 -c 1   # pingを1回送る (ARPが発生する)
ip neigh show dev tap0
# 10.0.0.2 lladdr 00:0c:29:00:00:01 REACHABLE
```

スタック側のログ:

```
[INFO] ARP REQUEST: who has 10.0.0.2? tell 10.0.0.1
[INFO] ARP REPLY: 00:0c:29:00:00:01 -> 10.0.0.1
```

### ICMP (ping) の確認

```bash
ping -I tap0 10.0.0.2 -c 5
```

期待される出力:

```
PING 10.0.0.2 (10.0.0.2) from 10.0.0.1 tap0: 56(84) bytes of data.
64 bytes from 10.0.0.2: icmp_seq=1 ttl=64 time=...
64 bytes from 10.0.0.2: icmp_seq=2 ttl=64 time=...
```

スタック側のログ:

```
[INFO] ICMP ECHO REQUEST id=1234 seq=1 from 10.0.0.1
[INFO] ICMP ECHO REPLY sent id=1234 seq=1
```

### TCP 接続の確認

スタックはポート `8080` でTCPリクエストを受け付けます。

```bash
# 1回限りのデータ送信
echo "Hello TCP Stack" | nc 10.0.0.2 8080

# 対話的な接続
nc 10.0.0.2 8080

# タイムアウトを設定する場合
nc -w 3 10.0.0.2 8080
```

スタック側のログ (3ウェイハンドシェイク):

```
[INFO] TCP SYN from 10.0.0.1:56789 -> port 8080 [LISTEN -> SYN_RECEIVED]
[INFO] TCP ESTABLISHED (local=8080 remote=56789)
[INFO] TCP DATA 16 bytes received (local=8080)
[INFO] TCP FIN received (local=8080) [ESTABLISHED -> LAST_ACK]
[INFO] TCP LAST_ACK -> CLOSED (local=8080)
```

### パケットキャプチャ

tcpdump でスタックとのやり取りを観察できます。

```bash
# 全パケットを表示
sudo tcpdump -i tap0 -nn -v

# TCPのみ
sudo tcpdump -i tap0 -nn tcp port 8080

# ARPのみ
sudo tcpdump -i tap0 -nn arp
```

## Fuzz Testing

### ビルド

fuzz testingにはclangが必要です。

```bash
make fuzz
# または
cd fuzz && make
```

生成されるターゲット:

| バイナリ | 対象 |
|---------|------|
| `fuzz/fuzz_eth` | `eth_recv()` - Ethernetフレーム全体 |
| `fuzz/fuzz_arp` | `arp_recv()` - ARPペイロード |
| `fuzz/fuzz_ip` | `ip_recv()` - IPパケット |
| `fuzz/fuzz_icmp` | `icmp_recv()` - ICMPペイロード |
| `fuzz/fuzz_tcp` | `tcp_recv()` - TCPセグメント (最重要) |

### シードコーパスの生成

```bash
cd fuzz
make seeds
# corpus/{eth,arp,ip,icmp,tcp}/ に有効なパケットのシードが生成される
```

### 実行

```bash
cd fuzz

# 個別実行 (デフォルト60秒)
make run-tcp      # TCP状態機械のfuzz
make run-ip       # IPヘッダ解析のfuzz
make run-eth      # Ethernetフレーム全体のfuzz
make run-arp      # ARP処理のfuzz
make run-icmp     # ICMP処理のfuzz

# 実行時間をカスタマイズ
FUZZ_RUNS="-max_total_time=300" make run-tcp   # 5分間実行

# 最大入力サイズをカスタマイズ
FUZZ_MAX_LEN="-max_len=100" make run-tcp

# 手動実行 (オプションを細かく制御)
./fuzz_tcp corpus/tcp/ -max_total_time=3600 -max_len=1500

# 無制限に実行する場合
./fuzz_tcp corpus/tcp/ -max_len=1500
```

### 出力の見方

```
INFO: Loaded 5 modules   (138 guards): ...
INFO: Seed: 2134567890
INFO: Loaded 5 seed files from corpus/tcp/

#0     READ   units: 5 exec/s: 0 rss: 63Mb
#5     pulse  cov: 52 ft: 53 corp: 3/60b ...
#364779 DONE   cov: 138 ft: 181 corp: 17/1938b exec/s: 33161 rss: 129Mb
```

- `cov`: カバレッジ (エッジ数)
- `corp`: コーパスのサイズ (件数/バイト数)
- `exec/s`: 毎秒の実行回数
- `DONE`: 指定時間で正常終了

### クラッシュが見つかったら

クラッシュが発生すると `crash-<hash>` というファイルが生成されます。

```bash
# クラッシュを再現する
./fuzz_tcp crash-<hash>

# AddressSanitizerのスタックトレースが表示される
# ==ERROR: AddressSanitizer: heap-buffer-overflow on address ...
#     #0 tcp_recv (fuzz_tcp+0x...)
#     #1 LLVMFuzzerTestOneInput (fuzz_tcp+0x...)
```

### クリーンアップ

```bash
cd fuzz && make clean
```

## ネットワーク設定の固定値

| 項目 | 値 |
|------|-----|
| スタックのIP | `10.0.0.2` |
| スタックのMAC | `00:0c:29:00:00:01` |
| ネットマスク | `255.255.255.0` |
| TCPリスニングポート | `8080` |
| ホスト側のIP (推奨) | `10.0.0.1` |

これらの値は `src/main.c` の先頭で変更できます。
