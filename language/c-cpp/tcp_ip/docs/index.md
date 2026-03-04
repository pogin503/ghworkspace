# TCP/IP スタック — ドキュメント

Linux の TAP デバイス (`/dev/net/tun`) を使ってユーザ空間で動作する、
学習目的のミニマルな TCP/IP スタック実装です。

---

## ドキュメント一覧

| ドキュメント | 内容 |
|------------|------|
| [使用方法](usage.md) | セットアップ・実行手順・動作確認コマンド |
| [アーキテクチャ](architecture.md) | レイヤー構造・モジュール一覧・主要データ構造 |
| [パケットフロー](data-flow.md) | 受信・送信の処理フロー (ASCII 図) |
| [TCP 状態機械](tcp-state-machine.md) | RFC 793 状態遷移・接続テーブル仕様 |
| [チェックサム](checksum.md) | RFC 1071 実装と `htons()` が必要な理由 |
| [テスト](testing.md) | ユニットテスト (Unity) + ファズテスト (libFuzzer) 構成 |
| [用語集](terminology.md) | TAP・TUN・RFC・TCP 用語などの解説 |
| [RFC 791](rfc/rfc791.txt) | Internet Protocol (IPv4) |
| [RFC 792](rfc/rfc792.txt) | Internet Control Message Protocol (ICMP) |
| [RFC 793](rfc/rfc793.txt) | Transmission Control Protocol (TCP) |
| [RFC 826](rfc/rfc826.txt) | An Ethernet Address Resolution Protocol (ARP) |
| [RFC 1071](rfc/rfc1071.txt) | Computing the Internet Checksum |

---

## ディレクトリ構成

```
tcp_ip/
├── src/            アプリケーションソース
│   ├── main.c          イベントループ (tap0 読み書き)
│   ├── ethernet.c      Ethernet II フレーム解析・組立
│   ├── arp.c           ARP リクエスト/リプライ・キャッシュ (32 エントリ)
│   ├── ip.c            IPv4 ヘッダ解析・組立・チェックサム
│   ├── icmp.c          ICMP Echo Request → Reply
│   ├── tcp.c           RFC 793 TCP 状態機械 (最大 16 接続)
│   ├── tun.c           TAP デバイス I/O
│   ├── utils.c         RFC 1071 チェックサム・ログ
│   ├── unity.c         Unity テストフレームワーク (テスト専用)
│   └── unity.h         Unity ヘッダ (テスト専用)
├── include/        ヘッダファイル
│   ├── ethernet.h, arp.h, ip.h, icmp.h, tcp.h, tun.h, utils.h
│   └── unity_internals.h
├── fuzz/           libFuzzer ターゲット
│   ├── fuzz_eth.c, fuzz_arp.c, fuzz_ip.c, fuzz_icmp.c, fuzz_tcp.c
│   ├── tun_mock.c      TAP デバイスのモック (fuzzing 用)
│   └── generate_seeds.c  シードコーパス生成
├── test/           ユニットテスト
│   ├── test_utils.c, test_arp.c, test_tcp.c
│   └── Makefile
├── docs/           このドキュメント群
└── Makefile
```

---

## ビルド・実行コマンド早見表

```bash
# メインバイナリのビルド (gcc)
make

# ユニットテスト実行 (gcc + Unity)
make test

# ファズターゲットのビルド (clang + libFuzzer)
make fuzz

# ファズ実行 (各60秒, シードコーパス使用)
make -C fuzz run-tcp    # TCP 状態機械のファズ
make -C fuzz run-icmp   # ICMP ハンドラのファズ
make -C fuzz run-ip     # IP 層のファズ
make -C fuzz run-arp    # ARP ハンドラのファズ
make -C fuzz run-eth    # Ethernet 層のファズ

# クリーン
make clean
make distclean
```

---

## 動作環境

- Linux (WSL2 含む)
- gcc (メイン・テスト), clang (ファズ)
- TAP デバイス操作に `CAP_NET_ADMIN` 権限が必要 (またはユーザ空間の仮想 TAP)
