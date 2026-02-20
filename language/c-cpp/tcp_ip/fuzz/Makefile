# fuzz/Makefile - libFuzzer + AddressSanitizer + UBSanitizer によるファズビルド
#
# 使い方:
#   make              全ファズターゲットをビルド
#   make seeds        シードコーパスを生成 (corpus/ 以下に配置)
#   make run-eth      eth_recv() ファザーを実行
#   make run-arp      arp_recv() ファザーを実行
#   make run-ip       ip_recv() ファザーを実行
#   make run-icmp     icmp_recv() ファザーを実行
#   make run-tcp      tcp_recv() ファザーを実行
#   make clean        ビルド成果物を削除

CC := clang

# -fsanitize=fuzzer: libFuzzerのエントリーポイント (main) を組み込む
# -fsanitize=address: AddressSanitizer (OOB, use-after-free 等を検出)
# -fsanitize=undefined: UndefinedBehaviorSanitizer (整数オーバーフロー等)
# -g: デバッグシンボル (クラッシュ時のスタックトレースに必要)
# -O1: 最適化 (0だと遅すぎる、2以上だとASAN精度が落ちる)
FUZZ_FLAGS := -fsanitize=fuzzer,address,undefined -g -O1

# libFuzzer は C++ ランタイムを必要とする。
# clang が libstdc++.so を見つけられない場合に gcc のライブラリパスを明示する。
GCC_LIB_DIR := $(shell gcc -print-file-name=libstdc++.so | xargs dirname 2>/dev/null)
LDFLAGS_FUZZ := $(if $(GCC_LIB_DIR),-L$(GCC_LIB_DIR),)

CFLAGS := -Wall -Wextra -Wconversion -Wsign-conversion \
          -I../include \
          -DFUZZING \
          $(FUZZ_FLAGS)

# main.c と tun.c を除いた共有ソースファイル
# tun.c の代わりに tun_mock.c を使う
SHARED_SRCS := \
    ../src/utils.c    \
    ../src/ethernet.c \
    ../src/arp.c      \
    ../src/ip.c       \
    ../src/icmp.c     \
    ../src/tcp.c

MOCK_SRC := tun_mock.c

TARGETS := fuzz_eth fuzz_arp fuzz_ip fuzz_icmp fuzz_tcp

SEED_GEN_CC := gcc
SEED_GEN_FLAGS := -Wall -I../include

.PHONY: all
all: $(TARGETS)

# --- ファズターゲット ---

fuzz_eth: fuzz_eth.c $(SHARED_SRCS) $(MOCK_SRC)
	$(CC) $(CFLAGS) $(LDFLAGS_FUZZ) -o $@ $^

fuzz_arp: fuzz_arp.c $(SHARED_SRCS) $(MOCK_SRC)
	$(CC) $(CFLAGS) $(LDFLAGS_FUZZ) -o $@ $^

fuzz_ip: fuzz_ip.c $(SHARED_SRCS) $(MOCK_SRC)
	$(CC) $(CFLAGS) $(LDFLAGS_FUZZ) -o $@ $^

fuzz_icmp: fuzz_icmp.c $(SHARED_SRCS) $(MOCK_SRC)
	$(CC) $(CFLAGS) $(LDFLAGS_FUZZ) -o $@ $^

fuzz_tcp: fuzz_tcp.c $(SHARED_SRCS) $(MOCK_SRC)
	$(CC) $(CFLAGS) $(LDFLAGS_FUZZ) -o $@ $^

# --- シードコーパス生成 ---

generate_seeds: generate_seeds.c $(MOCK_SRC) ../src/utils.c ../src/arp.c ../src/ip.c ../src/icmp.c ../src/tcp.c ../src/ethernet.c
	$(SEED_GEN_CC) $(SEED_GEN_FLAGS) -o $@ generate_seeds.c

.PHONY: seeds
seeds: generate_seeds
	./generate_seeds

# --- ファズ実行 (デフォルト: 60秒実行) ---

FUZZ_RUNS ?= -max_total_time=60
FUZZ_MAX_LEN ?= -max_len=1500

.PHONY: run-eth
run-eth: fuzz_eth seeds
	./fuzz_eth corpus/eth/ $(FUZZ_RUNS) $(FUZZ_MAX_LEN)

.PHONY: run-arp
run-arp: fuzz_arp seeds
	./fuzz_arp corpus/arp/ $(FUZZ_RUNS) $(FUZZ_MAX_LEN)

.PHONY: run-ip
run-ip: fuzz_ip seeds
	./fuzz_ip corpus/ip/ $(FUZZ_RUNS) $(FUZZ_MAX_LEN)

.PHONY: run-icmp
run-icmp: fuzz_icmp seeds
	./fuzz_icmp corpus/icmp/ $(FUZZ_RUNS) $(FUZZ_MAX_LEN)

.PHONY: run-tcp
run-tcp: fuzz_tcp seeds
	./fuzz_tcp corpus/tcp/ $(FUZZ_RUNS) $(FUZZ_MAX_LEN)

.PHONY: clean
clean:
	rm -f $(TARGETS) generate_seeds
