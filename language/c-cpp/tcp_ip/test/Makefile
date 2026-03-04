# test/Makefile - Unity によるユニットテスト
#
# 使い方:
#   make          全テストをビルドして実行
#   make build    ビルドのみ (実行しない)
#   make clean    ビルド成果物を削除

CC     := gcc
CFLAGS := -Wall -Wextra -Wconversion -Wsign-conversion \
          -g -O0 \
          -I../include \
          -I../src \
          -DFUZZING

# Unity ソース (src/ に配置済み)
UNITY_SRC := ../src/unity.c

# tun.c のモック (fuzz/ と共用)
MOCK_SRC := ../fuzz/tun_mock.c

# 共有ソース (main.c と tun.c は除く)
SHARED_SRCS := \
    ../src/utils.c    \
    ../src/ethernet.c \
    ../src/arp.c      \
    ../src/ip.c       \
    ../src/icmp.c     \
    ../src/tcp.c

TESTS := test_utils test_arp test_tcp

.PHONY: all
all: build
	@echo "=== Running tests ==="
	@for t in $(TESTS); do \
	    echo "--- $$t ---"; \
	    ./$$t; \
	done

.PHONY: build
build: $(TESTS)

test_utils: test_utils.c $(UNITY_SRC) ../src/utils.c $(MOCK_SRC)
	$(CC) $(CFLAGS) -o $@ $^

test_arp: test_arp.c $(UNITY_SRC) $(SHARED_SRCS) $(MOCK_SRC)
	$(CC) $(CFLAGS) -o $@ $^

test_tcp: test_tcp.c $(UNITY_SRC) $(SHARED_SRCS) $(MOCK_SRC)
	$(CC) $(CFLAGS) -o $@ $^

.PHONY: clean
clean:
	rm -f $(TESTS)
