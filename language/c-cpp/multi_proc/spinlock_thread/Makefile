# 参考: https://github.com/TheNetAdmin/Makefile-Templates/blob/master/SmallProject/Template/Makefile
CC := gcc

# コンパイルフラグ
CFLAGS := -Wall -Wextra -Wtrampolines -Wconversion -Wsign-conversion -Walloc-zero -Wformat-security -pthread
DBGFLAGS := -g
INCDIRS := # インクルードするディレクトリ
CFLAGS += $(INCDIRS)
# オブジェクトファイル作成時のフラグ
COBJFLAGS := $(CFLAGS) -c

# リンク時のフラグ
LDFRAGS =
# -lpthread ではなく -pthread を使う理由:
#   -lpthread はリンク時にライブラリを繋ぐだけだが、
#   -pthread はコンパイル時に _REENTRANT などスレッドセーフな動作に必要な
#   マクロも有効化する。ツールチェインによっては -lpthread だけでは
#   pthread_spinlock_t などの型が正しく宣言されない場合がある。
#   CFLAGS にも指定することでコンパイル・リンク両方に適用される。
#
# LDFLAGS ではなく LDLIBS に書く理由:
#   LDFLAGS はライブラリ検索パス (-L) やリンカオプション (-Wl,...) 用。
#   LDLIBS はライブラリを繋ぐフラグ (-l) 用。-pthread はライブラリを
#   繋く役割なので LDLIBS が適切。
#   また、リンクコマンドは "$(OBJ) $(LDFRAGS) $(LDLIBS)" の順になっており、
#   ライブラリフラグをオブジェクトファイルより後に置くことで、
#   一部のリンカで発生する未解決シンボルエラーを防いでいる。
LDLIBS = -pthread

# path macros
BIN_PATH := bin
OBJ_PATH := obj
SRC_PATH := src
DBG_PATH := debug

# 実行ファイル
TARGET_NAME=spinlock_thread
ifeq ($(OS),Windows_NT)
	TARGET_NAME := $(addsuffix .exe,$(TARGET_NAME))
endif
TARGET := $(BIN_PATH)/$(TARGET_NAME)
TARGET_DEBUG := $(DBG_PATH)/$(TARGET_NAME)

# ソースファイル & オブジェクトファイル
SRC := $(foreach x, $(SRC_PATH), $(wildcard $(addprefix $(x)/*,.c*)))
OBJ := $(addprefix $(OBJ_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))
OBJ_DEBUG := $(addprefix $(DBG_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))

# 元の配布物以外の削除対象の生成物
DISTCLEAN_LIST := $(OBJ) $(OBJ_DEBUG)
# 配布物と中間ファイルなどの削除対象の生成物
CLEAN_LIST := $(TARGET) $(TARGET_DEBUG) $(DISTCLEAN_LIST)

# デフォルトルール
default: makedir all

# non-phony targets
$(TARGET): $(OBJ)
	$(CC) -o $@ $(OBJ) $(LDFRAGS) $(LDLIBS)

$(OBJ_PATH)/%.o: $(SRC_PATH)/%.c*
	$(CC) $(COBJFLAGS) -o $@ $<

$(TARGET_DEBUG): $(OBJ_DEBUG)
	$(CC) $(DBGFLAGS) $(OBJ_DEBUG) -o $@ $(LDFRAGS) $(LDLIBS)

$(DBG_PATH)/%.o: $(SRC_PATH)/%.c*
	$(CC) $(COBJFLAGS) $(DBGFLAGS) -o $@ $<

# phonyルール
.PHONY: makedir
makedir:
	@mkdir -p $(BIN_PATH) $(OBJ_PATH) $(DBG_PATH)

.PHONY: all
all: $(TARGET)

.PHONY: clean
clean:
	@echo clean $(CLEAN_LIST)
	@rm -f $(CLEAN_LIST)

.PHONY: distclean
distclean:
	@echo clean $(DISTCLEAN_LIST)
	@rm -f $(DISTCLEAN_LIST)
