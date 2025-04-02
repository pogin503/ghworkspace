# 参考: https://github.com/TheNetAdmin/Makefile-Templates/blob/master/SmallProject/Template/Makefile
CC := gcc

# コンパイルフラグ
# CFLAGS := -Wall -Wextra -Wtrampolines -Wconversion -Wsign-conversion -Walloc-zero -Walloca-larger-than -Wformat-security -Wvla-larger-than
CFLAGS := -Wall -Wextra -Wtrampolines -Wconversion -Wsign-conversion -Walloc-zero -Wformat-security
DBGFLAGS := -g
INCDIRS := # インクルードするディレクトリ
CFLAGS += $(INCDIRS)
# オブジェクトファイル作成時のフラグ
COBJFLAGS := $(CFLAGS) -c

# リンク時のフラグ
# LDFRAGS = -Ldir1 -Ldir2
LDFRAGS =
# LDLIBS = -lxxx -lyyy
LDLIBS =

# path macros
BIN_PATH := bin
OBJ_PATH := obj
SRC_PATH := src
DBG_PATH := debug

# 実行ファイル
TARGET_NAME=signal
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

# シンプルな記述
# %.o: %.c
#	$(CC) $(CFLAGS) $< -o $@

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
