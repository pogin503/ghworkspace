# 参考: https://github.com/TheNetAdmin/Makefile-Templates/blob/master/SmallProject/Template/Makefile
CC := gcc

# compile flags
CFLAGS := -Wall -Wextra -Wtrampolines -Wconversion -Wsign-conversion
DBGFLAGS := -g
INCDIRS := -Iinclude
CFLAGS += $(INCDIRS)
COBJFLAGS := $(CFLAGS) -c

# path macros
BIN_PATH := bin
OBJ_PATH := obj
SRC_PATH := src
DBG_PATH := debug

# 実行ファイル
TARGET_NAME := tcp_stack
TARGET      := $(BIN_PATH)/$(TARGET_NAME)
TARGET_DEBUG := $(DBG_PATH)/$(TARGET_NAME)

INCDIR :=
CFLAGS += $(INCDIR)

# LDFRAGS = -L./dir1 -Ldir2
LDFRAGS =
# LDLIBS = -lxxx -lyyy
LDLIBS =

# src files & obj files
SRC := $(foreach x, $(SRC_PATH), $(wildcard $(addprefix $(x)/*,.c)))
OBJ := $(addprefix $(OBJ_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))
OBJ_DEBUG := $(addprefix $(DBG_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))

# 元の配布物以外の削除対象の生成物
DISTCLEAN_LIST := $(OBJ) $(OBJ_DEBUG)
# 配布物と中間ファイルなどの削除対象の生成物
CLEAN_LIST := $(TARGET) $(TARGET_DEBUG) $(DISTCLEAN_LIST)

# default rule
default: makedir all

# non-phony targets
$(TARGET): $(OBJ)
	$(CC) -o $@ $(OBJ) $(CFLAGS) $(LDFRAGS) $(LDLIBS)

$(OBJ_PATH)/%.o: $(SRC_PATH)/%.c
	$(CC) $(COBJFLAGS) -o $@ $< $(LDFRAGS) $(LDLIBS)

$(DBG_PATH)/%.o: $(SRC_PATH)/%.c
	$(CC) $(COBJFLAGS) $(DBGFLAGS) -o $@ $< $(LDFRAGS) $(LDLIBS)

$(TARGET_DEBUG): $(OBJ_DEBUG)
	$(CC) $(CFLAGS) $(DBGFLAGS) $(OBJ_DEBUG) -o $@ $(LDFRAGS) $(LDLIBS)

# phony rules
.PHONY: makedir
makedir:
	@mkdir -p $(BIN_PATH) $(OBJ_PATH) $(DBG_PATH)

.PHONY: all
all: $(TARGET)

.PHONY: debug
debug: makedir $(TARGET_DEBUG)

.PHONY: clean
clean:
	@echo clean $(CLEAN_LIST)
	@rm -f $(CLEAN_LIST)

.PHONY: distclean
distclean:
	@echo clean $(DISTCLEAN_LIST)
	@rm -f $(DISTCLEAN_LIST)

# ファズターゲットのビルド (clang + libFuzzer + ASAN/UBSAN)
# 前提: clang がインストール済みであること
.PHONY: fuzz
fuzz:
	$(MAKE) -C fuzz
