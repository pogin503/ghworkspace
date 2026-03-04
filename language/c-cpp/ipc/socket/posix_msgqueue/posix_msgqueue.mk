# 参考: https://github.com/TheNetAdmin/Makefile-Templates/blob/master/SmallProject/Template/Makefile
CC := gcc
 
# compile flags
CFLAGS := -Wall -Wextra -Wtrampolines -Wconversion -Wsign-conversion 
DBGFLAGS := -g
COBJFLAGS := $(CFLAGS) -c

# path macros
BIN_PATH := bin
OBJ_PATH := obj
SRC_PATH := src
DBG_PATH := debug

TARGET := $(BIN_PATH)/posix_msgqueue
TARGET_DEBUG := $(DBG_PATH)/posix_msgqueue

INCDIR := 
# コンパイルオプションを指定する
CFLAGS += $(INCDIR)
 
# LDFRAGS = -L./dir1 -Ldir2
LDFRAGS =
# LDLIBS = -lxxx -lyyy
LDLIBS = -lrt

# src files & obj files
SRC := $(foreach x, $(SRC_PATH), $(wildcard $(addprefix $(x)/*,.c*)))
OBJ := $(addprefix $(OBJ_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))
OBJ_DEBUG := $(addprefix $(DBG_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))

# 元の配布物以外の削除対象の生成物
DISTCLEAN_LIST := $(OBJ) $(OBJ_DEBUG)
# 配布物と中間ファイルなどの削除対象の生成物
CLEAN_LIST := $(TARGET) $(TARGET_DEBUG) $(DISTCLEAN_LIST)

# default rule
default: makedir all

# シンプルな記述
# %.o: %.c
#	$(CC) $(CFLAGS) $< -o $@

# non-phony targets
$(TARGET): $(OBJ)
	$(CC) -o $@ $(OBJ) $(CFLAGS) $(LDFRAGS) $(LDLIBS)

$(OBJ_PATH)/%.o: $(SRC_PATH)/%.c*
	$(CC) $(COBJFLAGS) -o $@ $< $(LDFRAGS) $(LDLIBS)

$(DBG_PATH)/%.o: $(SRC_PATH)/%.c*
	$(CC) $(COBJFLAGS) $(DBGFLAGS) -o $@ $< $(LDFRAGS) $(LDLIBS)

$(TARGET_DEBUG): $(OBJ_DEBUG)
	$(CC) $(CFLAGS) $(DBGFLAGS) $(OBJ_DEBUG) -o $@ $(LDFRAGS) $(LDLIBS)

# phony rules
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
