# 参考: https://github.com/TheNetAdmin/Makefile-Templates/blob/master/SmallProject/Template/Makefile
# compile flags
CFLAGS := -Wall -Wextra -Wtrampolines -Wconversion -Wsign-conversion 
DBGFLAGS := -g
COBJFLAGS := $(CFLAGS) -c

# path macros
BIN_PATH := bin
OBJ_PATH := obj
SRC_PATH := src
DBG_PATH := debug

# サフィクスのリスト
SUFFIXES := 32 64

TARGET := $(BIN_PATH)/shared_memory 
TARGET_DEBUG :=$(DBG_PATH)/shared_memory 

# 各ターゲットのサフィクスごとのターゲットを定義
TARGETS := $(foreach target, $(TARGET), $(foreach suf, $(SUFFIXES), $(target)_$(suf)))
# TARGET_DEBUGS := $(foreach target, $(TARGET_DEBUG), $(foreach suf, $(SUFFIXES), $(DBG_PATH)/$(notdir $(target))_$(suf)))
TARGET_DEBUGS := $(foreach target, $(TARGET_DEBUG), $(foreach suf, $(SUFFIXES), $(target))_$(suf))

INCDIR := 
# コンパイルオプションを指定する
CFLAGS += $(INCDIR)
# 
# LDFRAGS = -L./dir1 -Ldir2
LDFRAGS =
# LDLIBS = -lxxx -lyyy
LDLIBS =

# コンパイルオプションのサフィクスを定義
CFLAGS_32 := $(CFLAGS) -m32
CFLAGS_64 := $(CFLAGS) -m64
COBJFLAGS_32 := $(CFLAGS_32) -c
COBJFLAGS_64 := $(CFLAGS_64) -c

# src files & obj files
SRC := $(foreach x, $(SRC_PATH), $(wildcard $(addprefix $(x)/*,.c*)))
# OBJ := $(addprefix $(OBJ_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))
# OBJ_DEBUG := $(addprefix $(DBG_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))


# 各サフィクスごとのオブジェクトファイルを定義
OBJS := $(foreach target, $(TARGET), $(foreach suf, $(SUFFIXES), $(addprefix $(OBJ_PATH)_$(suf)/, $(addsuffix .o, $(notdir $(basename $(wildcard $(SRC_PATH)/*.c)))))))
# OBJ_DEBUGS := $(foreach target, $(TARGET_DEBUG), $(foreach suf, $(SUFFIXES), $(addprefix $(DBG_PATH)/, $(addsuffix .o, $(notdir $(basename $(wildcard $(SRC_PATH)/*.c)))))))
OBJ_DEBUGS := $(foreach target, $(TARGET_DEBUG), $(foreach suf, $(SUFFIXES), $(addprefix $(OBJ_PATH)_$(suf)/, $(addsuffix .o, $(notdir $(basename $(wildcard $(SRC_PATH)/*.c)))))))


# 元の配布物以外の削除対象の生成物
DISTCLEAN_LIST := $(OBJS) $(OBJ_DEBUGS)
# 配布物と中間ファイルなどの削除対象の生成物
CLEAN_LIST := $(TARGETS) $(TARGET_DEBUGS) $(DISTCLEAN_LIST)

# default rule
default: makedir all
 
# シンプルな記述
# %.o: %.c
#	$(CC) $(CFLAGS) $< -o $@

# non-phony targets
$(TARGETS): $(OBJS)
#	$(CC) -o $@ $(OBJ) $(CFLAGS) $(LDFRAGS) $(LDLIBS)
	$(foreach target, $(TARGET), $(foreach suf, $(SUFFIXES), $(CC) -o $(target)_$(suf) $(filter $(OBJ_PATH)_$(suf)/%, $(OBJS)) $(CFLAGS_$(suf)) $(LDFRAGS) $(LDLIBS);))

# $(OBJ_PATH)/%.o: $(SRC_PATH)/%.c*
# 	$(CC) $(COBJFLAGS) -o $@ $< $(LDFRAGS) $(LDLIBS)
# 
# $(DBG_PATH)/%.o: $(SRC_PATH)/%.c*
# 	$(CC) $(COBJFLAGS) $(DBGFLAGS) -o $@ $< $(LDFRAGS) $(LDLIBS)

$(OBJ_PATH)_32/%.o: $(SRC_PATH)/%.c
	$(CC) $(COBJFLAGS_32) -o $@ $< $(LDFRAGS) $(LDLIBS)

$(OBJ_PATH)_64/%.o: $(SRC_PATH)/%.c
	$(CC) $(COBJFLAGS_64) -o $@ $< $(LDFRAGS) $(LDLIBS)

#	$(CC) $(CFLAGS) $(DBGFLAGS) $(OBJ_DEBUG) -o $@ $(LDFRAGS) $(LDLIBS)
#	$(foreach target, $(TARGET_DEBUG), $(foreach suf, $(SUFFIXES), $(CC) $(CFLAGS_$(suf)) $(DBGFLAGS) $(filter $(DBG_PATH)/%, $(OBJ_DEBUGS)) -o $(DBG_PATH)/$(notdir $(target))_$(suf) $(LDFRAGS) $(LDLIBS);))

$(TARGET_DEBUGS): $(OBJ_DEBUGS)
	$(foreach target, $(TARGET_DEBUG), $(foreach suf, $(SUFFIXES), $(CC) -o $(target)_$(suf) $(DBGFLAGS) $(filter $(OBJ_PATH)_$(suf)/%, $(OBJ_DEBUGS)) $(CFLAGS_$(suf)) $(LDFRAGS) $(LDLIBS);))

# phony rules
.PHONY: makedir
makedir:
	@mkdir -p $(BIN_PATH)  $(foreach suf, $(SUFFIXES), $(OBJ_PATH)_$(suf)) $(DBG_PATH)

.PHONY: all
all: $(TARGETS)

.PHONY: debug
debug: $(TARGET_DEBUGS)

.PHONY: clean
clean:
	@echo clean $(CLEAN_LIST)
	@rm -f $(CLEAN_LIST)

.PHONY: distclean
distclean:
	@echo clean $(DISTCLEAN_LIST)
	@rm -f $(DISTCLEAN_LIST)
