
###############  Changeable  Parameters  ##############

include ../Makefile.common

OUTPUT_DIR = ../bld

###############  DO NOT MODIFY BELOW THIS   ##############

SHARED_LIB_NAME         = lib$(PRODUCT_NAME).so.$(VERSION)
STATIC_LIB_NAME         = lib$(PRODUCT_NAME).a.$(VERSION)

ifeq ($(BUILD_MODE), debug)
    SHARED_LIB_NAME     = lib$(PRODUCT_NAME)_debug.so.$(VERSION)
    STATIC_LIB_NAME     = lib$(PRODUCT_NAME)_debug.a.$(VERSION)
endif

SHARED_LIB_OUTPUT       =   "$(OUTPUT_DIR)/$(SHARED_LIB_NAME)"
STATIC_LIB_OUTPUT       =   "$(OUTPUT_DIR)/$(STATIC_LIB_NAME)"

CUTILS_HOME = ../../cutils
LANGUTILS_HOME = ../../langutils
LLVM_HOME = /opt/llvm

SOURCE_FOLDERS  =   . 
LIBRARY_FOLDERS :=  $(CUTILS_HOME)/bld $(LANGUTILS_HOME)/bld
INCLUDE_FOLDERS :=  $(LLVM_HOME)/include $(foreach dir,$(SOURCE_FOLDERS),$(dir)) $(CUTILS_HOME)/src $(LANGUTILS_HOME)/src
CHUCK_SOURCES   := $(foreach dir,$(SOURCE_FOLDERS),$(wildcard $(dir)/*.c))

ifdef USE_LEMON_PARSER
ifeq (,$(findstring parser.c,$(CHUCK_SOURCES)))
    CHUCK_SOURCES   := $(CHUCK_SOURCES)  parser.c
endif
endif

ifeq (,$(findstring scanner.c,$(CHUCK_SOURCES)))
    CHUCK_SOURCES   := $(CHUCK_SOURCES)  scanner.c
endif
CHUCK_OBJS      = $(foreach obj, $(patsubst %.c,%.o,$(CHUCK_SOURCES)), $(OUTPUT_DIR)/src/$(obj))
LIBS            = -lcutils -llangutils

###################     Begin Targets       ######################

# Implicit rule to build .o into the build directory from .c
$(OUTPUT_DIR)/src/%.o : %.c
	@echo Building src/$<  \-\>  $@ 
	@$(GCC)  -std=c99 -c $(CFLAGS) $< -o $@

all: base static shared
	@echo LIB_INSTALL_DIR     = $(LIB_INSTALL_DIR)
	@echo INCLUDE_INSTALL_DIR = $(INCLUDE_INSTALL_DIR)

scanner.c: scanner.re2c
	- re2c -o scanner.c scanner.re2c

ifdef USE_LEMON_PARSER
parser.c: parser.lemon
	- lemon -m parser.lemon
else
parser.c:
endif

shared: base parser.c scanner.c $(CHUCK_OBJS)
	@echo Creating Shared Library ...
	@$(GPP) -shared $(CFLAGS) $(CHUCK_OBJS) -o "$(SHARED_LIB_OUTPUT)" $(LIBS)
	@rm -f "$(OUTPUT_DIR)/lib$(PRODUCT_NAME).so"
	@ln -s "$(SHARED_LIB_NAME)" "$(OUTPUT_DIR)/lib$(PRODUCT_NAME).so"

static: base parser.c scanner.c $(CHUCK_OBJS)
	@echo Creating Static Library ...
	@ar rcs "$(STATIC_LIB_OUTPUT)" $(CHUCK_OBJS)
	@rm -f "$(OUTPUT_DIR)/lib$(PRODUCT_NAME).a"
	@ln -s "$(STATIC_LIB_NAME)" "$(OUTPUT_DIR)/lib$(PRODUCT_NAME).a"

install: shared static
	@echo "Copying library files..."
	@mkdir -p "$(LIB_INSTALL_DIR)"
	@mkdir -p "$(INCLUDE_INSTALL_DIR)/lunarprobe"
	@cp -f "$(SHARED_LIB_OUTPUT)" "$(LIB_INSTALL_DIR)"
	@cp -f "$(STATIC_LIB_OUTPUT)" "$(LIB_INSTALL_DIR)"
	@rm -f "$(LIB_INSTALL_DIR)/lib$(PRODUCT_NAME).so"
	@ln -s "$(LIB_INSTALL_DIR)/$(SHARED_LIB_NAME)" "$(LIB_INSTALL_DIR)/lib$(PRODUCT_NAME).so"
	@echo "Copying header files..."
	@cp -f *.h $(INCLUDE_INSTALL_DIR)/lunarprobe

base:
	@echo Building output directories...  $(CHUCK_SOURCES)
	@mkdir -p "$(OUTPUT_DIR)/src"
	@cd $(OUTPUT_DIR)/src ; mkdir -p $(SOURCE_FOLDERS)

.PHONY: clean cleanall distclean parser
clean:
	@rm -f $(CHUCK_OBJS) $(ENTRY_OBJS)
	@rm -f parser.c parser.out scanner.c

cleanall: clean
	@rm -f "$(SHARED_LIB_OUTPUT)" "$(STATIC_LIB_OUTPUT)"
	@rm -f "$(OUTPUT_DIR)/lib$(PRODUCT_NAME).so"
	@rm -f "$(OUTPUT_DIR)/lib$(PRODUCT_NAME).a"

distclean: cleanall
	@rm -f Makefile

dep:
	makedepend -Y -p"$(OUTPUT_DIR)/" $(INCLUDE_FLAGS) -- $(CHUCK_SOURCES)

# DO NOT DELETE
