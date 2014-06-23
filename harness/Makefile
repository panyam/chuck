
###############  Changeable  Parameters  ##############

include ../Makefile.common

OUTPUT_DIR = ../bld

ifeq ($(MAIN_EXE_NAME),)
    MAIN_EXE_NAME     =     chuck
endif

###############  DO NOT MODIFY BELOW THIS   ##############

CUTILS_HOME = ../../cutils
LANGUTILS_HOME = ../../langutils

HARNESS_SOURCE_FOLDERS = .
INCLUDE_FOLDERS     = ../src 
INCLUDE_FOLDERS     = ../src $(CUTILS_HOME)/src $(LANGUTILS_HOME)/src
LIBRARY_FOLDERS     = $(CUTILS_HOME)/bld $(LANGUTILS_HOME)/bld
HARNESS_SOURCES     := $(foreach dir,$(HARNESS_SOURCE_FOLDERS),$(wildcard $(dir)/*.cpp))

HARNESS_OBJS           = $(foreach obj, $(patsubst %.cpp,%.o,$(HARNESS_SOURCES)), $(OUTPUT_DIR)/harness/$(obj))
HARNESS_OUTPUT         = $(OUTPUT_DIR)/$(MAIN_EXE_NAME)
ARCHIVES            = 

LIBS                =  


###################     Begin Targets       ######################

# Implicit rule to build .o into the build directory from .cpp
$(OUTPUT_DIR)/harness/%.o : %.cpp
	@echo Building harness/$<  \-\>  $@ 
	@$(GPP) -I. -c $(CFLAGS) $< -o $@

.PHONY: clean cleanall distclean harness

harness: base $(HARNESS_OBJS) 
	@echo Building Test with Static Libraries...
	@$(GPP) $(CFLAGS) $(HARNESS_OBJS) $(OUTPUT_DIR)/libchuck.a $(CUTILS_HOME)/bld/libcutils.a $(LANGUTILS_HOME)/bld/liblangutils.a $(ARCHIVES) -o $(HARNESS_OUTPUT) $(LIBS)

base:
	@echo Building harness output directories...
	@mkdir -p "$(OUTPUT_DIR)/harness"

clean:
	@echo Tests Objs: $(HARNESS_OBJS)
	@rm -f $(HARNESS_OBJS)

cleanall: clean
	@rm -f "$(HARNESS_OUTPUT)"

distclean: cleanall
	@rm -f Makefile

dep:
	makedepend -Y -p"$(OUTPUT_DIR)/" $(INCLUDE_FLAGS)  -- $(HARNESS_SOURCES)
# DO NOT DELETE

