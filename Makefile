
include Makefile.common

.PHONY: all libs tests harness install
all: libs tests harness


libs: src
	@cd src ; make static shared

tests: libs
	@cd tests ; make

harness: libs
	@cd harness; make

.PHONY: clean cleanall distclean package
clean:
	@cd src ; make clean
	@cd tests ; make clean
	@cd harness; make clean
	@rm -Rf $(OUTPUT_DIR)

cleanall: clean
	@cd src ; make cleanall
	@cd tests ; make cleanall
	@cd harness; make cleanall
	@rm -Rf $(OUTPUT_DIR)

distclean: cleanall
	@cd src ; make distclean
	@cd tests ; make distclean
	@cd harness; make distclean
	@rm -Rf $(OUTPUT_DIR)

