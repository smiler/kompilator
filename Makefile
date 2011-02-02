SUBDIRS = lexer parser semantic rtl mips

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

parser: lexer

semantic: parser

rtl: semantic

mips: rtl

