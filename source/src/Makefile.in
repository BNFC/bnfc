include config.mk

.PHONY: all install clean

all: force
	cd src; make -f ../config.mk -f Makefile;cd ..
force: ;
install:
	$(INSTALL) -d $(bindir)
	$(INSTALL) bin/$(PROGRAM)$(EXEEXT) $(bindir)
clean:
	find . '(' -name '*~' -o -name '*.hi' -o -name '*.ghi' -o -name '*.o' ')' -exec rm -f '{}' ';'
