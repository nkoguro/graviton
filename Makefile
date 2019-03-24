SCRIPT_GENERATED = lib/graviton/enum2sym.scm

all:
	(cd lib; make)

maintainer-clean:
	rm -f $(SCRIPT_GENERATED)
