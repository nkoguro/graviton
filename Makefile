SCRIPT_GENERATED = lib/graviton/enum2sym.scm

all:
	(cd lib; make)

clean:
	(cd lib; make clean)

maintainer-clean: clean
	rm -f $(SCRIPT_GENERATED)
	(cd lib; make maintainer-clean)
