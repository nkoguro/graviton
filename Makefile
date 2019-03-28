SCRIPT_GENERATED = src/enum2sym.scm

all:
	(cd src; make)

clean:
	(cd src; make clean)

maintainer-clean: clean
	rm -f $(SCRIPT_GENERATED)
	(cd src; make maintainer-clean)
