SCRIPT_GENERATED = lib/graviton/enum2sym.scm

lib/graviton/enum2sym.scm:
	mkdir -p lib/graviton
	gosh -I. scripts/genenum2sym > $@

maintainer-clean:
	rm -f $(SCRIPT_GENERATED)
