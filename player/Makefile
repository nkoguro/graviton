.PHONY: build-all zip-all clean

PLATFORM_ARCHS := win32-x64 win32-ia32 darwin-x64 linux-x64 linux-armv7l linux-arm64

BUILD_TARGETS :=
ZIP_TARGETS :=

define player-template
.PHONY: build-$(1)

build-$(1):
	(cd src; npx electron-packager . graviton-player --platform=$(word 1,$(subst -, ,$(1))) --arch=$(word 2,$(subst -, ,$(1))) --out=.. --verbose)
	if [ -f graviton-player-$(1)/graviton-player.exe ]; then \
	  chmod a+x graviton-player-$(1)/graviton-player.exe; \
	fi

BUILD_TARGETS := $(BUILD_TARGETS) build-$(1)

graviton-player-$(1).zip: build-$(1)
	zip -r $$@ graviton-player-$(1)

ZIP_TARGETS := $(ZIP_TARGETS) graviton-player-$(1).zip

endef

$(foreach platform_arch,$(PLATFORM_ARCHS),$(eval $(call player-template,$(platform_arch))))

build-all: $(BUILD_TARGETS)

zip-all: $(ZIP_TARGETS)

clean:
	rm -Rf graviton-player-*
