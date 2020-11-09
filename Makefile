BINDIR   ?= bin
PACKAGE  := cpufreq-plugin

# generate path of cabal build artifact
ARCH        := $(shell uname -i)-linux
GHC         := ghc-$(shell ghc --version | awk '{print $$NF}')
PACKAGE_VER := $(shell sed -n 's/version\s*:\s*\(.*\)/\1/p' package.yaml)
CABAL_BUILD := dist-newstyle/build/$(ARCH)/$(GHC)/$(PACKAGE)-$(PACKAGE_VER)
CABAL_BUILD := $(CABAL_BUILD)/x/$(PACKAGE)/build/$(PACKAGE)

all: $(BINDIR)/cpufreq-plugin

# we leave determining whether a new build is necessary to cabal
$(BINDIR)/cpufreq-plugin: $(BINDIR) force
	hpack
	cabal build
	cp $(CABAL_BUILD)/cpufreq-plugin $@
	strip $@

$(BINDIR):
	mkdir -p $@

clean:
	[ -d $(BINDIR) ] && rm $(BINDIR) -rf || true
	cabal clean

dist: $(PACKAGE)_$(PACKAGE_VER)_amd64.tar.gz

$(PACKAGE)_$(PACKAGE_VER)_amd64.tar.gz: all
	[ $(ARCH) = "x86_64-linux" ]
	mkdir $(PACKAGE)
	cp -r $(BINDIR) $(PACKAGE)
	cp sudoers $(PACKAGE)
	[ -f $@ ] && rm $@ || true
	tar czf $@ $(PACKAGE)
	rm $(PACKAGE) -rf

PREFIX ?= /usr/local
install:
	install -D $(BINDIR)/cpufreq-plugin -t $(DESTDIR)$(PREFIX)/bin
	install -D sudoers $(DESTDIR)/etc/sudoers.d/cpufreq -m 440 -o root
uninstall:
	rm $(DESTDIR)$(PREFIX)/bin/cpufreq-plugin
	rm $(DESTDIR)/etc/sudoers.d/cpufreq

.PHONY: force all clean dist install uninstall
