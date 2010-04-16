LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)

VERSION=0.0.1

all:
	mkdir -p ebin
	(cd src;$(MAKE))

doc:
	(cd src; $(MAKE) doc)

test: all 
	prove t/*.t

cover: all 
	rm -f cover/*.coverdata
	COVER=1 COVER_BIN=./ebin prove t/*.t
	SRC=./src/ \
		erl -noshell \
		-eval 'etap_report:create()' \
		-s init stop  > /dev/null 2>&1

clean:
	(cd src;$(MAKE) clean)
	rm -rf cover/
	rm -rf ebin/

install: all
	mkdir -p $(prefix)/$(LIBDIR)/similar-$(VERSION)/ebin
	for i in ebin/*; do install $$i $(prefix)/$(LIBDIR)/similar-$(VERSION)/$$i ; done

