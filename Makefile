all:
	mkdir -p ebin/
	mkdir -p doc/
	(cd src;$(MAKE))
 
test: all
	prove t/*.t
 
clean:
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump *.beam *.hrl cover
 
cover: all
	COVER=1 prove -v t/*.t
	erl -detached -noshell -eval 'etap_report:create()' -s init stop
