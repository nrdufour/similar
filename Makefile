
TEST_SUPPORT= \
	test/etap.beam \
	test/test_util.beam

all:
	./rebar compile

%.beam: %.erl
	erlc -o test/ $<

check: all $(TEST_SUPPORT)
	prove test/*.t

clean: 
	./rebar clean
	rm -f test/*.beam
