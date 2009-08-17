#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin
 
main(_) ->
	etap:plan(3),
	etap:ok(true, "the 'true' atom is recognized"),
	etap:is(1 + 1, 2, "simple math"),
	etap:isnt(2 + 2, 5, "some would argue"),
	etap:end_tests(),
	ok.
