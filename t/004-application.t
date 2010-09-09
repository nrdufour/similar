#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

main(_) ->
    etap:plan(2),
    etap:load_ok(similar, "Application Similar loads"),
    etap:start_ok(similar, "Application Similar starts"),
    etap:end_tests(),
    ok.

