#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(2),
    etap_application:load_ok(similar, "Application Similar loads"),
    etap_application:start_ok(similar, "Application Similar starts"),
    etap:end_tests(),
    ok.

