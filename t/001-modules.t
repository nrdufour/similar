#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
 
main(_) ->
	etap:plan(unknown),
	test_mod_similar(),
	test_mod_events(),
	test_mod_process(),
	test_mod_query(),
	test_mod_engine(),
	test_mod_scenario(),
	test_mod_sup(),
	test_mod_logger(),
	test_mod_log(),
	test_mod_utils(),
	etap:end_tests(),
	ok.

test_mod_similar() ->
	etap:loaded_ok(similar, "Module 'similar' loaded"),
	etap:is_behaviour(similar, application),
	ok.

test_mod_events() ->
	etap:loaded_ok(similar_events, "Module 'similar_events' loaded"),
	ok.

test_mod_process() ->
	etap:loaded_ok(similar_process, "Module 'similar_process' loaded"),
	ok.

test_mod_query() ->
	etap:loaded_ok(similar_query, "Module 'similar_query' loaded"),
	ok.

test_mod_engine() ->
	etap:loaded_ok(similar_engine, "Module 'similar_engine' loaded"),
	etap:is_behaviour(similar_engine, gen_server),
	ok.

test_mod_scenario() ->
	etap:loaded_ok(similar_scenario, "Module 'similar_scenario' loaded"),
	etap:is_behaviour(similar_scenario, gen_server),
	ok.

test_mod_sup() ->
	etap:loaded_ok(similar_sup, "Module 'similar_sup' loaded"),
	etap:is_behaviour(similar_sup, supervisor),
	ok.

test_mod_logger() ->
	etap:loaded_ok(similar_logger, "Module 'similar_logger' loaded"),
	etap:is_behaviour(similar_logger, gen_event),
	ok.

test_mod_log() ->
	etap:loaded_ok(similar_log, "Module 'similar_log' loaded"),
	ok.

test_mod_utils() ->
	etap:loaded_ok(similar_utils, "Module 'similar_utils' loaded"),
	ok.

