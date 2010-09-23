#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
 
main(_) ->
	etap:plan(8),
	etap:loaded_ok(similar, "Module 'similar' loaded"),
	etap:loaded_ok(similar_events, "Module 'similar_events' loaded"),
	etap:loaded_ok(similar_process, "Module 'similar_process' loaded"),
	etap:loaded_ok(similar_query, "Module 'similar_query' loaded"),
	etap:loaded_ok(similar_engine, "Module 'similar_engine' loaded"),
	etap:loaded_ok(similar_sup, "Module 'similar_sup' loaded"),
	etap:loaded_ok(similar_logger, "Module 'similar_logger' loaded"),
	etap:loaded_ok(similar_utils, "Module 'similar_utils' loaded"),
	etap:end_tests(),
	ok.
