#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin
 
main(_) ->
	etap:plan(10),
	etap_can:loaded_ok(similar, "Module 'similar' loaded"),
	etap_can:loaded_ok(similar_events, "Module 'similar_events' loaded"),
	etap_can:loaded_ok(similar_file_logger, "Module 'similar_file_logger' loaded"),
	etap_can:loaded_ok(similar_manager, "Module 'similar_manager' loaded"),
	etap_can:loaded_ok(similar_process, "Module 'similar_process' loaded"),
	etap_can:loaded_ok(similar_query, "Module 'similar_query' loaded"),
	etap_can:loaded_ok(similar_server, "Module 'similar_server' loaded"),
	etap_can:loaded_ok(similar_sup, "Module 'similar_sup' loaded"),
	etap_can:loaded_ok(similar_terminal_logger, "Module 'similar_terminal_logger' loaded"),
	etap_can:loaded_ok(similar_utils, "Module 'similar_utils' loaded"),
	etap:end_tests(),
	ok.
