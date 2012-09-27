#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./test
 
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
    test_mod_config(),
    etap:end_tests(),
    ok.

test_mod_similar() ->
    etap:loaded_ok(similar, "Module 'similar' loaded"),
    etap:is_behaviour(similar, application),

    etap:can_ok(similar, start, 0),
    etap:can_ok(similar, start, 2),
    etap:can_ok(similar, stop, 1),
    
    ok.

test_mod_events() ->
    etap:loaded_ok(similar_events, "Module 'similar_events' loaded"),

    etap:can_ok(similar_events, create_event, 1),
    etap:can_ok(similar_events, create_event, 2),
    etap:can_ok(similar_events, create_event_store, 0),
    etap:can_ok(similar_events, schedule_process, 3),
    etap:can_ok(similar_events, terminate_event, 2),
    etap:can_ok(similar_events, get_first_event, 1),
    etap:can_ok(similar_events, get_event, 2),

    ok.

test_mod_process() ->
    etap:loaded_ok(similar_process, "Module 'similar_process' loaded"),

    etap:can_ok(similar_process, create, 3),
    etap:can_ok(similar_process, execute, 3),
    etap:can_ok(similar_process, terminate, 1),
    etap:can_ok(similar_process, fall_asleep, 1),

    ok.

test_mod_query() ->
    etap:loaded_ok(similar_query, "Module 'similar_query' loaded"),

    etap:can_ok(similar_query, r, 1),
    etap:can_ok(similar_query, p, 1),
    etap:can_ok(similar_query, e, 1),
    etap:can_ok(similar_query, s, 1),
    etap:can_ok(similar_query, c, 1),
    etap:can_ok(similar_query, event_time, 1),

    ok.

test_gen_server_callbacks(Module) ->
    etap:can_ok(Module, init, 1),
    etap:can_ok(Module, handle_call, 3),
    etap:can_ok(Module, handle_cast, 2),
    etap:can_ok(Module, handle_info, 2),
    etap:can_ok(Module, terminate, 2),
    etap:can_ok(Module, code_change, 3),
    ok.

test_mod_engine() ->
    etap:loaded_ok(similar_engine, "Module 'similar_engine' loaded"),
    etap:is_behaviour(similar_engine, gen_server),

    test_gen_server_callbacks(similar_engine),

    etap:can_ok(similar_engine, start_link, 0),

    %% API
    etap:can_ok(similar_engine, kill_current, 0),
    etap:can_ok(similar_engine, reset, 0),
    etap:can_ok(similar_engine, kill_pid, 1),
    etap:can_ok(similar_engine, r, 0),
    etap:can_ok(similar_engine, p, 0),
    etap:can_ok(similar_engine, e, 0),
    etap:can_ok(similar_engine, s, 0),
    etap:can_ok(similar_engine, c, 0),
    etap:can_ok(similar_engine, event_time, 0),
    etap:can_ok(similar_engine, new_P, 3),
    etap:can_ok(similar_engine, hold, 1),

    ok.

test_mod_scenario() ->
    etap:loaded_ok(similar_scenario, "Module 'similar_scenario' loaded"),
    etap:is_behaviour(similar_scenario, gen_server),

    test_gen_server_callbacks(similar_scenario),

    etap:can_ok(similar_scenario, start_link, 0),

    ok.

test_mod_sup() ->
    etap:loaded_ok(similar_sup, "Module 'similar_sup' loaded"),
    etap:is_behaviour(similar_sup, supervisor),

    etap:can_ok(similar_sup, start_link, 1),
    etap:can_ok(similar_sup, init, 1),

    ok.

test_mod_logger() ->
    etap:loaded_ok(similar_logger, "Module 'similar_logger' loaded"),
    etap:is_behaviour(similar_logger, gen_event),

    etap:can_ok(similar_logger, init, 1),
    etap:can_ok(similar_logger, handle_event, 2),
    etap:can_ok(similar_logger, terminate, 2),
    etap:can_ok(similar_logger, handle_call, 2),
    etap:can_ok(similar_logger, handle_cast, 2),
    etap:can_ok(similar_logger, handle_info, 2),
    etap:can_ok(similar_logger, code_change, 3),

    ok.

test_mod_log() ->
    etap:loaded_ok(similar_log, "Module 'similar_log' loaded"),

    etap:can_ok(similar_log, start, 0),
    etap:can_ok(similar_log, stop, 0),
    etap:can_ok(similar_log, info, 1),
    etap:can_ok(similar_log, info, 2),
    etap:can_ok(similar_log, format_time, 1),
    etap:can_ok(similar_log, switch, 1),
    etap:can_ok(similar_log, is_activated, 0),

    ok.

test_mod_utils() ->
    etap:loaded_ok(similar_utils, "Module 'similar_utils' loaded"),

    etap:can_ok(similar_utils, create_sim_state, 0),
    etap:can_ok(similar_utils, kill_current, 1),
    etap:can_ok(similar_utils, reset, 1),

    ok.

test_mod_config() ->
    etap:loaded_ok(similar_config, "Module 'similar_config' loaded"),
    etap:is_behaviour(similar_config, gen_server),

    test_gen_server_callbacks(similar_config),

    ok.


