#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin
 
main(_) ->
	etap:plan(1),
	State = similar_utils:create_sim_state(),
	etap:is(similar_query:event_time(State), 0, "the event time should be zero"),
	etap:end_tests(),
	ok.
