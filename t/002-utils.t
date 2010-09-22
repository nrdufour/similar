#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

-include_lib("./include/similar.hrl").

main(_) ->
	etap:plan(unknown),

	etap:loaded_ok(similar_utils, "Module 'similar_utils' loaded."),
	etap:can_ok(similar_utils, create_sim_state, 0),
	etap:can_ok(similar_utils, kill_current, 1),
	etap:can_ok(similar_utils, reset, 1),

	test_create_sim_state(),
	
	etap:end_tests(),
	ok.

test_create_sim_state() ->
	State = similar_utils:create_sim_state(),
	etap:is(is_dict(State#sm_data.events), true, "Events should be a dict"),
	etap:is(State#sm_data.resources, [], "Resources should be empty"),
	etap:is(State#sm_data.actives, [], "Actives should be empty"),
	etap:is(State#sm_data.time, 0, "Time should be 0"),
	etap:is(State#sm_data.props, [], "Props should be empty"),
	ok.

is_dict(D) ->
	case catch dict:to_list(D) of
		L when is_list(L) ->
			true;
		{'EXIT', {badarg,_}} ->
			false
	end.

