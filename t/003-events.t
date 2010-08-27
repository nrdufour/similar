#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz ./ebin

-include_lib("./include/similar_data.hrl").

main(_) ->
	etap:plan(unknown),

	etap_can:loaded_ok(similar_events, "Module 'similar_events' loaded."),
	etap_can:can_ok(similar_events, create_event, 1),
	etap_can:can_ok(similar_events, create_event, 2),
	etap_can:can_ok(similar_events, create_event_store, 0),
	etap_can:can_ok(similar_events, schedule_process, 3),
	etap_can:can_ok(similar_events, terminate_event, 2),
	etap_can:can_ok(similar_events, get_first_event, 1),
	etap_can:can_ok(similar_events, get_event, 2),

	Timestamp = 12345,
	NewEvent = similar_events:create_event(Timestamp),
	etap:is(Timestamp, NewEvent#sm_event.timestamp, "Event should have the right timestamp"),

	Procs = [ 1, 2, 3 ],
	NewEventWithProcs = similar_events:create_event(Timestamp, Procs),
	etap:is(Timestamp, NewEventWithProcs#sm_event.timestamp, "Event should have the right timestamp"),
	etap:is(Procs, NewEventWithProcs#sm_event.procs, "Event should have the right procs"),

	NewEventStore = similar_events:create_event_store(),
	etap:is(true, is_dict(NewEventStore), "An event store should be a dict"),

	Process = 15,
	UpdatedEventStore = similar_events:schedule_process(NewEventStore, Process, Timestamp),
	etap:is(true, dict:is_key(Timestamp, UpdatedEventStore), "Timestamp should be a key"),
	StoredEvent = dict:fetch(Timestamp, UpdatedEventStore),
	etap:is(Timestamp, StoredEvent#sm_event.timestamp, "Stored event should have the same timestamp"),
	etap:is([Process], StoredEvent#sm_event.procs, "Stored event should have the proc in the list"),

	etap:end_tests(),
	ok.


is_dict(D) ->
	case catch dict:to_list(D) of
		L when is_list(L) ->
			true;
		{'EXIT', {badarg,_}} ->
			false
	end.

