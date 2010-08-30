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
	etap:is(NewEvent#sm_event.timestamp, Timestamp, "Event should have the right timestamp"),

	Procs = [ 1, 2, 3 ],
	NewEventWithProcs = similar_events:create_event(Timestamp, Procs),
	etap:is(NewEventWithProcs#sm_event.timestamp, Timestamp, "Event should have the right timestamp"),
	etap:is(NewEventWithProcs#sm_event.procs, Procs, "Event should have the right procs"),

	NewEventStore = similar_events:create_event_store(),
	etap:is(is_dict(NewEventStore), true, "An event store should be a dict"),

	Process = 15,
	UpdatedEventStore = similar_events:schedule_process(NewEventStore, Process, Timestamp),
	etap:is(dict:is_key(Timestamp, UpdatedEventStore), true, "Timestamp should be a key"),
	StoredEvent = dict:fetch(Timestamp, UpdatedEventStore),
	etap:is(StoredEvent#sm_event.timestamp, Timestamp, "Stored event should have the same timestamp"),
	etap:is(StoredEvent#sm_event.procs, [Process], "Stored event should have the proc in the list"),

	SecondProcess = 45,
	UpdatedEventStoreWith2Process = similar_events:schedule_process(UpdatedEventStore, SecondProcess, Timestamp),
	etap:is(dict:is_key(Timestamp, UpdatedEventStoreWith2Process), true, "Timestamp should be a key"),
	UpdatedStoredEvent = dict:fetch(Timestamp, UpdatedEventStoreWith2Process),
	etap:is(UpdatedStoredEvent#sm_event.timestamp, Timestamp, "Stored event should have the same timestamp"),
	etap:is(UpdatedStoredEvent#sm_event.procs, [SecondProcess, Process], "2 processes now"),
	
	ThirdProcess = 30,
	SecondTimestamp = 45678,
	UpdatedEventStoreWith2Timestamp = similar_events:schedule_process(UpdatedEventStoreWith2Process, ThirdProcess, SecondTimestamp),
	etap:is(dict:is_key(Timestamp, UpdatedEventStoreWith2Timestamp), true, "Timestamp should be a key"),
	etap:is(dict:is_key(SecondTimestamp, UpdatedEventStoreWith2Timestamp), true, "SecondTimestamp should be a key"),
	NewStoredEvent = dict:fetch(SecondTimestamp, UpdatedEventStoreWith2Timestamp),
	etap:is(NewStoredEvent#sm_event.timestamp, SecondTimestamp, "Stored event should have the same timestamp"),
	etap:is(NewStoredEvent#sm_event.procs, [ThirdProcess], "The new event should have the process"),
	UnchangedStoredEvent = dict:fetch(Timestamp, UpdatedEventStoreWith2Timestamp),
	etap:is(UnchangedStoredEvent#sm_event.timestamp, Timestamp, "Stored event should have the same timestamp"),
	etap:is(UnchangedStoredEvent#sm_event.procs, [SecondProcess, Process], "Still 2 processes"),

	etap:end_tests(),
	ok.


is_dict(D) ->
	case catch dict:to_list(D) of
		L when is_list(L) ->
			true;
		{'EXIT', {badarg,_}} ->
			false
	end.

