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

	test_create_event(),
	test_create_event_store(),
	test_schedule_process_with_one_process(),
	test_schedule_process_with_two_processes(),
	test_schedule_process_with_three_processes(),

	etap:end_tests(),
	ok.

test_create_event() ->
	Timestamp = 12345,
	Event = similar_events:create_event(Timestamp),
	etap:is(Event#sm_event.timestamp, Timestamp, "Event should have the right timestamp"),

	Procs = [ 1, 2, 3 ],
	EventWithProcs = similar_events:create_event(Timestamp, Procs),
	etap:is(EventWithProcs#sm_event.timestamp, Timestamp, "Event should have the right timestamp"),
	etap:is(EventWithProcs#sm_event.procs, Procs, "Event should have the right procs").

test_create_event_store() ->
	EventStore = similar_events:create_event_store(),
	etap:is(is_dict(EventStore), true, "An event store should be a dict").

test_schedule_process_with_one_process() ->
	%% create an event store and a fake process id
	EventStore = similar_events:create_event_store(),
	Process = 15,
	Timestamp = 12345,

	%% do the test
	UpdatedEventStore = similar_events:schedule_process(EventStore, Process, Timestamp),
	etap:is(dict:is_key(Timestamp, UpdatedEventStore), true, "Timestamp should be a key"),
	Event = dict:fetch(Timestamp, UpdatedEventStore),
	etap:is(Event#sm_event.timestamp, Timestamp, "Stored event should have the same timestamp"),
	etap:is(Event#sm_event.procs, [Process], "Stored event should have the proc in the list").

test_schedule_process_with_two_processes() ->
	%% create an event store and schedule a first process
	EventStore = similar_events:create_event_store(),
	Process = 15,
	Timestamp = 12345,
	UpdatedEventStore = similar_events:schedule_process(EventStore, Process, Timestamp),

	%% do the test
	SecondProcess = 45,
	UpdatedEventStoreWith2Process = similar_events:schedule_process(UpdatedEventStore, SecondProcess, Timestamp),
	etap:is(dict:is_key(Timestamp, UpdatedEventStoreWith2Process), true, "Timestamp should be a key"),
	UpdatedStoredEvent = dict:fetch(Timestamp, UpdatedEventStoreWith2Process),
	etap:is(UpdatedStoredEvent#sm_event.timestamp, Timestamp, "Stored event should have the same timestamp"),
	etap:is(UpdatedStoredEvent#sm_event.procs, [SecondProcess, Process], "2 processes now").
	
test_schedule_process_with_three_processes() ->
	%% create an event store and first schedule two processes at a different timestamp
	EventStore = similar_events:create_event_store(),
	Process = 15,
	Timestamp = 12345,
	UpdatedEventStore = similar_events:schedule_process(EventStore, Process, Timestamp),
	SecondProcess = 45,
	UpdatedEventStoreWith2Process = similar_events:schedule_process(UpdatedEventStore, SecondProcess, Timestamp),

	%% do the test
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
	etap:is(UnchangedStoredEvent#sm_event.procs, [SecondProcess, Process], "Still 2 processes").

is_dict(D) ->
	case catch dict:to_list(D) of
		L when is_list(L) ->
			true;
		{'EXIT', {badarg,_}} ->
			false
	end.

