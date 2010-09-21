#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

-include_lib("./include/similar.hrl").

main(_) ->
	etap:plan(unknown),

	etap:loaded_ok(similar_events, "Module 'similar_events' loaded."),
	etap:can_ok(similar_events, create_event, 1),
	etap:can_ok(similar_events, create_event, 2),
	etap:can_ok(similar_events, create_event_store, 0),
	etap:can_ok(similar_events, schedule_process, 3),
	etap:can_ok(similar_events, terminate_event, 2),
	etap:can_ok(similar_events, get_first_event, 1),
	etap:can_ok(similar_events, get_event, 2),

	test_create_event(),
	test_create_event_store(),
	test_schedule_process_with_one_process(),
	test_schedule_process_with_two_processes(),
	test_schedule_process_with_three_processes(),
	test_terminate_event(),
	test_get_first_event(),

	etap:end_tests(),
	ok.

test_create_event() ->
	Timestamp = 12345,
	Event = similar_events:create_event(Timestamp),
	etap:is(Event#sm_event.timestamp, Timestamp, "Event should have the right timestamp"),

	Procs = [ 1, 2, 3 ],
	EventWithProcs = similar_events:create_event(Timestamp, Procs),
	etap:is(EventWithProcs#sm_event.timestamp, Timestamp, "Event should have the right timestamp"),
	etap:is(EventWithProcs#sm_event.procs, Procs, "Event should have the right procs"),
	ok.

test_create_event_store() ->
	EventStore = similar_events:create_event_store(),
	etap:is(is_dict(EventStore), true, "An event store should be a dict"),
	ok.

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
	etap:is(Event#sm_event.procs, [Process], "Stored event should have the proc in the list"),
	ok.

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
	etap:is(UpdatedStoredEvent#sm_event.procs, [SecondProcess, Process], "2 processes now"),
	ok.
	
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
	etap:is(UnchangedStoredEvent#sm_event.procs, [SecondProcess, Process], "Still 2 processes"),
	ok.

test_terminate_event() ->
	First = 123,
	Second = 456,
	Third = 789,
	EventStore = similar_events:create_event_store(),

	EV1 = similar_events:schedule_process(EventStore, First, First),
	EV2 = similar_events:schedule_process(EV1, Second, Second),
	EV3 = similar_events:schedule_process(EV2, Third, Third),

	etap:is(dict:is_key(First, EV3), true, "First key"),
	etap:is(dict:is_key(Second, EV3), true, "Second key"),
	etap:is(dict:is_key(Third, EV3), true, "Third key"),

	EV4 = similar_events:terminate_event(EV3, First),
	etap:is(dict:is_key(First, EV4), false, "First key should be gone"),
	etap:is(dict:is_key(Second, EV4), true, "Second key should still be there"),
	etap:is(dict:is_key(Third, EV4), true, "Third key should still be there"),

	EV5 = similar_events:terminate_event(EV4, Third),
	etap:is(dict:is_key(First, EV5), false, "First key should be gone"),
	etap:is(dict:is_key(Second, EV5), true, "Second key should still be there"),
	etap:is(dict:is_key(Third, EV5), false, "Third should be gone"),

	EV6 = similar_events:terminate_event(EV5, Second),
	etap:is(dict:is_key(First, EV6), false, "First key should be gone"),
	etap:is(dict:is_key(Second, EV6), false, "Second key should be done too"),
	etap:is(dict:is_key(Third, EV6), false, "Third key should be gone too"),
	ok.

test_get_first_event() ->
	EV = similar_events:create_event_store(),

	NilEvent = similar_events:get_first_event(EV),
	etap:is(NilEvent, nil, "An empty event store should return a nil first event"),

	%% adding a process
	First = 123,
	EV2 = similar_events:schedule_process(EV, First, First),

	FirstEvent = similar_events:get_first_event(EV2),
	etap:is(FirstEvent#sm_event.timestamp, First, "FirstEvent timestamp should be 123"),
	etap:is(FirstEvent#sm_event.procs, [First], "FirstEvent procs should be [123]"),

	%% adding a process later
	Second = 456,
	EV3 = similar_events:schedule_process(EV2, Second, Second),
	StillFirstEvent = similar_events:get_first_event(EV3),
	etap:is(StillFirstEvent#sm_event.timestamp, First, "FirstEvent timestamp should still be 123"),
	etap:is(StillFirstEvent#sm_event.procs, [First], "FirstEvent procs should still be [123]"),

	%% add a process before
	Before = 2,
	EV4 = similar_events:schedule_process(EV3, Before, Before),
	BeforeEvent = similar_events:get_first_event(EV4),
	etap:is(BeforeEvent#sm_event.timestamp, Before, "FirstEvent timestamp should now be 2"),
	etap:is(BeforeEvent#sm_event.procs, [Before], "FirstEvent procs should now be [2]"),
	ok.

is_dict(D) ->
	case catch dict:to_list(D) of
		L when is_list(L) ->
			true;
		{'EXIT', {badarg,_}} ->
			false
	end.

