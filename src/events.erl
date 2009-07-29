%%%-------------------------------------------------------------------
%%% File : events.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/25
%%% Description :
%%%	Provide function to manipulate events
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(events).

-export([create_event/1, create_event/2, create_event_store/0, schedule_process/3, terminate_event/2, get_first_event/1, get_event/2]).

-include("similar_data.hrl").

create_event(Timestamp) ->
	#sm_event{timestamp = Timestamp}.

create_event(Timestamp, Procs) ->
	#sm_event{timestamp = Timestamp, procs = Procs}.

%% Event Store

create_event_store() ->
	dict:new().

schedule_process(EventStore, Process, Time) ->
	CurrentEvent = get_event(EventStore, Time),
	if
		CurrentEvent == nil ->
			NewEvent = create_event(Time, [Process]),
			dict:store(Time, NewEvent, EventStore);
		true ->
			UpdatedEvent = CurrentEvent#sm_event{procs = [Process|CurrentEvent#sm_event.procs]},
			dict:store(Time, UpdatedEvent, EventStore)
	end.

terminate_event(EventStore, Timestamp) ->
	HasTimestamp = dict:is_key(Timestamp, EventStore),
	if
		HasTimestamp == true -> dict:erase(Timestamp, EventStore);
		true -> EventStore
	end.

get_first_event(EventStore) ->
	Size = dict:size(EventStore),
	if
		Size == 0 -> nil;
		true ->
			[First|_Tail] = lists:sort(dict:fetch_keys(EventStore)),
			dict:fetch(First, EventStore)
	end.

get_event(EventStore, Timestamp) ->
	HasTimestamp = dict:is_key(Timestamp, EventStore),
	if
		HasTimestamp == true -> dict:fetch(Timestamp, EventStore);
		true -> nil
	end.
	

%% END

