% This file is part of Similar released under the MIT license.
% See the LICENSE file for more information.

-module(similar_events).

-export([create_event/1, create_event/2, create_event_store/0, schedule_process/3, terminate_event/2, get_first_event/1, get_event/2]).

-include("similar.hrl").

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

