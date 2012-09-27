% This file is part of Similar released under the MIT license.
% See the LICENSE file for more information.

-module(similar_utils).

-export([create_sim_state/0, kill_current/1, reset/1]).

-include("similar.hrl").

%% create an empty simulation state
create_sim_state() ->
    #sm_data{events = similar_events:create_event_store()}.

kill_current(State) ->
    State.

reset(State) ->
    lists:foreach(fun similar_process:kill_sim_proc/1, State#sm_data.processes),
    lists:foreach(fun kill_erlang_process/1, State#sm_data.resources),
    State#sm_data{events = similar_events:create_event_store(), resources = [], processes = [], actives = []}.

kill_erlang_process(Pid) ->
    similar_log:info("Killing process ~p now", [Pid]),
    exit(Pid, terminated).

%% END
