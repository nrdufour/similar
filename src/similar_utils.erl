%% Copyright 2009-2010 Nicolas R Dufour.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @author Nicolas R Dufour <nrdufour@gmail.com>
%% @copyright 2009-2010 Nicolas R Dufour.

-module(similar_utils).

-export([create_sim_state/0, trace_on/1, trace_off/1, kill_current/1, reset/1]).

-include("similar.hrl").

%% create an empty simulation state
create_sim_state() ->
	#sm_data{events = similar_events:create_event_store()}.

trace_on(State) ->
	State#sm_data{trace = true}.

trace_off(State) ->
	State#sm_data{trace = false}.

kill_current(State) ->
	State.

reset(State) ->
	lists:foreach(fun similar_process:kill_sim_proc/1, State#sm_data.processes),
	lists:foreach(fun kill_erlang_process/1, State#sm_data.resources),
	State#sm_data{events = similar_events:create_event_store(), resources = [], processes = [], actives = []}.

kill_erlang_process(Pid) ->
	similar_log:log_format("Killing process ~p now", [Pid]),
	exit(Pid, terminated).

%% END
