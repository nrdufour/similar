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

-module(similar_process).

-export([kill_sim_proc/1, new_P/2, process_terminate/2]).

-include("similar_data.hrl").

%% Kill a simulation process
kill_sim_proc(Pid) ->
	similar_utils:log("Killing process ~p now", [Pid]),
	exit(Pid, {process, terminated}).

%% Create a new simulation process at the current time (active)
new_P({Mod, Func, Args}, State) ->
	% Create the process first
	Pid = spawn_link(Mod, Func, Args),

	CurrentTime = State#sm_data.time,

	% Add it to process list
	NewProcesses = [Pid|State#sm_data.processes],

	% schedule it in the event list
	NewEvents = similar_events:schedule_process(State#sm_data.events, Pid, CurrentTime),

	% Add it in the active
	NewActives = [Pid|State#sm_data.actives],	

	State#sm_data{processes = NewProcesses, events = NewEvents, actives = NewActives}.

%% Clean the simulation state knowing that the given process is dead
process_terminate(Pid, State) ->
	% Remove it from the process list
	NewProcesses = lists:delete(Pid, State#sm_data.processes),

	% Remove it from the active list (if present)
	NewActives = lists:delete(Pid, State#sm_data.actives),

	% Remove it from the event list (if present)
	NewEvents = State#sm_data.events,

	State#sm_data{processes = NewProcesses, actives = NewActives, events = NewEvents}.

%% END
	
