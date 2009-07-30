%%%-------------------------------------------------------------------
%%% File : similar_process.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/29
%%% Description :
%%%	Process related functions used by the main server
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(similar_process).

-export([kill_sim_proc/1, new_P/2, process_terminate/2]).

-include("similar_data.hrl").

%% Kill a simulation process
kill_sim_proc(Pid) ->
	loggers:log("Killing process ~p now", [Pid]),
	exit(Pid, {process, terminated}).

%% Create a new simulation process at the current time (active)
new_P({Mod, Func, Args}, State) ->
	% Create the process first
	Pid = spawn_link(Mod, Func, Args),

	CurrentTime = State#sm_data.time,

	% Add it to process list
	NewProcesses = [Pid|State#sm_data.processes],

	% schedule it in the event list
	NewEvents = events:schedule_process(State#sm_data.events, Pid, CurrentTime),

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
	
