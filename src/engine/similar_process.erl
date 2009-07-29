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

-export([new_P/3]).

-include("similar_data.hrl").

%% Create a new simulation process at the current time (active)
new_P({Mod, Func, Args}, _From, State) ->
	% Create the process first
	Pid = spawn_link(Mod, Func, Args),

	CurrentTime = State#sm_data.time,

	% Add it to process list
	NewProcesses = [Pid|State#sm_data.processes],

	% schedule it in the event list
	NewEvents = events:schedule_process(State#sm_data.events, Pid, CurrentTime),

	% Add it in the active
	NewActives = [Pid|State#sm_data.actives],	

	% Prepare the new state
	NewState = State#sm_data{processes = NewProcesses, events = NewEvents, actives = NewActives},

	{reply, Pid, NewState}.

%% END
	
