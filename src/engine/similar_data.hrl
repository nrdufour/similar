%%%-------------------------------------------------------------------
%%% File : similar_data.hrl
%%% Author : Nicolas R Dufour <nrdufour@gmail.com>
%%% Created : 2009/05/19
%%% Description :
%%%     Basic record containing the simulation data.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-record(sm_data,
	{

	%%% {EvTree; R;E;Current; RealTime; SList; Trace}

	%% Events to be scheduled
	events = [],
	%% Resources list
	resources = [],
	%% Alive processes in the simulation
	processes = [],
	%% Active processes in the simulation
	actives = [],
	%% Simulation time
	time = 0,
	%% Properties
	props = [],
	%% Activate the traces or not
	trace = false
	
	}
).

-record(sm_event,
	{
		%% Event timestamp
		timestamp = 0,
		%% Processes to wake up at that timestamp
		procs = []
	}
).

%%%
