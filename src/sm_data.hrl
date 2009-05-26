%%%-------------------------------------------------------------------
%%% File : sim_data.hrl
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

-record(sim_data,
	{

	%%% {EvTree; R;E;Current; RealTime; SList; Trace}

	events = [],
	resources = [],
	processes = [],
	actives = [],
	time,
	props = [],
	trace = false
	
	}
).

%%%
