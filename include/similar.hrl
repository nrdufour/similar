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

-define(LOG_EM, sm_evm_logger).

-define(VERSION, "0.01").

%% Scenario Process Description
%% Describe a process to start during a scenario
-record(pdesc,
	{
	%% Module
	mod,
	%% Function
	func,
	%% Arguments
	args,
	%% Time to activate it
	time = 0
	}
).

%% Scenario
-record(scenario,
	{
	%% Scenario name -- Informational
	name = "Unknown Scenario",
	%% List of pdesc structures: processes to use
	processes = [],
	%% List of resources -- TBD
	resources = [],
	%% Instant t0 -- can be greater
	time = 0,
	%% meta data -- dict TBD
	meta = []
	}
).

%% Simulation Data
-record(sm_data,
	{
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
	props = []
	}
).

%% Simulation Event
-record(sm_event,
	{
	%% Event timestamp
	timestamp = 0,
	%% Processes to wake up at that timestamp
	procs = []
	}
).

%%%
