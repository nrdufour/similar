% This file is part of Similar released under the MIT license.
% See the LICENSE file for more information.

-define(LOG_EM, sm_evm_logger).

-define(VERSION, "0.01").

-define(DEFAULT_ETC, "/tmp").

%% Scenario Process Descriptor
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
