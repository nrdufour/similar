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

-module(similar_sup).
-behaviour(supervisor).
-author('Nicolas R Dufour <nrdufour@gmail.com>').

-export([start_link/1, init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    %% Install my personal error handler
    %%gen_event:swap_handler(alarm_handler,
    %%            {alarm_handler, swap},
    %%            {my_alarm_handler, xyz}),

    SimilarConfig = {similar_config,
        {similar_config, start_link, []},
        permanent,
	10000,
	worker,
	[similar_config]
    },

    SimilarScenario = {similar_scenario,
        {similar_scenario, start_link, []},
        permanent,
	10000,
	worker,
	[similar_scenario]
    },

    SimilarEngine = {similar_engine,
        {similar_engine, start_link, []},
        permanent,
	10000,
	worker,
	[similar_engine]
    },

    Strategies = {
        {one_for_one, 10, 3600},
	[SimilarConfig, SimilarEngine, SimilarScenario]
    },
    
    {ok, Strategies}.

