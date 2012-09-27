% This file is part of Similar released under the MIT license.
% See the LICENSE file for more information.

-module(similar_sup).
-behaviour(supervisor).

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

