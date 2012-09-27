% This file is part of Similar released under the MIT license.
% See the LICENSE file for more information.

-module(similar_process).

-export([create/3, execute/3, terminate/1, fall_asleep/1]).

%% create a new simulation process and returns its pid
create(Mod, Func, Args) ->
    spawn_link(similar_process, execute, [Mod, Func, Args]).

execute(Mod, Func, Args) ->
    similar_log:info("Starting process ~p:~p now", [Mod, Func]),
    similar_process:fall_asleep(self()),
    %% The execution ----
    Result = apply(Mod, Func, Args),
    %% ------------------
    similar_log:info("Ending process ~p:~p now with result: ~p", [Mod, Func, Result]),
    ok.

%% Kill a simulation process
terminate(Pid) ->
    similar_log:info("Killing process ~p now", [Pid]),
    exit(Pid, {process, terminated}).

fall_asleep(Pid) ->
    receive
        wake_up -> similar_log:info("Pid ~p is waking up!", [Pid])
    end,
    ok.

%% END
    
