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

-export([create/3, terminate/1, fall_asleep/1]).

%% create a new simulation process and returns its pid
create(Mod, Func, Args) ->
	similar_utils:log("Starting process ~p:~p now", [Mod, Func]),
	spawn_link(Mod, Func, Args).

%% Kill a simulation process
terminate(Pid) ->
	similar_utils:log("Killing process ~p now", [Pid]),
	exit(Pid, {process, terminated}).

fall_asleep(Pid) ->
	receive
		wake_up -> similar_utils:log("Pid ~p is waking up!", [Pid])
	end.

%% END
	
