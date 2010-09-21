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

-module(similar_query).

-export([r/1, p/1, e/1, s/1, c/1, event_time/1]).

-include("similar.hrl").

r(State) ->
	State#sm_data.resources.

p(State) ->
	State#sm_data.processes.

e(State) ->
	State#sm_data.events.

s(State) ->
	State#sm_data.props.

c(State) ->
	State#sm_data.actives.

event_time(State) ->
	State#sm_data.time.

%% END
	
