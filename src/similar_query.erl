% This file is part of Similar released under the MIT license.
% See the LICENSE file for more information.

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
    
