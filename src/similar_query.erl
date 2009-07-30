%%%-------------------------------------------------------------------
%%% File : similar_query.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/29
%%% Description :
%%%	Query functions
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(similar_query).

-export([r/1, p/1, e/1, s/1, c/1, event_time/1]).

-include("similar_data.hrl").

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
	
