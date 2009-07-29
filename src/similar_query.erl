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
-compile(export_all).

-include("similar_data.hrl").

r(State) ->
	{reply, State#sm_data.resources, State}.

p(State) ->
	{reply, State#sm_data.processes, State}.

e(State) ->
	{reply, State#sm_data.events, State}.

s(State) ->
	{reply, State#sm_data.props, State}.

c(State) ->
	{reply, State#sm_data.actives, State}.

%% END
	
