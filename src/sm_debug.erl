%%%-------------------------------------------------------------------
%%% File : sm_debug.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/05/25
%%% Description :
%%%	Debug callbacks.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------
-module(sm_debug).
-author("Nicolas R Dufour").

-export([r/1,p/1,e/1,s/1,c/1]).

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

