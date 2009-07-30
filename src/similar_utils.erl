%%%-------------------------------------------------------------------
%%% File : similar_process.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/29
%%% Description :
%%%	Process related functions used by the main server
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(similar_process).

-export([trace_on/1, trace_off/1, kill_current/1, reset/1]).

-include("similar_data.hrl").

trace_on(State) ->
	State#sm_data{trace = true}.

trace_off(State) ->
	State#sm_data{trace = false}.

kill_current(State) ->
	State.

reset(State) ->
	lists:foreach(fun similar_process:kill_sim_proc/1, State#sm_data.processes),
	lists:foreach(fun utils:kill_erlang_process/1, State#sm_data.resources),
	State#sm_data{events = [], resources = [], processes = [], actives = []}.

%% END
	