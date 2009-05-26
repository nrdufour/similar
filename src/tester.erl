%%%-------------------------------------------------------------------
%%% File : filename
%%% Author : Nicolas R Dufour <nrdufour@gmail.com>
%%% Created : 2009/05/25
%%% Description :
%%%     Simple tester application
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(tester).
-export([start/0]).


start() ->
	%% Start the engine
	sm_engine:start_link(),

	%% Add a few processes
	sm_engine:new_P(foo, loop,[]),
	sm_engine:new_P(foo, loop,[]),
	sm_engine:new_P(foo, loop,[]),

	%% Display them
	io:format("Processes: ~p~n", [sm_engine:p()]),

	%% Saying good bye
	sm_engine:stop(),
	io:format("END~n").
