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
	sm_eng:start(),

	%% Add a few processes
	sm_eng:new_P(foo, loop,[]),
	sm_eng:new_P(foo, loop,[]),
	sm_eng:new_P(foo, loop,[]),

	%% Display them
	io:format("Processes: ~p~n", [sm_eng:p()]),

	%% Saying good bye
	io:format("END~n").
