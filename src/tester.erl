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
	similar:start_link(),

	%% Add a few processes
	similar:new_P(foo, loop,[]),
	similar:new_P(foo, loop,[]),
	similar:new_P(foo, loop,[]),

	%% Display them
	io:format("Processes: ~p~n", [similar:p()]),

	%% kill all processes
	similar:reset(),
	io:format("Processes after reset: ~p~n", [similar:p()]),

	%% Saying good bye
	similar:stop(),
	io:format("END~n").
