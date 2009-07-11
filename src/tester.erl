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
	similar_server:start_link(),

	%% Add a few processes
	similar_server:new_P(foo, loop,[]),
	similar_server:new_P(foo, loop,[]),
	similar_server:new_P(foo, loop,[]),

	%% Display them
	io:format("Processes: ~p~n", [similar_server:p()]),

	%% kill all processes
	similar_server:reset(),
	io:format("Processes after reset: ~p~n", [similar_server:p()]),

	%% Saying good bye
	similar_server:stop(),
	io:format("END~n").
