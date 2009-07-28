%%%-------------------------------------------------------------------
%%% File : dummy_process.erl
%%% Author : Nicolas R Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/14
%%% Description :
%%%     Simple process just saying hello and dieing.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(simple).

-export([start/0]).

start() ->
	loop().

loop() ->
	receive
		start ->
			io:format("Hello I'm ~p and going to die soon!~n", [self()]),
			loop();
		Other ->
			io:format("Received an unknown message [~p]~n", [Other]),
			loop()
	end.

%%% end
