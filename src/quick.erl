%%%-------------------------------------------------------------------
%%% File : quick.erl
%%% Author : Nicolas R Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/30
%%% Description :
%%%     Simple process just saying hello and dieing.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(quick).

-export([start/0]).

start() ->
	io:format("Hello I'm ~p and going to die soon!~n", [self()]).

%%% end
