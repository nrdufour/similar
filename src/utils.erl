%%%-------------------------------------------------------------------
%%% File : utils.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/11
%%% Description :
%%%	Utility functions.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(utils).

-export([kill_erlang_process/1]).

kill_erlang_process(Pid) ->
	loggers:log("Killing process ~p now", [Pid]),
	exit(Pid, terminated).

%% END
	
