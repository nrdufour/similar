%%%-------------------------------------------------------------------
%%% File : file_logger.erl
%%% Author : Nicolas R Dufour <nrdufour@gmail.com>
%%% Created : 2009/06/03
%%% Description :
%%%     Log a message in a file.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------
-module(file_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).

init(File) ->
	{ok, Fd} = file:open(File, read),
	State = {true, Fd},
	{ok, State}.

handle_event({trace, Trace}, {_Trace, Fd}) ->
	{ok, {Trace, Fd}};

handle_event({Msg, Args}, {Trace, Fd}) ->
	if
		Trace ->
			Format = string:concat(Msg, "~n"),
			io:format(Fd, Format, Args);
		true ->
			true
	end,
	{ok, {Trace, Fd}}.

terminate(_Args, {_Trace, Fd}) ->
	file:close(Fd).

%% END
