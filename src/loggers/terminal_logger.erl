%%%-------------------------------------------------------------------
%%% File : terminal_logger.erl
%%% Author : Nicolas R Dufour <nrdufour@gmail.com>
%%% Created : 2009/06/01
%%% Description :
%%%     Simple terminal logger from the erlang documentation.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------
-module(terminal_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_cast/2, handle_info/2, code_change/3]).

%% trace is enabled by default
init(_Args) ->
	{ok, true}.

%% activate or not the trace
handle_event({trace, Trace}, _State) ->
	{ok, Trace};

%% display a simple message
handle_event(Msg, State) ->
	if
		State ->
			io:format(Msg);
		true ->
			true
	end,
	{ok, State}.

handle_call(_, State) ->
	{noreply, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Args, _State) ->
	ok.

%%% END
