%%%-------------------------------------------------------------------
%%% File : similar.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/11
%%% Description :
%%%	Facade to access the main server
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(similar).
-behaviour(application).
-author('Nicolas R Dufour <nrdufour@gmail.com>').

%% Application API
-export([start/2, stop/1]).

%% User API
-export([start/0]).


%%====================================================================
%% API
%%====================================================================

start(_Type, StartArgs) ->
	Reply = case start_apps([sasl]) of
		ok ->
			similar_sup:start_link(StartArgs);
		{error, Reason} ->
			{error, Reason}
	end,
	gen_event:start({local, sm_msg_man}),
	gen_event:add_handler(sm_msg_man, similar_terminal_logger, []),
	similar_utils:log("Similar Engine is starting!", []),
	Reply.


%%--------------------------------------------------------------------
%% Function: start() -> {ok, Pid}
%% Description: Initiates the server
%%--------------------------------------------------------------------
start() ->
	application:start(similar).

stop(_State) ->
	similar_utils:log("Similar Engine is stopping!", []),
	similar_server:reset(),
	gen_event:stop(sm_msg_man),
	ok.

%%====================================================================


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Piece of code written by Benoît Chesneau <benoitc@e-engura.org>
%% @doc starts any dependencies.
start_apps([]) ->
	ok;
start_apps([App|Rest]) ->
	case application:start(App) of
		ok ->
			start_apps(Rest);
		{error, {already_started, App}} ->
			start_apps(Rest);
		{error, _Reason} ->
			{error, {app_would_not_start, App}}
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% END
	
