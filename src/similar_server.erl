%%%-------------------------------------------------------------------
%%% File : similar_server.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/05/19
%%% Description :
%%%	Main process steering the simulation.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(similar_server).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-compile(export_all).

-include("similar_data.hrl").

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%% {ok, State, Timeout} |
%% ignore |
%% {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Args) ->
	process_flag(trap_exit, true),
	InitialState = #sm_data{ events = events:create_event_store() },
	{ok, InitialState}.

%%--------------------------------------------------------------------
%% Function:
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(event_time, _From, State) ->
	{reply, similar_process:event_time(State), State};

handle_call({new_P, Mod, Func, Args}, _From, State) ->
	{reply, ok, similar_process:new_P({Mod, Func, Args}, State)};

handle_call({trace, on}, _From, State) ->
	{reply, ok, similar_utils:trace_on(State)};

handle_call({trace, off}, _From, State) ->
	{reply, ok, similar_utils:trace_off(State)};

handle_call({debug, r}, _From, State) ->
	{reply, similar_query:r(State), State};

handle_call({debug, p}, _From, State) ->
	{reply, similar_query:p(State), State};

handle_call({debug, e}, _From, State) ->
	{reply, similar_query:e(State), State};

handle_call({debug, s}, _From, State) ->
	{reply, similar_query:s(State), State};

handle_call({debug, c}, _From, State) ->
	{reply, similar_query:c(State), State};

handle_call(stop, _From, State) ->
	{stop, normal, State};

handle_call(kill_current, _From, State) ->
	{reply, not_yet_implemented, similar_utils:kill_current(State)};

handle_call(reset, _From, State) ->
	{reply, ok, similar_utils:reset(State)};

handle_call({kill_pid, Pid}, _From, State) ->
	similar_process:kill_sim_proc(Pid),
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
	case Reason of
		{process, _} ->
			NewState = similar_process:process_terminate(Pid, State),
			{noreply, NewState};
		_ ->
			io:format("Received an unknown EXIT signal from ~p with Reason ~p~n", [Pid, Reason]),
			{noreply, State}
	end.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(normal, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% END
	
