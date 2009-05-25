%%%-------------------------------------------------------------------
%%% File : sm_engine.erl
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

-module(sm_engine).
-behaviour(gen_server).

%% API
-export([start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-compile(export_all).

-include("sm_data.hrl").


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

kill_current() ->
	gen_server:call(?MODULE, kill_current).

reset() ->
	gen_server:call(?MODULE, reset).

kill(Pid) ->
	gen_server:call(?MODULE, {kill, Pid}). 

%%% DEBUG

%%% TODO change that from r to {debug, r}
r() ->
	gen_server:call(?MODULE, {debug, r}).

p() ->
	gen_server:call(?MODULE, {debug, p}).

e() ->
	gen_server:call(?MODULE, {debug, e}).

s() ->
	gen_server:call(?MODULE, {debug, s}).

c() ->
	gen_server:call(?MODULE, {debug, c}).

trace() ->
	gen_server:call(?MODULE, {trace, on}).

no_trace() ->
	gen_server:call(?MODULE, {trace, off}).

%%% PROCESSES

new_P(Mod, Func, Args) ->
	gen_server:call(?MODULE, {new_P, Mod, Func, Args}).


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
init([]) ->
	{ok, #sim_data{}}.

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
handle_call({new_P, Mod, Func, Args}, _From, State) ->
	io:format("Creating new Process from module ~p~n", [Mod]),
	Pid = spawn_link(Mod, Func, Args),
	io:format("Adding new process ~p~n", [Pid]),

	NewProcesses = [Pid|State#sim_data.processes],
	NewState = State#sim_data{processes = NewProcesses},
	{reply, Pid, NewState};

handle_call({trace, on}, _From, State) ->
	NewState = State#sim_data{trace = true},
	{reply, ok, NewState};

handle_call({trace, off}, _From, State) ->
	NewState = State#sim_data{trace = false},
	{reply, ok, NewState};

handle_call({debug, r}, _From, State) ->
	sm_debug:r(State);

handle_call({debug, p}, _From, State) ->
	sm_debug:p(State);

handle_call({debug, e}, _From, State) ->
	sm_debug:e(State);

handle_call({debug, s}, _From, State) ->
	sm_debug:s(State);

handle_call({debug, c}, _From, State) ->
	sm_debug:c(State);

handle_call(stop, _From, State) ->
	{stop, normal, State};

handle_call(kill_current, _From, State) ->
	{reply, ok, State};

handle_call(reset, _From, State) ->
	{reply, ok, State};

handle_call({kill, _Pid}, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

