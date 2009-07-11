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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-compile(export_all).

-include("similar_data.hrl").


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_event:start({local, sm_msg_man}),

	gen_event:add_handler(sm_msg_man, terminal_logger, []),

	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop),
	gen_event:stop(sm_msg_man).

%%====================================================================

kill_current() ->
	gen_server:call(?MODULE, kill_current).

reset() ->
	gen_server:call(?MODULE, reset).

kill(Pid) ->
	gen_server:call(?MODULE, {kill, Pid}). 

%%====================================================================

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

%%====================================================================

event_time() ->
	gen_server:call(?MODULE, event_time).

trace() ->
	gen_server:call(?MODULE, {trace, on}).

no_trace() ->
	gen_server:call(?MODULE, {trace, off}).

%%====================================================================

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
	process_flag(trap_exit, true),
	log("~p starting" ,[?MODULE]),
	{ok, #sm_data{}}.

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
	{reply, State#sm_data.time, State};

handle_call({new_P, Mod, Func, Args}, _From, State) ->
	log("Creating new Process from module ~p", [Mod]),
	Pid = spawn_link(Mod, Func, Args),
	log("Adding new process ~p", [Pid]),

	NewProcesses = [Pid|State#sm_data.processes],
	NewState = State#sm_data{processes = NewProcesses},
	{reply, Pid, NewState};

handle_call({trace, on}, _From, State) ->
	NewState = State#sm_data{trace = true},
	{reply, ok, NewState};

handle_call({trace, off}, _From, State) ->
	NewState = State#sm_data{trace = false},
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
	lists:foreach(fun internal_kill/1, State#sm_data.processes),
	lists:foreach(fun internal_kill/1, State#sm_data.resources),
	NewState = State#sm_data{events = [], resources = [], processes = [], actives = []},
	{reply, ok, NewState};

handle_call({kill, Pid}, _From, State) ->
	exit(Pid, terminated),
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
	log("~p is dead. Reason:\t~p", [Pid, Reason]),
	Processes = State#sm_data.processes,
	IsAProcess = lists:member(Pid, Processes),
	if
		IsAProcess ->
			NewProcesses = lists:delete(Pid, Processes),
			NewState = State#sm_data{processes = NewProcesses},
			{noreply, NewState};
		true ->
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
	log("~p stopping" ,[?MODULE]),
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

internal_kill(Pid) ->
	log("Killing process ~p now", [Pid]),
	exit(Pid, terminated).

log(Format, Args) ->
	SF1 = string:concat("\t", Format),
	SF2 = string:concat(SF1, "\n"),
	Msg = io_lib:format(SF2, Args),
	gen_event:notify(sm_msg_man, Msg).

%% END
	
