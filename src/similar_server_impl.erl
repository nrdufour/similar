%%%-------------------------------------------------------------------
%%% File : similar_server_impl.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/11
%%% Description :
%%%	Main server implementation.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(similar_server_impl).
-compile(export_all).

-include("similar_data.hrl").

initialize_server(_Args) ->
	process_flag(trap_exit, true),
	{ok, #sm_data{}}.

event_time(_From, State) ->
	{reply, State#sm_data.time, State}.

new_P({Mod, Func, Args}, _From, State) ->
	Pid = spawn_link(Mod, Func, Args),

	NewProcesses = [Pid|State#sm_data.processes],
	NewState = State#sm_data{processes = NewProcesses},
	{reply, Pid, NewState}.

trace_on(_From, State) ->
	NewState = State#sm_data{trace = true},
	{reply, ok, NewState}.

trace_off(_From, State) ->
	NewState = State#sm_data{trace = false},
	{reply, ok, NewState}.

kill_current(_From, State) ->
	{reply, ok, State}.

reset(_From, State) ->
	lists:foreach(fun utils:internal_kill/1, State#sm_data.processes),
	lists:foreach(fun utils:internal_kill/1, State#sm_data.resources),
	NewState = State#sm_data{events = [], resources = [], processes = [], actives = []},
	{reply, ok, NewState}.

kill_simulation_process(Pid, _From, State) ->
	exit(Pid, terminated),
	{reply, ok, State}.

receiving_exit_from_P(Pid, Reason, State) ->
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

%% END
	
