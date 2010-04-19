%%%-------------------------------------------------------------------
%%% File : similar_process.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/29
%%% Description :
%%%	Utility functions related to the simulation engine.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(similar_utils).

-export([create_sim_state/0, trace_on/1, trace_off/1, kill_current/1, reset/1, log/2, format_time/1]).

-include("similar_data.hrl").

create_sim_state() ->
	#sm_data{events = similar_events:create_event_store()}.

trace_on(State) ->
	State#sm_data{trace = true}.

trace_off(State) ->
	State#sm_data{trace = false}.

kill_current(State) ->
	State.

reset(State) ->
	lists:foreach(fun similar_process:kill_sim_proc/1, State#sm_data.processes),
	lists:foreach(fun kill_erlang_process/1, State#sm_data.resources),
	State#sm_data{events = similar_events:create_event_store(), resources = [], processes = [], actives = []}.

kill_erlang_process(Pid) ->
	log("Killing process ~p now", [Pid]),
	exit(Pid, terminated).

log(Format, Args) ->
	Msg = io_lib:format(Format, Args),
	{_Date, Time} = calendar:local_time(),
	FormattedTime = format_time(Time),
	Final = io_lib:format("[~s] -- ~s ~n", [FormattedTime, Msg]),
        gen_event:notify(sm_msg_man, Final).

format_time({Hour, Minute, Second}) ->
	io_lib:format("~2..0w:~2..0w:~2..0w", [Hour, Minute, Second]).

%% END
