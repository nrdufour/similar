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

-compile(export_all).

-define(SERVER, similar_server).


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start() -> {ok, Pid}
%% Description: Initiates the server
%%--------------------------------------------------------------------
start() ->
	gen_event:start({local, sm_msg_man}),
	gen_event:add_handler(sm_msg_man, terminal_logger, []),
	gen_server:start_link({local, ?SERVER}, ?SERVER, [], []),
	loggers:log("Similar Engine is starting!", []).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%% Description: Stops the server (kills all processes/resources).
%%--------------------------------------------------------------------
stop() ->
	loggers:log("Similar Engine is stopping!", []),
	reset(),
	gen_server:cast(?SERVER, stop),
	gen_event:stop(sm_msg_man).

%%====================================================================

%%--------------------------------------------------------------------
%% Function: kill_current() -> ok
%% Description: Kills all active processes.
%%--------------------------------------------------------------------
kill_current() ->
	gen_server:call(?SERVER, kill_current).

%%--------------------------------------------------------------------
%% Function: reset() -> ok
%% Description: Kills every processes and resources.
%%--------------------------------------------------------------------
reset() ->
	gen_server:call(?SERVER, reset).

%%--------------------------------------------------------------------
%% Function: kill_pid(Pid) -> ok
%% Description: Kills the process/resource managed by the server.
%%--------------------------------------------------------------------
kill_pid(Pid) ->
	gen_server:call(?SERVER, {kill_pid, Pid}). 

%%====================================================================

%%--------------------------------------------------------------------
%% Function: r() -> list()
%% Description: Returns the resources list.
%%--------------------------------------------------------------------
r() ->
	gen_server:call(?SERVER, {debug, r}).

%%--------------------------------------------------------------------
%% Function: p() -> list()
%% Description: Returns the processes list.
%%--------------------------------------------------------------------
p() ->
	gen_server:call(?SERVER, {debug, p}).

%%--------------------------------------------------------------------
%% Function: e() -> list()
%% Description: Returns the events list.
%%--------------------------------------------------------------------
e() ->
	gen_server:call(?SERVER, {debug, e}).

%%--------------------------------------------------------------------
%% Function: s() -> list()
%% Description: Returns the properties list.
%%--------------------------------------------------------------------
s() ->
	gen_server:call(?SERVER, {debug, s}).

%%--------------------------------------------------------------------
%% Function: c() -> list()
%% Description: Returns the active processes list.
%%--------------------------------------------------------------------
c() ->
	gen_server:call(?SERVER, {debug, c}).

%%====================================================================

%%--------------------------------------------------------------------
%% Function: event_time() -> int()
%% Description: Returns the current simulation time.
%%--------------------------------------------------------------------
event_time() ->
	gen_server:call(?SERVER, event_time).

%%--------------------------------------------------------------------
%% Function: trace(boolean) -> ok
%% Description: Activate/Deactive the traces
%%--------------------------------------------------------------------
trace(true)  -> gen_server:call(?SERVER, {trace, on});

trace(false) -> gen_server:call(?SERVER, {trace, off}).

%%====================================================================

%%--------------------------------------------------------------------
%% Function: new_P(Mod, Func, Args) -> {ok, Pid}
%% Description: Initiates a new process based on the MFA arguments.
%%--------------------------------------------------------------------
new_P(Mod, Func, Args) ->
	gen_server:call(?SERVER, {new_P, Mod, Func, Args}).

%%--------------------------------------------------------------------
%% Function: hold(Time) -> ok
%% Description: Holds the calling process at the given time
%%--------------------------------------------------------------------
hold(Time) ->
	gen_server:call(?SERVER, {hold, Time, self()}),
	receive
		startagain ->
			true
	end.

%% END
	
