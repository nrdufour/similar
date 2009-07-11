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

start_link() ->
	gen_event:start({local, sm_msg_man}),

	gen_event:add_handler(sm_msg_man, terminal_logger, []),

	gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

stop() ->
	gen_server:cast(?SERVER, stop),
	gen_event:stop(sm_msg_man).

%%====================================================================

kill_current() ->
	gen_server:call(?SERVER, kill_current).

reset() ->
	gen_server:call(?SERVER, reset).

kill(Pid) ->
	gen_server:call(?SERVER, {kill, Pid}). 

%%====================================================================

r() ->
	gen_server:call(?SERVER, {debug, r}).

p() ->
	gen_server:call(?SERVER, {debug, p}).

e() ->
	gen_server:call(?SERVER, {debug, e}).

s() ->
	gen_server:call(?SERVER, {debug, s}).

c() ->
	gen_server:call(?SERVER, {debug, c}).

%%====================================================================

event_time() ->
	gen_server:call(?SERVER, event_time).

trace() ->
	gen_server:call(?SERVER, {trace, on}).

no_trace() ->
	gen_server:call(?SERVER, {trace, off}).

%%====================================================================

new_P(Mod, Func, Args) ->
	gen_server:call(?SERVER, {new_P, Mod, Func, Args}).


%% END
	
