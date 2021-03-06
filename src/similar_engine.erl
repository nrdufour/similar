% This file is part of Similar released under the MIT license.
% See the LICENSE file for more information.

-module(similar_engine).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-compile(export_all).

-include("similar.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    InitialState = #sm_data{ events = similar_events:create_event_store() },
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
    {reply, similar_query:event_time(State), State};

handle_call({new_P, Mod, Func, Args}, _From, State) ->
    {reply, ok, new_P({Mod, Func, Args}, State)};

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
    similar_process:terminate(Pid),
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
            NewState = clean_dead_P(Pid, State),
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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================

%%--------------------------------------------------------------------
%% Function: kill_current() -> ok
%% Description: Kills all active processes.
%%--------------------------------------------------------------------
kill_current() ->
    gen_server:call(?MODULE, kill_current).

%%--------------------------------------------------------------------
%% Function: reset() -> ok
%% Description: Kills every processes and resources.
%%--------------------------------------------------------------------
reset() ->
    gen_server:call(?MODULE, reset).

%%--------------------------------------------------------------------
%% Function: kill_pid(Pid) -> ok
%% Description: Kills the process/resource managed by the server.
%%--------------------------------------------------------------------
kill_pid(Pid) ->
    gen_server:call(?MODULE, {kill_pid, Pid}). 

%%====================================================================

%%--------------------------------------------------------------------
%% Function: r() -> list()
%% Description: Returns the resources list.
%%--------------------------------------------------------------------
r() ->
    gen_server:call(?MODULE, {debug, r}).

%%--------------------------------------------------------------------
%% Function: p() -> list()
%% Description: Returns the processes list.
%%--------------------------------------------------------------------
p() ->
    gen_server:call(?MODULE, {debug, p}).

%%--------------------------------------------------------------------
%% Function: e() -> list()
%% Description: Returns the events list.
%%--------------------------------------------------------------------
e() ->
    gen_server:call(?MODULE, {debug, e}).

%%--------------------------------------------------------------------
%% Function: s() -> list()
%% Description: Returns the properties list.
%%--------------------------------------------------------------------
s() ->
    gen_server:call(?MODULE, {debug, s}).

%%--------------------------------------------------------------------
%% Function: c() -> list()
%% Description: Returns the active processes list.
%%--------------------------------------------------------------------
c() ->
    gen_server:call(?MODULE, {debug, c}).

%%====================================================================

%%--------------------------------------------------------------------
%% Function: event_time() -> int()
%% Description: Returns the current simulation time.
%%--------------------------------------------------------------------
event_time() ->
    gen_server:call(?MODULE, event_time).

%%====================================================================

%%--------------------------------------------------------------------
%% Function: new_P(Mod, Func, Args) -> {ok, Pid}
%% Description: Initiates a new process based on the MFA arguments.
%%--------------------------------------------------------------------
new_P(Mod, Func, Args) ->
    gen_server:call(?MODULE, {new_P, Mod, Func, Args}).

%%--------------------------------------------------------------------
%% Function: hold(Time) -> ok
%% Description: Holds the calling process at the given time
%%--------------------------------------------------------------------
hold(Time) ->
    gen_server:call(?MODULE, {hold, Time, self()}),
    receive
        startagain ->
            true
    end.

%%--------------------------------------------------------------------
%% Internal API
%%--------------------------------------------------------------------

%% Create a new simulation process at the current time (active)
new_P({Mod, Func, Args}, State) ->
    % Create the process first
    Pid = similar_process:create(Mod, Func, Args),

    CurrentTime = State#sm_data.time,

    % Add it to process list
    NewProcesses = [Pid|State#sm_data.processes],

    % schedule it in the event list
    NewEvents = similar_events:schedule_process(State#sm_data.events, Pid, CurrentTime),

    % Add it in the active
    NewActives = [Pid|State#sm_data.actives],    

    State#sm_data{processes = NewProcesses, events = NewEvents, actives = NewActives}.

%% Clean the simulation state knowing that the given process is dead
clean_dead_P(Pid, State) ->
    % Remove it from the process list
    NewProcesses = lists:delete(Pid, State#sm_data.processes),

    % Remove it from the active list (if present)
    NewActives = lists:delete(Pid, State#sm_data.actives),

    % Remove it from the event list (if present)
    NewEvents = State#sm_data.events,

    State#sm_data{processes = NewProcesses, actives = NewActives, events = NewEvents}.

%% END
