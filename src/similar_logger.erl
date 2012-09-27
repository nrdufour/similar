% This file is part of Similar released under the MIT license.
% See the LICENSE file for more information.

-module(similar_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_cast/2, handle_info/2, code_change/3]).

%% trace is enabled by default
init(_Args) ->
    {ok, true}.

%% display a simple message
handle_event(Msg, State) ->
    if
        State ->
            io:format(Msg);
        true ->
            true
    end,
    {ok, State}.

%% activate or not the trace
handle_call({trace, true}, _State) ->
    {ok, true};
handle_call({trace, false}, _State) ->
    {ok, false};

%% returns true if the logger is activated, false otherwise
handle_call(is_activated, State) ->
    {{ok,State}, State};

handle_call(_, State) ->
    {noreply, State}.

%% --

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

%%% END
