% This file is part of Similar released under the MIT license.
% See the LICENSE file for more information.

-module(similar).
-behaviour(application).

%% Application API
-export([start/2, stop/1]).

%% User API
-export([start/0]).

%%====================================================================
%% Application API
%%====================================================================

start(_Type, StartArgs) ->
    similar_log:start(),
    similar_log:info("Similar Engine is starting!", []),

    Reply = case start_apps([sasl]) of
        ok ->
            similar_sup:start_link(StartArgs);
        {error, Reason} ->
            {error, Reason}
    end,
    Reply.

stop(_State) ->
    similar_log:info("Similar Engine is stopping!", []),
    similar_engine:reset(),
    similar_log:stop(),
    ok.

%%====================================================================
%% Public API
%%====================================================================

start() ->
    application:start(similar).

%%====================================================================
%% Internal API
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
