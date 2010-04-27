%% Copyright 2009-2010 Nicolas R Dufour.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @author Nicolas R Dufour <nrdufour@gmail.com>
%% @copyright 2009-2010 Nicolas R Dufour.

-module(similar_terminal_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_cast/2, handle_info/2, code_change/3]).

%% trace is enabled by default
init(_Args) ->
	{ok, true}.

%% activate or not the trace
handle_event({trace, Trace}, _State) ->
	{ok, Trace};

%% display a simple message
handle_event(Msg, State) ->
	if
		State ->
			io:format(Msg);
		true ->
			true
	end,
	{ok, State}.

handle_call(_, State) ->
	{noreply, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Args, _State) ->
	ok.

%%% END
