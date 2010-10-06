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

-module(similar_config).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-export([start_link/0, get/1, set/2]).

-include("similar.hrl").

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) ->
	gen_server:call(?MODULE, {get, Key}).

set(Key, Value) ->
	gen_server:call(?MODULE, {set, Key, Value}).

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
	EtcDir = get_etc_dir(),
	ConfigFile = filename:join(EtcDir, "similar.config"),
	Exists = filelib:is_regular(ConfigFile),
	
	%% retrieve the default values
	InitialDict = get_default_configuration(EtcDir),
	Dict = case Exists of
		false ->
			similar_log:info("No configuration file named [~p]! Defaulted!", [ConfigFile]),
			InitialDict;
		true  ->
			similar_log:info("Reading configuration file named [~p]!", [ConfigFile]),
			ReadDict = read_configuration(ConfigFile),
			%% Retain the value provided by the config file
			%% if the same key is seen in both dicts
			MergingFun = fun(Key, Value1, Value2) ->
								 case Key of
									 etc -> Value1;
									 version -> Value1;
									 _ -> Value2
								 end
						 end,
			MergedDict = dict:merge(MergingFun, InitialDict, ReadDict),
			MergedDict
	end,
	similar_log:info("Similar Config with ~p entries!", [dict:size(Dict)]),
	similar_log:info("Content: ~p", [dict:to_list(Dict)]),
	{ok, Dict}.

read_configuration(ConfigFile) ->
	List = case parse_file(ConfigFile) of
		{error, Why} ->
			throw({error, Why});
		{ok, Terms} ->
			Terms
	end,
	Dict = dict:from_list(List),
	Dict.

get_default_configuration(EtcDir) ->
	Dict = dict:new(),
	
	%% add etc directory
	D1 = dict:store(etc, EtcDir, Dict),
	
	%% add version
	D2 = dict:store(version, ?VERSION, D1),
	
	D2.

%% TODO probably a bad idea to fall back to /tmp for security reason
get_etc_dir() ->
    {ok, CurrentDir} = file:get_cwd(),
    Candidate = filename:join(CurrentDir, "etc"),
    IsDir = filelib:is_dir(Candidate),
    DataDir = if IsDir ->
        Candidate;
    true ->
        ?DEFAULT_ETC
    end,
    DataDir.

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
handle_call({get, Key}, _From, State) ->
	Value = try dict:fetch(Key, State) catch _:_ -> nil end,
	{reply, {ok, Value}, State};

handle_call({set, Key, Value}, _From, State) ->
	UpdatedDict = dict:store(Key, Value, State),
	{reply, ok, UpdatedDict};

handle_call(_Args, _From, State) ->
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
handle_info(_Info, State) ->
	{noreply, State}.

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

parse_file(Filename) ->
	%% Use Erlang format
	file:consult(Filename).

%% END
