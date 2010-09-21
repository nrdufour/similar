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

-module(similar_log).

-export([start/0, stop/0, log/1, log_format/2, format_time/1]).

-define(LOGGER, sm_logger).

start() ->
	gen_event:start({local, ?LOGGER}),
	gen_event:add_handler(?LOGGER, similar_logger, []).

stop() ->
	gen_event:delete_handler(?LOGGER, similar_logger, []),
	gen_event:stop(?LOGGER).

log(Msg) ->
        gen_event:notify(?LOGGER, Msg).

log_format(Format, Args) ->
	Msg = io_lib:format(Format, Args),
	{_Date, Time} = calendar:local_time(),
	FormattedTime = format_time(Time),
	MsgWithDateTime = io_lib:format("[~s] -- ~s ~n", [FormattedTime, Msg]),
        gen_event:notify(?LOGGER, MsgWithDateTime).

format_time({Hour, Minute, Second}) ->
        io_lib:format("~2..0w:~2..0w:~2..0w", [Hour, Minute, Second]).

%% END