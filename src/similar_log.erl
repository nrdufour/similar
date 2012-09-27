% This file is part of Similar released under the MIT license.
% See the LICENSE file for more information.

-module(similar_log).

-export([start/0, stop/0, info/1, info/2, switch/1, is_activated/0]).

-include("similar.hrl").

start() ->
    gen_event:start({local, ?LOG_EM}),
    gen_event:add_handler(?LOG_EM, similar_logger, []).

stop() ->
    gen_event:delete_handler(?LOG_EM, similar_logger, []),
    gen_event:stop(?LOG_EM).

info(Msg) ->
        gen_event:notify(?LOG_EM, Msg).

info(Format, Args) ->
    Msg = io_lib:format(Format, Args),
    {_Date, Time} = calendar:local_time(),
    FormattedTime = format_time(Time),
    MsgWithDateTime = io_lib:format("[~s] -- ~s ~n", [FormattedTime, Msg]),
    info(MsgWithDateTime).

switch(OnOrOff) when is_boolean(OnOrOff) ->
    gen_event:call(?LOG_EM, similar_loggger, {trace, OnOrOff}).
    
is_activated() ->
    gen_event:call(?LOG_EM, similar_logger, is_activated).

%% INTERNAL

format_time({Hour, Minute, Second}) ->
        io_lib:format("~2..0w:~2..0w:~2..0w", [Hour, Minute, Second]).

%% END
