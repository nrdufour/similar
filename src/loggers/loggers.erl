%%%-------------------------------------------------------------------
%%% File : loggers.erl
%%% Author : Nicolas R Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/17
%%% Description :
%%%     Provides logging facilities
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------
-module(loggers).

-compile(export_all).

log(Format, Args) ->
	Msg = io_lib:format(Format, Args),
	{_Date, Time} = calendar:local_time(),
	FormattedTime = format_time(Time),
	Final = io_lib:format("[~s] -- ~s ~n", [FormattedTime, Msg]),
        gen_event:notify(sm_msg_man, Final).

format_time({Hour, Minute, Second}) ->
	io_lib:format("~2..0w:~2..0w:~2..0w", [Hour, Minute, Second]).




%% END
