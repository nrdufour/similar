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
	{_Date, Time} = calendar:local_time(),
	LocalArgs = [Time | Args],
        SF1 = string:concat("[~p] -- ", Format),
        SF2 = string:concat(SF1, "\n"),
        Msg = io_lib:format(SF2, LocalArgs),
        gen_event:notify(sm_msg_man, Msg).


%% END
