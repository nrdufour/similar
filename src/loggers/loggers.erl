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

	%% Message to store
	Msg = io_lib:format(Format, Args),
	
	%% Header
	{_Date, Time} = calendar:local_time(),
	Header = io_lib:format("[~p] -- ", [Time]),

	%% Footer
	Footer = "\n",

	Final = string:concat(string:concat(Header, Msg), Footer),

        gen_event:notify(sm_msg_man, Final).


%% END
