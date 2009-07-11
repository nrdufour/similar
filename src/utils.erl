%%%-------------------------------------------------------------------
%%% File : utils.erl
%%% Author : Nicolas Dufour <nrdufour@gmail.com>
%%% Created : 2009/07/11
%%% Description :
%%%	Utility functions.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(utils).

-compile(export_all).

internal_kill(Pid) ->
	log("Killing process ~p now", [Pid]),
	exit(Pid, terminated).

log(Format, Args) ->
	SF1 = string:concat("\t", Format),
	SF2 = string:concat(SF1, "\n"),
	Msg = io_lib:format(SF2, Args),
	gen_event:notify(sm_msg_man, Msg).

%% END
	
