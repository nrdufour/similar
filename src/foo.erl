%%%-------------------------------------------------------------------
%%% File : foo.erl
%%% Author : Nicolas R Dufour <nrdufour@gmail.com>
%%% Created : 2009/05/22
%%% Description :
%%%     Dummy process to test in the simulation engine.
%%%
%%% Copyright 2009 Nicolas R Dufour <nrdufour@gmail.com>
%%%
%%% This software is licensed as described in the file LICENSE, which
%%% you should have received as part of this distribution.
%%%-------------------------------------------------------------------

-module(foo).
-compile(export_all).

loop() ->
	receive
		die ->
			true;
		blah ->
			io:format("Blah from foo~n"),
			loop()
	end.

%%% end
