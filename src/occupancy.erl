-module(occupancy).

-export([
	start/0,
	stop/0
]).

%%% =================================================================
%%% Geexporteerde functies
%%% =================================================================

start() ->
	application:start(occupancy).

stop() ->
	application:stop(occupancy).