-module(occupancy_rest_history_list_handler).
-behavior(cowboy_rest).

-export([
	init/2,
	allowed_methods/2,
	content_types_provided/2,

	return_json/2
]).

-include("occupancy_database.hrl").

% -----------------------------------------------------------------------------
% init functie
%
%	TODO uitleg wat deze functie doet
% -----------------------------------------------------------------------------

init(Req, State) ->
	{cowboy_rest, Req, State}.


% -----------------------------------------------------------------------------
% allowed_methods functie
%
%	TODO uitleg wat deze functie doet
% -----------------------------------------------------------------------------

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.


% -----------------------------------------------------------------------------
% content_types_provided functie
%
% 	Hierin geven we mee welke functies welke HTTP content types accepteren
% 	Bijvoorbeeld, return_json returnt application/json content type
% -----------------------------------------------------------------------------

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, return_json}
	], Req, State}.


% -----------------------------------------------------------------------------
% return_json
%
% 	Geeft de lijst met camera's terug in JSON-formaat
% -----------------------------------------------------------------------------

return_json(Req, State) ->
	% Verkrijg alle history entries uit de database en zet ze om naar JSON formaat
	Message = lists:foldl(fun(_Point = #occupancy_history_entry{key = Key, people_amount = PeopleAmount}, List) ->
		PointJson = {[
			{time,   Key#occupancy_history_key.timestamp},
			{amount, PeopleAmount}
		]},

		[PointJson|List]
	end, [], occupancy_database:get_history_for_camera(binary_to_list(cowboy_req:binding(camera, Req)))),

	{jiffy:encode(Message), Req, State}.