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
	CameraName = binary_to_list(cowboy_req:binding(camera, Req)),
	Options = [{binary_to_existing_atom(K, latin1), V} || {K, V} <- cowboy_req:parse_qs(Req)],

	PaginationOffset = binary_to_integer(proplists:get_value(amount, Options)),
	PaginationAmount = binary_to_integer(proplists:get_value(offset, Options)),

	% Verkrijg alle history entries uit de database en zet ze om naar JSON formaat
	HistoryListJson = lists:foldl(fun(_Point = #occupancy_history_entry{key = Key, people_amount = PeopleAmount}, List) ->
		PointJson = {[
			{time,   Key#occupancy_history_key.timestamp / 1000000},
			{amount, PeopleAmount}
		]},

		[PointJson|List]
	end, [], occupancy_database:get_history_for_camera(CameraName, {PaginationOffset, PaginationAmount})),

	% Vraag de camera status op bij het cameraproces
	CameraProcess = {global, occupancy_camera:process_name(CameraName)},
	CameraState = try
		case gen_server:call(CameraProcess, {is_online}, 100) of
			{noproc, _} ->
				offline;
			ProcessState ->
				ProcessState
		end
	catch
		_:_  ->
			false
	end,

	Message = {[
		{status, CameraState},
		{history, HistoryListJson}
	]},

	{jsone:encode(Message), Req, State}.