-module(occupancy_rest_camera_status_handler).
-behavior(cowboy_rest).

%% API
-export([
	init/2,
	allowed_methods/2,
	content_types_accepted/2,
	content_types_provided/2,

	accept_json/2
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
	{[<<"POST">>], Req, State}.


% -----------------------------------------------------------------------------
% content_types_provided functie
%
% 	Hierin geven we mee welke functies welke HTTP content types accepteren
% 	Bijvoorbeeld, accept_json accepteert application/json content type
% -----------------------------------------------------------------------------

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, accept_json}
	], Req, State}.

% -----------------------------------------------------------------------------
% content_types_provided functie
%
% 	Hierin geven we mee welke functies welke HTTP content types accepteren
% 	Bijvoorbeeld, accept_json returnt application/json content type
% -----------------------------------------------------------------------------

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, accept_json}
	], Req, State}.


% -----------------------------------------------------------------------------
% accept_json
%
% 	Geeft de lijst met camera's terug in JSON-formaat
% -----------------------------------------------------------------------------

accept_json(Req, _State) ->
	% Lees de data uit de POST request, converteer JSON naar een Erlang Map
	{ok, Body, Req1} = cowboy_req:read_body(Req),
	Decoded = jsone:decode(Body, [{object_format, proplist}]),

	% Haal de naam van de camera uit de REST-URL
	CameraName = binary_to_list(cowboy_req:binding(name, Req1)),

	% Haal de variabelen uit de map zodat we ze makkelijk kunnen gebruiken
	Action = proplists:get_value(<<"action">>, Decoded),
	CameraEntry = occupancy_database:get_camera(CameraName),

	case CameraEntry of
		undefined ->
			{false, Req1, undefined};
		_ ->
			case binary_to_list(Action) of
				"connect" ->
					occupancy_camera:start_link(CameraEntry),
					{true, Req1, undefined};
				"disconnect" ->
					gen_server:stop({global, occupancy_camera:process_name(CameraName)}),
					{true, Req1, undefined}
			end
	end.