-module(occupancy_rest_camera_create_handler).
-behavior(cowboy_rest).

%% API
-export([
	init/2,
	allowed_methods/2,
	content_types_accepted/2,
	content_types_provided/2,
	resource_exists/2,

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
% resource_exists
%
% 	TODO uitleg wat deze functie doet
% -----------------------------------------------------------------------------

resource_exists(Req, State) ->
	% Lees de data uit de POST request, converteer JSON naar een Erlang Map
	{ok, Body, Req1} = cowboy_req:read_body(Req),
	Decoded = jiffy:decode(Body, [return_maps, dedupe_keys]),

	% Haal de variabelen uit de map zodat we ze makkelijk kunnen gebruiken
	Name = maps:get(<<"name">>, Decoded),

	% Check in database of camera met de naam 'Name' bestaat.
	case occupancy_database:camera_exists(binary_to_list(Name)) of
		% Bestaat al. kap ermee.
		true ->
			Req2 = cowboy_req:reply(409, Req1),
			{stop, Req2, Decoded};
		% OK, ga door met accept_json
		false ->
			{false, Req1, Decoded}
	end.


% -----------------------------------------------------------------------------
% accept_json
%
% 	Geeft de lijst met camera's terug in JSON-formaat
% -----------------------------------------------------------------------------

accept_json(Req, Decoded) ->
	logger:warning("accept ~p", [Decoded]),
	%try
		% Haal de variabelen uit de map zodat we ze makkelijk kunnen gebruiken
		Name = maps:get(<<"name">>, Decoded),
		VpsIp = maps:get(<<"vps_ip">>, Decoded),
		CamIp = maps:get(<<"cam_ip">>, Decoded),

		% Maak een database entry van de bovenstaande variabelen
		Entry = #occupancy_camera_entry{
			name			= binary_to_list(Name),
			vps_ip_address	= binary_to_list(VpsIp),
			cam_ip_address	= binary_to_list(CamIp)
		},

		% Geef de entry door naar de database module
		occupancy_database:create_camera(Entry),

		% Return success=true
		{{true, "/camera/" ++ Name}, Req, undefined}.
	%catch
	%    _:_  ->
	%		{false, Req, undefined}
	%end.