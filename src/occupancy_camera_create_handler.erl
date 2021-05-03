-module(occupancy_camera_create_handler).
-behavior(cowboy_rest).

%% API
-export([
	init/2,
	allowed_methods/2,
	content_types_accepted/2,
	resource_exists/2,

	accept_json/2
]).

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
		{<<"application/json">>, accept_json}
	], Req, State}.


% -----------------------------------------------------------------------------
% resource_exists
%
% 	TODO uitleg wat deze functie doet
% -----------------------------------------------------------------------------

resource_exists(Req, State) ->
	{false, Req, State}.


% -----------------------------------------------------------------------------
% accept_json
%
% 	Geeft de lijst met camera's terug in JSON-formaat
% -----------------------------------------------------------------------------

accept_json(Req, State) ->

	% TODO alles

	% TODO verkrijg data uit POST request en INSERT een nieuwe record in de database

	Message = {
		[{<<"success">>, true}]
	},

	Req1 = cowboy_req:set_resp_body(jiffy:encode(Message), Req),

	{true, Req1, State}.