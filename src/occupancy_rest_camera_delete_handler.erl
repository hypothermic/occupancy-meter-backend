-module(occupancy_rest_camera_delete_handler).
-behavior(cowboy_rest).

%% API
-export([
	init/2,
	allowed_methods/2,
	resource_exists/2,
	delete_resource/2
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
	{[<<"DELETE">>], Req, State}.

% -----------------------------------------------------------------------------
% resource_exists
%
% 	TODO uitleg wat deze functie doet
% -----------------------------------------------------------------------------

resource_exists(Req, _State) ->
	% Haal de variabelen uit de map zodat we ze makkelijk kunnen gebruiken
	CameraName = binary_to_list(cowboy_req:binding(name, Req)),

	% Check in database of camera met de naam 'Name' bestaat.
	case occupancy_database:camera_exists(CameraName) of
		% Bestaat niet. Stoppen.
		false ->
			Req1 = cowboy_req:reply(409, Req),
			{stop, Req1, CameraName};
		% Bestaat, ga door met accept_json
		true ->
			{true, Req, CameraName}
	end.

% -----------------------------------------------------------------------------
% delete_resource
%
% 	TODO uitleg wat deze functie doet
% -----------------------------------------------------------------------------

delete_resource(Req, CameraName) ->
	% Stop de camera indien hij nog bezig is
	ok = try
		gen_server:stop({global, occupancy_camera:process_name(CameraName)})
	catch
		_:_ ->
			ok
	end,

	% Verwijder de camera.
	occupancy_database:delete_camera(CameraName),

	% Return success=true
	{{true}, Req, undefined}.
