-module(occupancy_rest_camera_list_handler).
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
%	Initialiseert de cowboy rest API
% -----------------------------------------------------------------------------

init(Req, State) ->
	{cowboy_rest, Req, State}.


% -----------------------------------------------------------------------------
% allowed_methods functie
%
%	Laat alleen GET berichten door
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
	% Verkrijg alle camera info's uit de database en zet ze om naar JSON formaat
	Message = lists:foldl(fun(_Camera = #occupancy_camera_entry{name = Name, vps_ip_address = VpsIp, cam_ip_address = CamIp}, List) ->

		% Vraag de camera status op bij het cameraproces
		CameraProcess = {global, occupancy_camera:process_name(Name)},
		CameraState = try
		    case gen_server:call(CameraProcess, {is_online}, 100) of
				{noproc, _} ->
					false;
				ProcessState ->
					ProcessState
			end
		catch
		    _:_  ->
				false
		end,

		% Maak een JSON object van al deze data
		CameraJson = {[
			{name, list_to_binary(Name)},
			{vps_ip, list_to_binary(VpsIp)},
			{cam_ip, list_to_binary(CamIp)},
			{is_online, CameraState}
		]},

		% Append het JSON object wat we net gemaakt hebben aan de List
		[CameraJson|List]
	end, [], occupancy_database:get_all_cameras()),

	{jsone:encode(Message), Req, State}.