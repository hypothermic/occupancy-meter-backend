-module(occupancy_database).

-export([
	setup/0,

	get_all_cameras/0,
	create_camera/1,
	camera_exists/1
]).

-include("occupancy_database.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

% -----------------------------------------------------------------------------
% setup functie
%
%	Deze functie zorgt er voor dat het database schema aangemaakt wordt
% 	en dat alle tabellen aangemaakt worden.
% -----------------------------------------------------------------------------

setup() ->
	% Maak de database aan
	ok = case mnesia:create_schema([node()]) of
		ok -> % aanmaken van database successvol
			ok;
		{error, {_, {already_exists, _}}} -> % de database bestaat al, dit is prima
			ok
	end,

	% Start mnesia
	application:start(mnesia),
	application:ensure_started(mnesia),
	mnesia:system_info(),

	% Maak de camera's tabel aan (gekoppeld aan het record "occupancy_camera")
	ok = case create_cameras_table() of
		{atomic, ok} -> % aanmaken is successvol
			insert_sample_data();
		{aborted, {already_exists, occupancy_camera_entry}} -> % het tabel bestaat al, dit is prima
			ok
	end,

	% Wacht totdat de tabellen aangemaakt zijn
	case mnesia:wait_for_tables([occupancy_camera_entry], 1000) of
		{timeout, _} ->
			error;
		(_)->
			ok
	end.

% -----------------------------------------------------------------------------
% create_cameras_table functie
%
%	Deze functie zorgt er voor dat het het camera tabel aangemaakt wordt
% -----------------------------------------------------------------------------

create_cameras_table() ->
	mnesia:create_table(occupancy_camera_entry, [
		{type, set},
		{record_name, occupancy_camera_entry},
		{attributes, record_info(fields, occupancy_camera_entry)},
		{disc_copies, [node()]}
	]).

insert_sample_data() ->
	DemoCamera1 = #occupancy_camera_entry{name = "My Sample Camera 1", vps_ip_address = "192.168.0.85", cam_ip_address = "192.168.0.86"},
	DemoCamera2 = #occupancy_camera_entry{name = "My Sample Camera 2", vps_ip_address = "192.168.0.85", cam_ip_address = "192.168.0.87"},

	Query = fun() ->
		mnesia:write(DemoCamera1),
		mnesia:write(DemoCamera2)
	end,

	mnesia:transaction(Query),
	ok.

get_all_cameras() ->
	% Voer een SELECT query uit met een wildcard
	Query = fun() -> mnesia:select(occupancy_camera_entry, [{'_',[],['$_']}]) end,

	{atomic, Cameras} = mnesia:transaction(Query),

	Cameras.

create_camera(CameraEntry = #occupancy_camera_entry{}) ->
	Query = fun() -> mnesia:write(CameraEntry) end,

	{atomic, ok} = mnesia:transaction(Query),

	ok.

camera_exists(CameraName) ->
	% Als er een record met de camera naam CameraName bestaat, return true
	Query = fun() ->
		case mnesia:read(occupancy_camera_entry, CameraName) of
			[#occupancy_camera_entry{} = _CameraEntry] ->
				true;
			[] ->
				false
		end
	end,

	{atomic, Exists} = mnesia:transaction(Query),

	Exists.