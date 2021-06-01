-module(occupancy_database).

-export([
	setup/0,

	get_all_cameras/0,
	create_camera/1,
	delete_camera/1,
	get_camera/1,
	camera_exists/1,

	get_all_histories/0,
	get_history_for_camera/2
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

	% Maak de camera's tabel aan (gekoppeld aan het record "occupancy_camera_entry")
	ok = case create_cameras_table() of
		{atomic, Result} -> % aanmaken is successvol
			Result;
		{aborted, {already_exists, occupancy_camera_entry}} -> % het tabel bestaat al, dit is prima
			ok
	end,

	% Maak de history tabel aan (gekoppeld aan het record "occupancy_history_entry")
	ok = case create_history_table() of
		{atomic, ok} -> % aanmaken is successvol
			ok;
		{aborted, {already_exists, occupancy_history_entry}} -> % het tabel bestaat al, dit is prima
			ok
	end,

	% Wacht totdat de tabellen aangemaakt zijn
	case mnesia:wait_for_tables([occupancy_camera_entry, occupancy_history_entry], 1000) of
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

% -----------------------------------------------------------------------------
% create_history_table
%
% 	Functie die er voor zorgt dat het history tabel aangemaakt wordt
% -----------------------------------------------------------------------------

create_history_table() ->
	mnesia:create_table(occupancy_history_entry, [
		{type, set},
		{record_name, occupancy_history_entry},
		{attributes, record_info(fields, occupancy_history_entry)},
		{disc_copies, [node()]}
	]).

% -----------------------------------------------------------------------------
% get_all_cameras
%
% 	Functie die een lijst met alle camera's returnt
% -----------------------------------------------------------------------------

get_all_cameras() ->
	% Voer een SELECT query uit met een wildcard
	Query = fun() -> mnesia:select(occupancy_camera_entry, [{'_',[],['$_']}]) end,

	{atomic, Cameras} = mnesia:transaction(Query),

	Cameras.

% -----------------------------------------------------------------------------
% get_all_histories
%
% 	Functie die alle history entries van alle camera's returnt
% -----------------------------------------------------------------------------

get_all_histories() ->
	Query = fun() -> mnesia:select(occupancy_history_entry, [{'_',[],['$_']}]) end,

	{atomic, Histories} = mnesia:transaction(Query),

	Histories.

% -----------------------------------------------------------------------------
% get_history_for_camera
%
% 	Functie die alle history punten voor een camera returnt
% -----------------------------------------------------------------------------

get_history_for_camera(Camera, {ResultAmount, ResultOffset}) ->
	% Patroon voor welke eisen we stellen aan de return records (WHERE commando in SQL)
	Pattern = #occupancy_history_entry{
		key = #occupancy_history_key{camera = #occupancy_camera_entry{name = Camera, cam_ip_address = '_', vps_ip_address = '_'}, timestamp = '_'},
		people_amount = '_'
	},

	Query = fun() ->
		Sorted =
			lists:sort(
				fun(#occupancy_history_entry{key = KeyA}, #occupancy_history_entry{key = KeyB}) ->
					KeyA#occupancy_history_key.timestamp > KeyB#occupancy_history_key.timestamp
				end,
				mnesia:match_object(Pattern)
			),

		if
			ResultOffset > 0 ->
				lists:sublist(Sorted, ResultOffset, ResultAmount);
			true ->
				lists:sublist(Sorted, ResultAmount)
		end
	end,

	{atomic, History} = mnesia:transaction(Query),

	History.

% -----------------------------------------------------------------------------
% create_camera
%
% 	Functie die een nieuwe camera entry in de database aanmaakt
% -----------------------------------------------------------------------------

create_camera(CameraEntry = #occupancy_camera_entry{}) ->
	Query = fun() -> mnesia:write(CameraEntry) end,

	{atomic, ok} = mnesia:transaction(Query),

	ok.

% -----------------------------------------------------------------------------
% delete_camera
%
% 	Functie die een camera uit de database verwijdert
% -----------------------------------------------------------------------------

delete_camera(CameraName) ->
	Query = fun() -> mnesia:delete(occupancy_camera_entry, CameraName, write) end,

	{atomic, ok} = mnesia:transaction(Query),

	ok.

% -----------------------------------------------------------------------------
% get_camera
%
% 	Functie die camera info uit de database verkrijgt
% -----------------------------------------------------------------------------

get_camera(CameraName) ->
	% Als er een record met de camera naam CameraName bestaat, return deze record
	Query = fun() ->
		case mnesia:read(occupancy_camera_entry, CameraName) of
			[#occupancy_camera_entry{} = CameraEntry] ->
				CameraEntry;
			[] ->
				undefined
		end
	end,

	{atomic, Exists} = mnesia:transaction(Query),

	Exists.

% -----------------------------------------------------------------------------
% camera_exists
%
% 	Functie die controleert of er een camera bestaat met deze naam in de database
% -----------------------------------------------------------------------------

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