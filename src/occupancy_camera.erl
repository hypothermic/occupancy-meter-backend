-module(occupancy_camera).
-behaviour(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
	code_change/3
]).

-define(SERVER, ?MODULE).

-include("occupancy_database.hrl").

-record(occupancy_camera_state, {
    camera_entry :: #occupancy_camera_entry{},
    socket       :: term()
}).

start_link(#occupancy_camera_entry{} = CameraEntry) ->
	gen_server:start_link(?MODULE, [CameraEntry], []).

init([CameraEntry = #occupancy_camera_entry{vps_ip_address = VpsIp, cam_ip_address = CamIp}]) ->

    % Verbind met de VPS via een socket. Het python script op de VPS accept de socket.
    {ok, Socket} = gen_tcp:connect(VpsIp, 40001, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket,
		<<
			1:8/unsigned-integer,
			(length(CamIp))/unsigned-integer,
			(list_to_binary(CamIp))/binary
		>>),

	{ok, Command} = gen_tcp:recv(Socket, 1), //////// TODO

    % TODO receive statistieken van VPS

	{ok, #occupancy_camera_state{camera_entry = CameraEntry, socket = Socket}}.

handle_call(_Request, _From, State = #occupancy_camera_state{}) ->
	{reply, ok, State}.

handle_cast(_Request, State = #occupancy_camera_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #occupancy_camera_state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #occupancy_camera_state{}) ->
	ok.

code_change(_OldVsn, State = #occupancy_camera_state{}, _Extra) ->
	{ok, State}.