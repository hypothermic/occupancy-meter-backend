-module(occupancy_camera).
-behaviour(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
	code_change/3,
	process_name/1
]).

-define(SERVER, ?MODULE).
-define(CAMERA_PROCESS_PREFIX, "__occupancy_camera_process_").

-include("occupancy_database.hrl").

-record(occupancy_camera_state, {
    camera_entry 		:: #occupancy_camera_entry{},
    socket       		:: term(),
	remaining	= <<>>	:: binary()
}).

process_name(CameraName) ->
	list_to_atom(?CAMERA_PROCESS_PREFIX ++ CameraName).

start_link(#occupancy_camera_entry{name = CameraName} = CameraEntry) ->
	gen_server:start_link({global, process_name(CameraName)}, ?MODULE, [CameraEntry], []).

init([CameraEntry = #occupancy_camera_entry{vps_ip_address = VpsIp, cam_ip_address = CamIp}]) ->

    % Verbind met de VPS via een socket. Het python script op de VPS accept de socket.
    {ok, Socket} = gen_tcp:connect(VpsIp, 40001, [binary, {packet, 0}]),

	% Stuur een packet1, met het IP-adres van de camera
    ok = gen_tcp:send(Socket,
		<<
			1:8/unsigned-integer,
			(length(CamIp))/unsigned-integer,
			(list_to_binary(CamIp))/binary
		>>),

	{ok, #occupancy_camera_state{camera_entry = CameraEntry, socket = Socket}}.

handle_call({is_online}, _From, State = #occupancy_camera_state{}) ->
	{reply, true, State};

handle_call(_Request, _From, State = #occupancy_camera_state{}) ->
	{reply, ok, State}.

handle_cast(_Request, State = #occupancy_camera_state{}) ->
	{noreply, State}.

handle_info(Info, State = #occupancy_camera_state{}) ->
	case Info of
		{tcp_closed, _} ->
			{stop, State};

		{tcp, _, Data} when is_binary(Data) ->
			NewState = handle_data(Data, State),
			{noreply, NewState};

		(_) ->
			{noreply, State}
	end.

terminate(_Reason, _State = #occupancy_camera_state{}) ->
	ok.

code_change(_OldVsn, State = #occupancy_camera_state{}, _Extra) ->
	{ok, State}.

handle_data(Data, State = #occupancy_camera_state{camera_entry = CameraEntry, remaining = RemainingData}) ->
	<<CommandId:8/unsigned-integer, Rest/binary>> = <<RemainingData/binary, Data/binary>>,

	case CommandId of
		4 ->
			% Lees het getal uit de data
			<<PeopleAmount:8/unsigned-integer, Rest2/binary>> = Rest,

			% Sla dit getal op in de database
			{atomic, ok} = mnesia:transaction(fun() ->
				EntryKey = #occupancy_history_key{camera = CameraEntry, timestamp = erlang:system_time()},
				mnesia:write(#occupancy_history_entry{key = EntryKey, people_amount = PeopleAmount})
			end),
			State#occupancy_camera_state{remaining = Rest2};

		_ ->
			State#occupancy_camera_state{remaining = Rest}
	end.
