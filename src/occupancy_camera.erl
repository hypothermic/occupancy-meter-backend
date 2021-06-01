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

-include("occupancy_database.hrl").

% -----------------------------------------------------------------------------
% CAMERA_PROCESS_PREFIX
%
% 	Constante met daarin de prefix van de naam van een cameraproces
% -----------------------------------------------------------------------------

-define(CAMERA_PROCESS_PREFIX, "__occupancy_camera_process_").

% -----------------------------------------------------------------------------
% occupancy_camera_state
%
% 	Datastructuur met daarin de variabelen van een cameraproces
% -----------------------------------------------------------------------------

-record(occupancy_camera_state, {
    camera_entry 		:: #occupancy_camera_entry{},
    socket       		:: term(),
	remaining	= <<>>	:: binary()
}).

% -----------------------------------------------------------------------------
% process_name
%
% 	Functie om de naam van een occupancy_camera proces te krijgen van de cameranaam
% -----------------------------------------------------------------------------

process_name(CameraName) ->
	list_to_atom(?CAMERA_PROCESS_PREFIX ++ CameraName).

% -----------------------------------------------------------------------------
% start_link
%
% 	Functie om een cameraproces te starten
% -----------------------------------------------------------------------------

start_link(#occupancy_camera_entry{name = CameraName} = CameraEntry) ->
	gen_server:start_link({global, process_name(CameraName)}, ?MODULE, [CameraEntry], []).

% -----------------------------------------------------------------------------
% init
%
% 	Functie die wordt uitgevoerd zodra een cameraproces gestart wordt
% -----------------------------------------------------------------------------

init([CameraEntry = #occupancy_camera_entry{vps_ip_address = VpsIp, cam_ip_address = CamIp}]) ->

	try
    	% Verbind met de VPS via een socket. Het python script op de VPS accept de socket.
    	{ok, Socket} = gen_tcp:connect(VpsIp, 40001, [binary, {packet, 0}]),

		% Stuur een packet1, met het IP-adres van de camera
    	ok = gen_tcp:send(Socket,
			<<
				1:8/unsigned-integer,
				(length(CamIp))/unsigned-integer,
				(list_to_binary(CamIp))/binary
			>>),

		{ok, #occupancy_camera_state{camera_entry = CameraEntry, socket = Socket}}
	catch
		_:_ ->
			{stop, normal, undefined}
	end.

% -----------------------------------------------------------------------------
% handle_call - {is_online}
%
% 	Functie die antwoordt of de camera online is
% -----------------------------------------------------------------------------

handle_call({is_online}, _From, State = #occupancy_camera_state{}) ->
	{reply, true, State};

handle_call(_Request, _From, State = #occupancy_camera_state{}) ->
	{reply, ok, State}.

handle_cast(_Request, State = #occupancy_camera_state{}) ->
	{noreply, State}.

% -----------------------------------------------------------------------------
% handle_info
%
% 	Functie die de inkomende TCP socket informatie verwerkt
% -----------------------------------------------------------------------------

handle_info(Info, State = #occupancy_camera_state{}) ->
	case Info of
		% TCP socket is terminated
		{tcp_closed, _} ->
			% Shutdown dit cameraproces
			{stop, shutdown, State};
		% Nieuwe bytes ontvangen door socket
		{tcp, _, Data} when is_binary(Data) ->
			% Verwerk deze bytes
			NewState = handle_data(Data, State),
			{noreply, NewState};
		% Overige info messages
		(_) ->
			{noreply, State}
	end.

% -----------------------------------------------------------------------------
% terminate
%
% 	Functie die wordt uitgevoerd wanneer een cameraproces afsluit
% -----------------------------------------------------------------------------

terminate(_Reason, _State = #occupancy_camera_state{socket = Socket}) ->
	% Probeer nog een terminate bericht te sturen naar de VPS
	try
		% Stuur een packet 2, maakt niet uit of deze overkomt naar de VPS omdat de socket toch automatisch sluit
	    gen_tcp:send(Socket,
			<<
				2:8/unsigned-integer
			>>)
	catch
	    _:_  ->
			ok
	end.

code_change(_OldVsn, State = #occupancy_camera_state{}, _Extra) ->
	{ok, State}.

% -----------------------------------------------------------------------------
% handle_data
%
% 	Verwerk inkomende bytes vanuit de TCP socket.
% -----------------------------------------------------------------------------

handle_data(Data, State = #occupancy_camera_state{camera_entry = CameraEntry, remaining = RemainingData}) ->
	% Eerst wordt er een byte gelezen, de rest is een binary array
	<<CommandId:8/unsigned-integer, Rest/binary>> = <<RemainingData/binary, Data/binary>>,

	% Kijk welke opcode dit is
	case CommandId of
		% Stop-opcode
		3 ->
			gen_server:stop(self());
		% Person data opcode
		4 ->
			% Lees het getal uit de data
			<<PeopleAmount:8/unsigned-integer, Rest2/binary>> = Rest,

			% Sla dit getal op in de database
			{atomic, ok} = mnesia:transaction(fun() ->
				EntryKey = #occupancy_history_key{camera = CameraEntry, timestamp = erlang:system_time()},
				mnesia:write(#occupancy_history_entry{key = EntryKey, people_amount = PeopleAmount})
			end),
			State#occupancy_camera_state{remaining = Rest2};
		% Andere opcodes
		_ ->
			State#occupancy_camera_state{remaining = Rest}
	end.
