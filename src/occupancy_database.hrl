% -----------------------------------------------------------------------------
% occupancy_camera_entry
%
% 	Datastructuur met de informatie voor een camera
% -----------------------------------------------------------------------------

-record(occupancy_camera_entry, {
	name			:: string(), % gebruikersvriendelijke naam
	vps_ip_address	:: string(), % IP-adres van de VPS
	cam_ip_address	:: string()  % IP-adres van de cameramodule
}).

% -----------------------------------------------------------------------------
% occupancy_history_key
%
% 	Datastructuur met de composite primary key van de history points tabel
% -----------------------------------------------------------------------------

-record(occupancy_history_key, {
	camera			:: #occupancy_camera_entry{},
	timestamp		:: integer() % unix timestamp van wanneer de telling is genomen
}).

% -----------------------------------------------------------------------------
% occupancy_history_entry
%
% 	Datastructuur met de informatie van een history point
% -----------------------------------------------------------------------------

-record(occupancy_history_entry, {
	key				:: #occupancy_history_key{},
	people_amount	:: integer() % hoeveel mensen er gedetecteerd zijn
}).