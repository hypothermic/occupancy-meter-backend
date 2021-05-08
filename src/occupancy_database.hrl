-record(occupancy_camera_entry, {
	name			:: string(), % gebruikersvriendelijke naam
	vps_ip_address	:: string(), % IP-adres van de VPS
	cam_ip_address	:: string()  % IP-adres van de cameramodule
}).

-record(occupancy_history_key, {
	camera			:: #occupancy_camera_entry{},
	timestamp		:: integer() % unix timestamp van wanneer de telling is genomen
}).

-record(occupancy_history_entry, {
	key				:: #occupancy_history_key{},
	people_amount	:: integer() % hoeveel mensen er gedetecteerd zijn
}).