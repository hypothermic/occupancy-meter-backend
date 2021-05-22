-module(occupancy_app).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

% -----------------------------------------------------------------------------
% HTTP_PORT
%
% 	Constante met de poort waarop de HTTP server moet runnen
% -----------------------------------------------------------------------------

-define(HTTP_PORT, 8080).


% -----------------------------------------------------------------------------
% start
%
% 	Functie die wordt uitgevoerd wanneer de applicatie wordt gestart
% -----------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    logger:debug("Mnesia aan het configureren...."),
    occupancy_database:setup(),
    application:ensure_started(mnesia),

    logger:debug("Camera's aanmaken...."),
    lists:foreach(fun(Camera) ->
        occupancy_camera:start_link(Camera)
    end, occupancy_database:get_all_cameras()),

    logger:debug("REST API starten...."),
    application:ensure_all_started([cowboy, ranch, cowlib]),

    Routes = cowboy_router:compile([
        {'_', [
            {"/camera",          occupancy_rest_camera_list_handler,   []},
            {"/camera/new",      occupancy_rest_camera_create_handler, []},
            {"/camera/:name",    occupancy_rest_camera_delete_handler, []},
            {"/history/:camera", occupancy_rest_history_list_handler,  []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(occupancy_http_listener,
        [{port, ?HTTP_PORT}],
        #{
            env => #{dispatch => Routes},
            middlewares => [cowboy_router, cowboy_handler]
        }
    ),

    logger:debug("OK"),

    occupancy_sup:start_link().


% -----------------------------------------------------------------------------
% stop
%
% 	Functie die wordt aangeroepen wanneer de server wordt gestopt
% -----------------------------------------------------------------------------

stop(_State) ->
    ok.