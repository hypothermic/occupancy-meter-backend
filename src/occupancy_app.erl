-module(occupancy_app).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

-define(HTTP_PORT, 8080).

start(_StartType, _StartArgs) ->
    logger:debug("Mnesia aan het configureren...."),
    occupancy_database:setup(),

    logger:debug("REST API starten...."),
    application:ensure_all_started([cowboy, ranch, cowlib]),

    Routes = cowboy_router:compile([
        {'_', [
            {"/camera", occupancy_camera_list_handler, []}
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

stop(_State) ->
    ok.