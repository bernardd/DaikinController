-module(ac_controller).

-compile(export_all). % Lazy

start() ->
	rc_server:start(),
	Apps = [crypto, cowlib, ranch, cowboy],
	[application:start(A) || A <- Apps],
	Dispatch = cowboy_router:compile([{'_', [{'_', ac_web_handler, []}]}]),
	cowboy:start_http(ac_http_listener, 10, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]).
