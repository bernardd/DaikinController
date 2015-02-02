-module(freon).

-compile(export_all). % Lazy

apps() -> [crypto, cowlib, ranch, cowboy].

start() ->
	rc_server:start(),
	[application:start(A) || A <- apps()],
	Dispatch = cowboy_router:compile([{'_', [
														{"/static/[...]", cowboy_static, {dir, "static"}},
														{"/", freon_web, []},
														{"/socket", freon_websocket, []}
												]}]),
	cowboy:start_http(ac_http_listener, 10, [{port, 8080}, {reuseaddr, true}], [{env, [{dispatch, Dispatch}]}]).

stop() ->
	rc_server:stop(),
	[application:stop(A) || A <- apps()].
