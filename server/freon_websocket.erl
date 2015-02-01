-module(freon_websocket).

-compile(export_all).

-behaviour(cowboy_websocket_handler).

-include("rc_server.hrl").

-record(state, {
			}).

init(_Type, _Req, _Opts) ->
	io:fwrite("Websocket init ~p ~p ~p\n", [_Type, _Req, _Opts]),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
	io:fwrite("websocket_init\n"),
	{ok, Req, #state{}}.

websocket_handle({text, JSON}, Req, State) ->
	io:fwrite("Got websocket text frame:\n~p\n", [JSON]),
	{ok, Req, State};

websocket_handle(Frame, Req, State) ->
	io:fwrite("Got unexpected websocket frame:\n~p\n", [Frame]),
	{ok, Req, State}.

websocket_info(_, Req, State) ->
	% handle update from arduino side
	{ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
	io:fwrite("Socket terminated: ~p", [Reason]),
	ok.
