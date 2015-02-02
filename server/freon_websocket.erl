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
	self() ! post_init,
	{ok, Req, #state{}}.

websocket_handle({text, JSON}, Req, State) ->
	io:fwrite("Got websocket text frame:\n~p\n", [JSON]),
	{ok, Req, State};

websocket_handle(Frame, Req, State) ->
	io:fwrite("Got unexpected websocket frame:\n~p\n", [Frame]),
	{ok, Req, State}.

websocket_info(post_init, Req, State) ->
	{reply, {text, json_state()}, Req, State};

websocket_info(_, Req, State) ->
	% handle update from arduino side
	{ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
	io:fwrite("Socket terminated: ~p", [Reason]),
	ok.

to_bin(I) when is_integer(I) -> integer_to_binary(I);
to_bin(S) when is_list(S) -> list_to_binary(S);
to_bin(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
to_bin(B) when is_binary(B) -> B.

json_state() ->
	Values = tl(tuple_to_list(#ac_state{})),
	Zipped = lists:zip(?STATE_FIELDS, Values),
	Tuples = {[{to_bin(K), to_bin(V)} || {K, V} <- Zipped]},
	jiffy:encode(Tuples).
	
