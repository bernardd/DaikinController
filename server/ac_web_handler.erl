-module(ac_web_handler).

-compile(export_all). % Lazy

-behaviour(cowboy_http_handler).

-include("rc_server.hrl").

init(_Type, Req, _Opts) ->
	erlydtl:compile("index.html", index_template),
	{ok, Req, no_state}.

handle(Req, State) ->
	{ok, Req2} = cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/html">>}
		], body(), Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

body() ->
	State = rc_server:get_state(),
	{ok, Body} = index_template:render(
		[
			{fan_powers, settings_to_proplist(ac_state:fan(), State#ac_state.fan)},
			{modes, settings_to_proplist(ac_state:modes(), State#ac_state.mode)},
			{vert_deflectors, settings_to_proplist(ac_state:deflectors(), State#ac_state.vert_deflector)},
			{horiz_deflectors, settings_to_proplist(ac_state:deflectors(), State#ac_state.horiz_deflector)}
		]),
	iolist_to_binary(Body).

settings_to_proplist(Settings, Value) ->
	[begin
				[{name, N}, {key, K}, {value, V} | 
					case V of Value -> [{selected, true}]; _ -> [] end] 
		end || #setting{name = N, key = K, value = V} <- Settings].
