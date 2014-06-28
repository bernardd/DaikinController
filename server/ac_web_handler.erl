-module(ac_web_handler).

-compile(export_all). % Lazy

-export([init/3, handle/2, terminate/3]).

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
	{Type, State} = rc_server:get_state(),
	RenderData = 
	  [
		{fan_powers, settings_to_proplist(ac_data:fan(), State#ac_state.fan)},
		{modes, settings_to_proplist(ac_data:modes(), State#ac_state.mode)},
		{vert_deflectors, settings_to_proplist(ac_data:deflectors(), State#ac_state.vert_deflector)},
		{horiz_deflectors, settings_to_proplist(ac_data:deflectors(), State#ac_state.horiz_deflector)},
		{temperature, [{value, State#ac_state.temp}, {min, ?MIN_TEMP}, {max, ?MAX_TEMP}]},
		{power, State#ac_state.power},
		{comfort, State#ac_state.comfort},
		{powerful, State#ac_state.powerful},
		{quiet, State#ac_state.quiet},
		{motion_detect, State#ac_state.motion_detect},
		{eco, State#ac_state.eco},
		{type, Type}
	  ],
	io:fwrite("~p\n", [RenderData]),
	{ok, Body} = index_template:render(RenderData),
	iolist_to_binary(Body).

settings_to_proplist(Settings, Value) ->
	[begin
				[{name, N}, {key, K}, {value, V} | 
					case V of Value -> [{selected, true}]; _ -> [] end]
		end || #setting{name = N, key = K, value = V} <- Settings].
