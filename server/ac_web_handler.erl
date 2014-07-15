-module(ac_web_handler).

-compile(export_all). % Lazy

-export([init/3, handle/2, terminate/3]).

-behaviour(cowboy_http_handler).

-include("rc_server.hrl").

init(_Type, Req, _Opts) ->
	erlydtl:compile("index.html", index_template),
	{ok, Req, no_state}.

handle(Req, State) ->
	io:fwrite("State: ~p\nReq: ~p\n", [State, Req]),
	Req2 = case cowboy_req:body_qs(Req) of
		{ok, [], R} ->
			R;
		{ok, List, R} ->
			update_state(List),
			R;
		{error, Reason} ->
			io:fwrite("Body parsing error: ~p\n", [Reason]),
			Req
	end,
	{ok, Req3} = cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/html">>}
		], body(), Req2),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

body() ->
	{Type, State} = rc_server:get_state(),
	RenderData =
	  [
		{fan_powers, settings_to_proplist(ac_data:fan(), State#ac_state.fan)},
		{modes, settings_to_proplist(ac_data:modes(), State#ac_state.mode)},
		{vert_deflector, settings_to_proplist(ac_data:deflector(), State#ac_state.vert_deflector)},
		{horiz_deflector, settings_to_proplist(ac_data:deflector(), State#ac_state.horiz_deflector)},
		{temperature, [{value, State#ac_state.temp}, {min, ?MIN_TEMP}, {max, ?MAX_TEMP}]},
		{power, State#ac_state.power},
		{comfort, State#ac_state.comfort},
		{powerful, State#ac_state.powerful},
		{quiet, State#ac_state.quiet},
		{motion_detect, State#ac_state.motion_detect},
		{eco, State#ac_state.eco},
		{turn_on_time, {value, "T16:00:00"}}, %rc_server:p_time(State#ac_state.turn_on_time)},
		{turn_off_time, "T16:00:00"}, %rc_server:p_time(State#ac_state.turn_on_time)},
		{type, Type}
	  ],
	{ok, Body} = index_template:render(RenderData),
	iolist_to_binary(Body).

settings_to_proplist(Settings, Value) ->
	[begin
				[{name, N}, {key, K}, {value, V} | 
					case V of Value -> [{selected, true}]; _ -> [] end]
		end || #setting{name = N, key = K, value = V} <- Settings].

update_state(List) ->
	io:fwrite("Got request: ~p\n", [List]),
	{Time, Day} = cur_time_day(),
	NewState = #ac_state{
		comfort=0,
		power=0,
		vert_deflector=0,
		horiz_deflector=0,
		powerful=0,
		quiet=0,
		motion_detect=0,
		eco=0,
		turn_on_time=?NO_TIME,
		turn_off_time=?NO_TIME,
		time=Time,
		day=Day
	},
	S = lists:foldl(fun update_state/2, NewState, List),
	rc_server:set_state(S),
	put(state, S).

cur_time_day() ->
	{Date, {H, M, _}} = calendar:local_time(),
	ISODay = calendar:day_of_the_week(Date),
	ACDay = case ISODay of
		1 -> 7;
		N -> N-1
	end,
	{to_time(H, M), ACDay}.

to_time(H, M) ->
	H*60 + M.

update_state({<<"power">>,<<"on">>}, State) ->
	State#ac_state{power=1};
update_state({<<"fan_select">>,Val}, State) ->
	State#ac_state{fan=binary_to_integer(Val)};
update_state({<<"mode_select">>,Val}, State) ->
	State#ac_state{mode=binary_to_integer(Val)};
update_state({<<"temperature">>,Val}, State) ->
	State#ac_state{temp=binary_to_integer(Val)};
update_state({<<"vert_deflector">>,<<"on">>}, State) ->
	State#ac_state{vert_deflector=?DEFLECTOR_ON};
update_state({<<"horiz_deflector">>,<<"on">>}, State) ->
	State#ac_state{horiz_deflector=?DEFLECTOR_ON};
update_state({<<"comfort">>,<<"on">>}, State) ->
	State#ac_state{comfort=1};
update_state({<<"quiet">>,<<"on">>}, State) ->
	State#ac_state{quiet=1};
update_state({<<"motion_detect">>,<<"on">>}, State) ->
	State#ac_state{motion_detect=1};
update_state({<<"eco">>,<<"on">>}, State) ->
	State#ac_state{eco=1};
update_state(Other, State) ->
	io:fwrite("Unrecognised request string: ~p\n", [Other]),
	State.
