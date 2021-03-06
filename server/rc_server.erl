-module(rc_server).

-compile(export_all).

-include("rc_server.hrl").

-define(DEFAULT_PORT, 9750).

start() -> start(?DEFAULT_PORT).

start(Port) ->
	register(?MODULE, spawn(fun() -> init_handler(Port) end)).

stop() ->
	exit(whereis(?MODULE), kill).

get_state() ->
	?MODULE ! {get_state, self()},
	receive
		{Type, S} when Type =:= rc_state orelse Type =:= rc_state_cached -> {Type, S}
	after 500 ->
		throw({error, timeout})
	end.

set_state(State) ->
	?MODULE ! {set_state, State}.

init_listener(Port) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {nodelay, true}]),
	io:fwrite("Listening on socket.~n", []),
	listen(Socket).

listen(Socket) ->
	{ok, NewSocket} = gen_tcp:accept(Socket),
	io:fwrite("Connection accepted.~n", []),
	gen_tcp:controlling_process(NewSocket, whereis(?MODULE)),
	?MODULE ! {new_socket, NewSocket},
	listen(Socket).

init_handler(Port) ->
	spawn_link(fun() -> init_listener(Port) end),
	put(state, #ac_state{}),
	server_idle().

server_idle() ->
	receive
		{new_socket, Socket} ->
			handle_new_socket(Socket);
		{get_state, ReplyTo} ->
			ReplyTo ! {rc_state_cached, get(state)},
			server_idle();
		{set_state, From, _State} ->
			From ! {error, no_connection},
			server_idle();
		M ->
			io:fwrite("Unexpected message: ~p~n", [M]),
			server_idle()
	end.

server_active(Socket, DataSoFar) ->
	receive
		{tcp, Socket, Data} ->
%			io:fwrite("Got Data: ~p~n", [Data]),
			case handle_data(<<DataSoFar/binary, Data/binary>>) of
				stop ->
					gen_tcp:close(Socket),
					server_idle();
				Tail ->
					inet:setopts(Socket, [{active, once}]),
					server_active(Socket, Tail)
			end;
		{tcp_closed, Socket} ->
			io:fwrite("Socket closed.~n", []),
			server_idle();
		{tcp_error, Socket, Reason} ->
			io:fwrite("Socket error: ~p~n", [Reason]),
			server_idle();
		{get_state, ReplyTo} ->
			ReplyTo ! {rc_state, get(state)},
			server_active(Socket, DataSoFar);
		{set_state, State} ->
			Packet = state_to_packet(State),
			gen_tcp:send(Socket, Packet),
			put(state, State),
			server_active(Socket, DataSoFar);
		{new_socket, NewSocket} ->
			gen_tcp:close(Socket),
			handle_new_socket(NewSocket);
		M ->
			io:fwrite("Unexpected message: ~p~n", [M]),
			server_active(Socket, DataSoFar)
	end.

handle_new_socket(Socket) ->
	gen_tcp:send(Socket, <<"S">>),
	inet:setopts(Socket, [{active, once}]),
	server_active(Socket, <<>>).

state_to_packet(State) ->
	io:fwrite("State to packet: ~p\n", [State]),
	<<"C",
		(State#ac_state.time):16/little,
		(State#ac_state.turn_on_time):16/little,
		(State#ac_state.turn_off_time):16/little,
		(State#ac_state.comfort),
		(State#ac_state.day),
		(State#ac_state.power),
		(State#ac_state.mode),
		(State#ac_state.temp),
		(State#ac_state.vert_deflector),
		(State#ac_state.fan),
		(State#ac_state.horiz_deflector),
		(State#ac_state.powerful),
		(State#ac_state.quiet),
		(State#ac_state.motion_detect),
		(State#ac_state.eco)
		>>.

handle_data(D = <<"D", Rest/binary>>) ->
	case binary:split(Rest, <<"\n">>) of
		[Rest] -> D;
		[String, Tail] ->
			io:fwrite("Debug received: ~p~n", [String]),
			handle_data(Tail)
	end;

handle_data(<<"S", Time:16/little, TurnOn:16/little, TurnOff:16/little, Comfort, Day, Power, Mode, Temp, VertDef, Fan, HoizDef,
		Powerful, Quiet, MotionDetect, Eco, Rest/binary>>) ->
	NewState = #ac_state{
		comfort = Comfort,
		time = Time,
		day = Day,
		power = Power,
		mode = Mode,
		temp = Temp,
		vert_deflector = VertDef,
		fan = Fan,
		horiz_deflector = HoizDef,
		turn_on_time = TurnOn,
		turn_off_time = TurnOff,
		powerful = Powerful,
		quiet = Quiet,
		motion_detect = MotionDetect,
		eco = Eco
	},
	io:fwrite("Got new state: ~s~n", [format_state(NewState)]),
	put(state, NewState),
	handle_data(Rest);

handle_data(Data = <<"S", _Rest/binary>>) ->
	Data;

handle_data(<<"K", Rest/binary>>) ->
	% Do nothing for a keepalive
	handle_data(Rest);

handle_data(<<"R", _Rest/binary>>) ->
	stop;

handle_data(<<>>) ->
	<<>>;

handle_data(Data) ->
	io:fwrite("Unexpected data - closing connection:~n~p~n", [Data]),
	stop.

p_error(I) -> "Error (" ++ integer_to_list(I) ++ ")".

p_toggle(0) -> "Off";
p_toggle(1) -> "On";
p_toggle(I) -> p_error(I).

p_time(16#600) -> "No Time";
p_time(T) ->
	H = T div 60,
	M = T rem 60,
	io_lib:format("~B:~2..0B", [H, M]).

p_day(1) -> "Sunday";
p_day(2) -> "Monday";
p_day(3) -> "Tuesday";
p_day(4) -> "Wednesday";
p_day(5) -> "Thursday";
p_day(6) -> "Friday";
p_day(7) -> "Saturday";
p_day(I) -> p_error(I).

p_mode(0) -> "Auto";
p_mode(2) -> "Dry";
p_mode(3) -> "Cool";
p_mode(4) -> "Heat";
p_mode(6) -> "Fan";
p_mode(I) -> p_error(I).

p_deflector(0) -> "Off";
p_deflector(16#F) -> "On";
p_deflector(I) -> p_error(I).

p_fan(16#A) -> "Auto";
p_fan(16#B) -> "Quiet";
p_fan(3) -> "1";
p_fan(4) -> "2";
p_fan(5) -> "3";
p_fan(6) -> "4";
p_fan(7) -> "5";
p_fan(I) -> p_error(I).

format_state(State) ->
	"Comfort: " ++ p_toggle(State#ac_state.comfort) ++ "\n" ++
	"Time: " ++ p_time(State#ac_state.time) ++ "\n" ++
	"Day: " ++ p_day(State#ac_state.day) ++ "\n" ++
	"Power: " ++ p_toggle(State#ac_state.power) ++ "\n" ++
	"Mode: " ++ p_mode(State#ac_state.mode) ++ "\n" ++
	"Temp: " ++ integer_to_list(State#ac_state.temp) ++ "\n" ++
	"Vert_deflector: " ++ p_deflector(State#ac_state.vert_deflector) ++ "\n" ++
	"Fan: " ++ p_fan(State#ac_state.fan) ++ "\n" ++
	"Horiz_deflector: " ++ p_deflector(State#ac_state.horiz_deflector) ++ "\n" ++
	"Turn_on_time: " ++ p_time(State#ac_state.turn_on_time) ++ "\n" ++
	"Turn_off_time: " ++ p_time(State#ac_state.turn_off_time) ++ "\n" ++
	"Powerful: " ++ p_toggle(State#ac_state.powerful) ++ "\n" ++
	"Quiet: " ++ p_toggle(State#ac_state.quiet) ++ "\n" ++
	"Motion_detect: " ++ p_toggle(State#ac_state.motion_detect) ++ "\n" ++
	"Eco: " ++ p_toggle(State#ac_state.eco) ++ "\n".
