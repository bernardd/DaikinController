-module(rc_server).

-compile(export_all).

-define(DEFAULT_PORT, 9750).

-type ac_time() :: [0..60*23+59].

-record(ac_state, {
		comfort :: [0..1],
		time :: ac_time(),
		day :: [1..7],
		power :: [0..1],
		mode,
		temp,
		vert_deflector,
		fan,
		horiz_deflector,
		turn_on_time,
		turn_off_time,
		powerful,
		quiet,
		motion_detect,
		eco,
		timer
	}).

start() -> start(?DEFAULT_PORT).

start(Port) ->
	register(?MODULE, spawn(fun() -> init(Port) end)).

stop() ->
	exit(whereis(?MODULE), kill).

init(Port) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {nodelay, true}]),
	io:fwrite("Listening on socket.~n", []),
	listen(Socket).

listen(Socket) ->
	{ok, NewSocket} = gen_tcp:accept(Socket),
	Pid = spawn_link(fun() -> server() end),
	io:fwrite("Socket connected on ~p~n", [Pid]),
	gen_tcp:controlling_process(NewSocket, Pid),
	Pid ! {socket, NewSocket},
	listen(Socket).

server() ->
	put(state, #ac_state{}),
	receive
		{socket, Socket} ->
			gen_tcp:send(Socket, <<"S">>),
			server_loop(Socket, <<>>)
	end.

server_loop(Socket, DataSoFar) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Data} ->
			io:fwrite("Got Data: ~p~n", [Data]),
			case handle_data(<<DataSoFar/binary, Data/binary>>) of
				stop ->
					ok;
				Tail ->
					server_loop(Socket, Tail)
			end;
		{tcp_closed, Socket} ->
			io:fwrite("Socket closed.~n", []);
		{tcp_error, Socket, Reason} ->
			io:fwrite("Socket error: ~p~n", [Reason]);
		M ->
			io:fwrite("Unexpected message: ~p~n", [M])
	end.

handle_data(D = <<"D", Rest/binary>>) ->
	case binary:split(Rest, <<"\n">>) of
		[Rest] -> D;
		[String, Tail] ->
			io:fwrite("Debug received: ~p~n", [String]),
			handle_data(Tail)
	end;

handle_data(<<"S", Comfort:1, Time:11, Day:3, Power:1, Mode:3, Temp:7, VertDef:4, Fan:4, HoizDef:4, TurnOn:11, TurnOff:11, 
		Powerful:1, Quiet:1, MotionDetect:1, Eco:1, Timer:1, _Pad:7, Rest/binary>>) ->
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
		eco = Eco,
		timer = Timer
	},
	io:fwrite("Got new state: ~p~n", [NewState]),
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
