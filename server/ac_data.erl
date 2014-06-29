-module(ac_data).

-compile(export_all).

-include("rc_server.hrl").

-define(S(N,K,V), #setting{name = N, key = K, value = V}).

% Day of week
dow() ->
	[
		?S("Sunday", 'DOW_SUN', 1),
		?S("Monday", 'DOW_MON', 2),
		?S("Tuesday", 'DOW_TUE', 3),
		?S("Wednesday", 'DOW_WED', 4),
		?S("Thursday", 'DOW_THU', 5),
		?S("Friday", 'DOW_FRI', 6),
		?S("Saturday", 'DOW_SAT', 7)
	].

% Modes
modes() ->
	[
		?S("Automatic", 'MODE_AUTO', 0),
		?S("Dry", 'MODE_DRY',  2),
		?S("Cool", 'MODE_COOL', 3),
		?S("Heat", 'MODE_HEAT', 4),
		?S("Fan", 'MODE_FAN',  6)
	].

% Deflectors
deflector() ->
	[
		?S("Off", 'DEFLECTOR_OFF', 0),
		?S("On", 'DEFLECTOR_ON', ?DEFLECTOR_ON)
	].

% Fan
fan() ->
	[
		?S("Automatic", 'FAN_AUTO',  16#A),
		?S("Quiet", 'FAN_QUIET', 16#B),
		?S("1", 'FAN_1',     3),
		?S("2", 'FAN_2',     4),
		?S("3", 'FAN_3',     5),
		?S("4", 'FAN_4',     6),
		?S("5", 'FAN_5',     7)
	].
