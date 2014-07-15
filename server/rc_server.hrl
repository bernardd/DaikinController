-ifndef(_RC_SERVER_HRL_).
-define(_RC_SERVER_HRL_, 1).

-record(ac_state, {
		time,
		turn_on_time,
		turn_off_time,
		comfort,
		day,
		power,
		mode,
		temp,
		vert_deflector,
		fan,
		horiz_deflector,
		powerful,
		quiet,
		motion_detect,
		eco
	}).

-define(MIN_TEMP, 18).
-define(MAX_TEMP, 30).

-define(DEFLECTOR_ON, 16#F).

-define(NO_TIME, 16#600).

-record(setting, {
		name :: string(),
		key :: atom(),
		value :: non_neg_integer()
	}).

-endif. % _RC_SERVER_HRL_
