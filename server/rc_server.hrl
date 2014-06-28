-ifndef(_RC_SERVER_HRL_).
-define(_RC_SERVER_HRL_, 1).

-record(ac_state, {
		comfort,
		time,
		day,
		power,
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

-define(MIN_TEMP, 18).
-define(MAX_TEMP, 30).

-record(setting, {
		name :: string(),
		key :: atom(),
		value :: non_neg_integer()
	}).

-endif. % _RC_SERVER_HRL_
