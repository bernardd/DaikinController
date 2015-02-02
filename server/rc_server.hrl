-ifndef(_RC_SERVER_HRL_).
-define(_RC_SERVER_HRL_, 1).

-define(DEFAULT_TIME, "00:00").

-record(ac_state, {
		time = ?DEFAULT_TIME,
		turn_on_time = ?DEFAULT_TIME,
		turn_off_time = ?DEFAULT_TIME,
		comfort = 0,
		day = 0,
		power = 0,
		mode = 0,
		temp = 0,
		vert_deflector = 0,
		fan = 0,
		horiz_deflector = 0,
		powerful = 0,
		quiet = 0,
		motion_detect = 0,
		eco = 0
	}).

-define(STATE_FIELDS, record_info(fields, ac_state)).

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
