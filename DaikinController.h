#ifndef _DAIKINCONTROLLER_H_
#define _DAIKINCONTROLLER_H_

#include <Arduino.h>

#define IR_LED 9
#define RED_LED 13

#define PWM_RATE 27 // us interval
#define PWM_DUTY 379 // ~37% (100% = 1023)
#define PWM_ON Timer1.pwm(IR_LED, PWM_DUTY, PWM_RATE)
#define PWM_OFF Timer1.disablePwm(9);


// Day of week
#define DOW_SUN 1
#define DOW_MON 2
#define DOW_TUE 3
#define DOW_WED 4
#define DOW_THU 5
#define DOW_FRI 6
#define DOW_SAT 7

// Modes
#define MODE_AUTO 0
#define MODE_DRY  2
#define MODE_COOL 3
#define MODE_HEAT 4
#define MODE_FAN  6

// Deflectors
#define DEFLECTOR_OFF 0
#define DEFLECTOR_ON 0xF

// Fan
#define FAN_AUTO  0xA
#define FAN_QUIET 0xB
#define FAN_1     0x3
#define FAN_2     0x4
#define FAN_3     0x5
#define FAN_4     0x6
#define FAN_5     0x7

// Scheduled time no-op
#define NO_TIME 0x600

// Day of week
#define DOW_SUN 1
#define DOW_MON 2
#define DOW_TUE 3
#define DOW_WED 4
#define DOW_THU 5
#define DOW_FRI 6
#define DOW_SAT 7

// Modes
#define MODE_AUTO 0
#define MODE_DRY  2
#define MODE_COOL 3
#define MODE_HEAT 4
#define MODE_FAN  6

// Deflectors
#define DEFLECTOR_OFF 0
#define DEFLECTOR_ON 0xF

// Fan
#define FAN_AUTO  0xA
#define FAN_QUIET 0xB
#define FAN_1     0x3
#define FAN_2     0x4
#define FAN_3     0x5
#define FAN_4     0x6
#define FAN_5     0x7

#define NO_TIME 0x600

typedef struct ACstate {
	byte comfort:1;
	unsigned int time:11;
	byte day:3;
	byte power:1;
	byte mode:3;
	byte temp:7;
	byte vert_deflector:4;
	byte fan:4;
	byte horiz_deflector:4;
	unsigned int turn_on_time:11;
	unsigned int turn_off_time:11;
	byte powerful:1;
	byte quiet:1;
	byte motion_detect:1;
	byte eco:1;
	byte timer:1; // 61 bits
} ACstate;

void send_new_state(ACstate *s);

#endif // _DAIKINCONTROLLER_H_
