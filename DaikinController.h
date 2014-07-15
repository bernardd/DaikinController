#ifndef _DAIKINCONTROLLER_H_
#define _DAIKINCONTROLLER_H_

#include <Arduino.h>

#define IR_RECEIVER 4
#define IR_LED 9
#define RED_LED 13

#define PWM_RATE 27 // us interval
#define PWM_DUTY 379 // ~37% (100% = 1023)
#define PWM_ON Timer1.pwm(IR_LED, PWM_DUTY, PWM_RATE)
#define PWM_OFF Timer1.disablePwm(IR_LED);


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
#define NO_TEMP 0x60

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

typedef struct {
	unsigned int time;
	unsigned int turn_on_time;
	unsigned int turn_off_time;
	byte comfort;
	byte day;
	byte power;
	byte mode;
	byte temp;
	byte vert_deflector;
	byte fan;
	byte horiz_deflector;
	byte powerful;
	byte quiet;
	byte motion_detect;
	byte eco;
} ACstate;

typedef struct Block1 {
	byte header[6];
	byte pad1:4; // 0
	byte comfort:1;
	byte pad2:3; // 0
	byte checksum;
} Block1;

typedef struct Block2 {
	byte header[5];
	unsigned int time:11;
	byte day:3;
	byte pad:2;
	byte checksum;
} Block2;

typedef struct Block3 {
	byte header[5];
	byte power:1;
	byte on_timer:1;
	byte off_timer:1;
	byte pad:1;  // 1
	byte mode:3;
	byte pad2:2; // 0
	byte temp:7;
	byte pad3;   // 0
	byte vert_deflector:4;
	byte fan:4;
	byte horiz_deflector:4;
	byte pad4:4; // 0
	unsigned int turn_on_time:11;
	byte pad5:1; // 0
	unsigned int turn_off_time:11;
	byte pad6:1; // 0
	byte powerful:1;
	byte pad7:4; // 0
	byte quiet:1;
	byte pad8:2;
	byte pad9[2];
	byte pad10:1; // 0
	byte motion_detect:1;
	byte eco:1;
	byte pad11:4;
	byte program:1; // INVERTED: 1 when no program is present, 0 when one is.
	byte pad12; // 0
	byte checksum;
} Block3;

void handle_input();
bool receive();
void blocks_to_state(Block1 *b1, Block2 *b2, Block3 *b3, ACstate *s);
void update_server_state(ACstate *s);
void send_state_to_server();
void send_state_to_ac(ACstate *s);

// Pulse times (microseconds)
#define PREFIX_PULSE 475
#define ZERO_PULSE 440
#define ONE_PULSE 1500
#define ON_PULSE 440
#define BLOCK_START_PULSE 3500
#define BLOCK_START_OFF 2000
#define START_BLOCK_DELAY 25300 // Adjusted for receiving - was 25000 for sending
#define INTER_BLOCK_DELAY 34800 // Adjusted for receiving - was 34000

#endif // _DAIKINCONTROLLER_H_
