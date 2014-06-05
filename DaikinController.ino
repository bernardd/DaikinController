#include <Arduino.h>
#include <TimerOne.h>
#include "DaikinController.h"

// Pulse times (microseconds)
#define ZERO_PULSE 440
#define ONE_PULSE 1300
#define ON_PULSE 440
#define BLOCK_START_PULSE 3500
#define BLOCK_START_OFF 1740

#define PWM_RATE 27 // us interval
#define PWM_DUTY 379 // ~37% (100% = 1023)
#define PWM_ON Timer1.pwm(IR_LED, PWM_DUTY, PWM_RATE)
#define PWM_OFF Timer1.disablePwm(9);

const byte b1header[6] = {0x11, 0xDA, 0x27, 0, 0xC5, 0};
const byte b2header[5] = {0x11, 0xDA, 0x27, 0, 0x42};
const byte b3header[5] = {0x11, 0xDA, 0x27, 0, 0};

struct block1 {
	byte header[6];
	byte pad1:4; // 0
	byte comfort:1;
	byte pad2:3; // 0
	byte checksum;
};

struct block2 {
	byte header[5];
	byte time:11;
	byte day:3;
	byte pad:2;
	byte checksum;
};

struct block3 {
	byte header[5];
	byte power:1;
	byte pad:3;  // 0x6
	byte mode:3;
	byte pad2:2; // 0
	byte temp:7;
	byte pad3;   // 0
	byte vert_defelector:4;
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
	byte timer:1;
	byte pad12; // 0
	byte checksum;
};

void setup() {
	pinMode(IR_LED, OUTPUT);
	pinMode(RED_LED, OUTPUT);
	digitalWrite(IR_LED, LOW);
	digitalWrite(RED_LED, LOW);
	Timer1.initialize(PWM_RATE);

	Serial.begin(9600);
}

void loop() {
	digitalWrite(RED_LED, HIGH);

	digitalWrite(RED_LED, LOW);
}

void send_new_state(ACstate &s)
{
	block1 b1;
	block2 b2;
	block3 b3;

	init_b1(&b1);
	init_b2(&b2);
	init_b3(&b3);

	state_to_blocks(s, &b1, &b2, &b3);

	send_message(&b1, &b2, &b3);
}

void state_to_blocks(ACstate *s, block1 *b1, block2 *b2, block3 *b3)
{
	b1->comfort = s->comfort;
	b2->time = s->time;
	b2->day = s->day;
	b3->power = s->power;
	b3->mode = s->mode;
	b3->temp = s->temp;
	b3->vert_defelector = s->vert_defelector;
	b3->fan = s->fan
	b3->horiz_deflector = s->horiz_deflector;
	b3->powerful = s->powerful;
	b3->quiet = s->quiet;
	b3->motion_detect = s->motion_detect;
	b3->eco = s->eco;
	b3->timer = s->timer;
}

void send_message(struct block1 *b1, struct block2 *b2, struct block3 *b3)
{
	calc_checksum((byte*)b1, sizeof(struct block1));
	calc_checksum((byte*)b2, sizeof(struct block2));
	calc_checksum((byte*)b3, sizeof(struct block3));

	// Message opens with 5 leading 0s then a 26ms gap
	for (int i=0; i<5; i++)
		send(0);
	on_pulse();
	delay(25);

	send_block((byte*)b1, sizeof(struct block1));
	delay(34);

	send_block((byte*)b2, sizeof(struct block2));
	delay(34);

	send_block((byte*)b3, sizeof(struct block3));
}

void send_block(byte *b, unsigned int s)
{
	start_pulse();
	delayMicroseconds(BLOCK_START_OFF);
	for (int i=0; i<s; i++) {
		for (int j=0; j<8; j++) {
			send(*b & (1 << j));
		}
		b++;
	}
	on_pulse();
}

void send(boolean b) {
	on_pulse();
	if (!b)
		delayMicroseconds(ZERO_PULSE);
	else
		delayMicroseconds(ONE_PULSE);
}

void start_pulse() {
	PWM_ON;
	delayMicroseconds(BLOCK_START_PULSE);
	PWM_OFF;
}

void on_pulse() {
	PWM_ON;
	delayMicroseconds(ON_PULSE);
	PWM_OFF;
}

void send_byte(byte b) {
	for (int i=0; i<8; i++)
		send(b & (1<<i));
}

void init_b1(struct block1 *b1) {
	memset(b1, 0, sizeof(struct block1));
	memcpy(b1->header, b1header, sizeof(b1->header));
}

void init_b2(struct block2 *b2) {
	memset(b2, 0, sizeof(block2));
	memcpy(b2->header, b2header, sizeof(b2->header));
}

void init_b3(struct block3 *b3) {
	memset(b3, 0, sizeof(struct block3));
	memcpy(b3->header, b3header, sizeof(b3->header));
	b3->pad = 0x6;
}

void test_message(struct block1 *b1, struct block2 *b2, struct block3 *b3)
{
	b2->time=make_time(7, 0);
	b2->day=DOW_SUN;

	b3->power=1;
	b3->mode = MODE_AUTO;
	b3->temp = 23;
	b3->vert_defelector = DEFLECTOR_ON;
	b3->horiz_deflector = DEFLECTOR_ON;
	b3->fan = FAN_AUTO;
	b3->turn_off_time = NO_TIME;
	b3->turn_on_time = NO_TIME;
}

// Simple 8-bit modular-sum checksum
void calc_checksum(byte *d, unsigned int s) {
	byte sum = 0;
	for (int i=0; i<s-1; i++) {
		sum += *d;
		d++;
	}
	*d = sum;
}

unsigned int make_time(unsigned int hours, unsigned int mins)
{
	return (hours*60) + mins;
}

