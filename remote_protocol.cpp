#include <TimerOne.h>
#include "DaikinController.h"

const byte b1header[6] = {0x11, 0xDA, 0x27, 0, 0xC5, 0};
const byte b2header[5] = {0x11, 0xDA, 0x27, 0, 0x42};
const byte b3header[5] = {0x11, 0xDA, 0x27, 0, 0};

void on_pulse() {
	PWM_ON;
	delayMicroseconds(ON_PULSE);
	PWM_OFF;
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

void send_byte(byte b) {
	for (int i=0; i<8; i++)
		send(b & (1<<i));
}

unsigned int make_time(unsigned int hours, unsigned int mins)
{
	return (hours*60) + mins;
}

void test_message(Block1 *b1, Block2 *b2, Block3 *b3)
{
	b2->time=make_time(7, 0);
	b2->day=DOW_SUN;

	b3->power=1;
	b3->mode = MODE_AUTO;
	b3->temp = 23;
	b3->vert_deflector = DEFLECTOR_ON;
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

void state_to_blocks(ACstate *s, Block1 *b1, Block2 *b2, Block3 *b3)
{
	b1->comfort = s->comfort;
	b2->time = s->time;
	b2->day = s->day;
	b3->power = s->power;
	b3->mode = s->mode;
	b3->temp = s->temp;
	b3->vert_deflector = s->vert_deflector;
	b3->fan = s->fan;
	b3->horiz_deflector = s->horiz_deflector;
	b3->powerful = s->powerful;
	b3->quiet = s->quiet;
	b3->motion_detect = s->motion_detect;
	b3->eco = s->eco;
	b3->timer = s->timer;
}

void blocks_to_state(Block1 *b1, Block2 *b2, Block3 *b3, ACstate *s)
{
	s->comfort = b1->comfort;
	s->time = b2->time;
	s->day = b2->day;
	s->power = b3->power;
	s->mode = b3->mode;
	s->temp = b3->temp;
	s->vert_deflector = b3->vert_deflector;
	s->fan = b3->fan;
	s->horiz_deflector = b3->horiz_deflector;
	s->turn_on_time = b3->turn_on_time;
	s->turn_off_time = b3->turn_off_time;
	s->powerful = b3->powerful;
	s->quiet = b3->quiet;
	s->motion_detect = b3->motion_detect;
	s->eco = b3->eco;
	s->timer = b3->timer;
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

void send_message(Block1 *b1, Block2 *b2, Block3 *b3)
{
	digitalWrite(RED_LED, HIGH);
	Serial.println("Writing out state");
	calc_checksum((byte*)b1, sizeof(Block1));
	calc_checksum((byte*)b2, sizeof(Block2));
	calc_checksum((byte*)b3, sizeof(Block3));

	// Message opens with 5 leading 0s then a 26ms gap
	for (int i=0; i<5; i++)
		send(0);
	on_pulse();
	delay(START_BLOCK_DELAY / 1000);

	send_block((byte*)b1, sizeof(Block1));
	delay(INTER_BLOCK_DELAY / 1000);

	send_block((byte*)b2, sizeof(Block2));
	delay(INTER_BLOCK_DELAY / 1000);

	send_block((byte*)b3, sizeof(Block3));
	digitalWrite(RED_LED, LOW);
}

void init_b1(Block1 *b1) {
	memset(b1, 0, sizeof(Block1));
	memcpy(b1->header, b1header, sizeof(b1->header));
}

void init_b2(Block2 *b2) {
	memset(b2, 0, sizeof(Block2));
	memcpy(b2->header, b2header, sizeof(b2->header));
}

void init_b3(Block3 *b3) {
	memset(b3, 0, sizeof(Block3));
	memcpy(b3->header, b3header, sizeof(b3->header));
	b3->pad = 0x6;
}

void print_state(ACstate *s)
{
	Serial.print("comfort: ");
	Serial.println(s->comfort);
	Serial.print("time: ");
	Serial.println(s->time);
	Serial.print("day: ");
	Serial.println(s->day);
	Serial.print("power: ");
	Serial.println(s->power);
	Serial.print("mode: ");
	Serial.println(s->mode);
	Serial.print("temp: ");
	Serial.println(s->temp);
	Serial.print("vert_deflector: ");
	Serial.println(s->vert_deflector);
	Serial.print("fan: ");
	Serial.println(s->fan);
	Serial.print("horiz_deflector: ");
	Serial.println(s->horiz_deflector);
	Serial.print("turn_on_time: ");
	Serial.println(s->turn_on_time);
	Serial.print("turn_off_time: ");
	Serial.println(s->turn_off_time);
	Serial.print("powerful: ");
	Serial.println(s->powerful);
	Serial.print("quiet: ");
	Serial.println(s->quiet);
	Serial.print("motion_detect: ");
	Serial.println(s->motion_detect);
	Serial.print("eco: ");
	Serial.println(s->eco);
	Serial.print("timer: ");
	Serial.println(s->timer);
}

void send_state_to_ac(ACstate *s)
{
	Serial.println("Sending state to AC");
	print_state(s);

	Block1 b1;
	Block2 b2;
	Block3 b3;

	init_b1(&b1);
	init_b2(&b2);
	init_b3(&b3);

	state_to_blocks(s, &b1, &b2, &b3);

	send_message(&b1, &b2, &b3);
}


