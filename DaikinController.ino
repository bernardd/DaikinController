#include "DaikinController.h"
#include <TimerOne.h>
#include <SoftwareSerial.h>

SoftwareSerial xbSerial(2, 3);

void setup() {
	pinMode(IR_LED, OUTPUT);
	pinMode(RED_LED, OUTPUT);
	digitalWrite(IR_LED, LOW);
	digitalWrite(RED_LED, LOW);
	Timer1.initialize(PWM_RATE);

	Serial.begin(9600);
	xbSerial.begin(9600);
	send_state();
}

#define KA_INTERVAL 2000
unsigned int lastKA = 0;

void loop() {
	unsigned int now = millis();
	if ((now - lastKA) >= KA_INTERVAL) {
		xbSerial.write('K');
		lastKA = now;
	}
	if (xbSerial.available())
		handle_input();
}

