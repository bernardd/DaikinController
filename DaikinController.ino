#include "DaikinController.h"
#include <TimerOne.h>

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
