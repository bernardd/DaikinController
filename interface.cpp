#include <Serial.h>
#include "DaikinController.h"

#define STATE_START 0
#define STATE_READ 1

byte state = STATE_START;
byte readAt = 0;

ACstate acState = {};
ACstate newState = {};

void send_state()
{
	// TODO send state to server
}

void serialEvent()
{
	while (Serial.available()) {
		char c = Serial.read();
		switch (state) {
			case STATE_START:
				if (c == 'S')
					send_state();
				else if (c == 'C')
					state = STATE_READ;
				// Ignore anything else - it's invalid
				break;
			case STATE_READ:
				*((char*)(&newState) + readAt++) = c;
				if (readAt == sizeof(ACstate)) {
					acState = newState;
					send_new_state(&acState);
					readAt = 0;
				}
				break;
		}
	}
}
