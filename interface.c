#include "DaikinController.h"

#define STATE_START 0
#define STATE_READ 1

byte state = 
byte readAt = 0;

ACstate acState = {};
ACstate newState = {};

void serialEvent()
{
	while (Serial.available()) {
		char c = Serial.read();
		switch state {
			case STATE_START:
				if (c == 'S')
					send_state();
				else if (c == 'C')
					state = STATE_READ;
				// Ignore anything else - it's invalid
				break;
			case STATE_READ:
				*((char*)(&newState) + readAt++) = c;
				if (readAt == sizeof(ACstate))

					break;
		}
	}
}
