#include <SoftwareSerial.h>
#include "DaikinController.h"

#define STATE_START 0
#define STATE_READ 1

byte state = STATE_START;
byte readAt = 0;

ACstate acState = {};
ACstate newState = {};

extern SoftwareSerial xbSerial;

void update_server_state(ACstate *s)
{
	acState = *s;
	send_state_to_server();
}

void send_state_to_server()
{
	unsigned short t;
	xbSerial.write('S');
	xbSerial.write(acState.comfort);
	t = acState.time;
	xbSerial.write((unsigned char*)&t, 2);
	xbSerial.write(acState.day);
	xbSerial.write(acState.power);
	xbSerial.write(acState.mode);
	xbSerial.write(acState.temp);
	xbSerial.write(acState.vert_deflector);
	xbSerial.write(acState.fan);
	xbSerial.write(acState.horiz_deflector);
	t = acState.turn_on_time;
	xbSerial.write((unsigned char*)&t, 2);
	t = acState.turn_off_time;
	xbSerial.write((unsigned char*)&t, 2);
	xbSerial.write(acState.powerful);
	xbSerial.write(acState.quiet);
	xbSerial.write(acState.motion_detect);
	xbSerial.write(acState.eco);
	xbSerial.write(acState.timer);
	xbSerial.flush();
}

void handle_input()
{
	while (xbSerial.available()) {
		char c = xbSerial.read();
		switch (state) {
			case STATE_START:
				if (c == 'S')
					send_state_to_server();
				else if (c == 'C')
					state = STATE_READ;
				// Ignore anything else - it's invalid
				break;
			case STATE_READ:
				*((char*)(&newState) + readAt++) = c;
				if (readAt == sizeof(ACstate)) {
					acState = newState;
					send_state_to_ac(&acState);
					readAt = 0;
					state = STATE_START;
				}
				break;
		}
	}
}
