#include "DaikinController.h"

#define PULSE_TOLERANCE 200
#define TIMEOUT (INTER_BLOCK_DELAY + BLOCK_START_PULSE + BLOCK_START_OFF + PULSE_TOLERANCE)

#define PULSE pulseIn(IR_RECEIVER, HIGH, TIMEOUT)

#define EXPECT(F) if (!F) {Serial.print("Bailed at line "); Serial.println(__LINE__); return false;}
#define IS_NEAR(T, V) (T > (V-PULSE_TOLERANCE) && T < (V+PULSE_TOLERANCE))
#define EXPECT_PULSE(V) { \
	unsigned long t = PULSE; \
	if (!IS_NEAR(t, V)) { \
		Serial.print("Expected pulse not received at: "); \
		Serial.println(__LINE__); \
		Serial.print(" Got: "); \
		Serial.println(t); \
		return false; \
	} \
}

bool read_block(void *block, size_t size)
{
	EXPECT_PULSE(BLOCK_START_OFF);
	for (size_t i=0; i<size; i++) {
		byte b = 0;
		for (int j=0; j<8; j++) {
			unsigned long t = PULSE;
			if (IS_NEAR(t, ONE_PULSE))
				b |= 1 << j;
			else if (!IS_NEAR(t, ZERO_PULSE)) {
				Serial.print("Expected pulse not received at: ");
				Serial.print(__LINE__);
				Serial.print(" Got: ");
				Serial.println(t);
				return false;
			}
		}
		*((byte *)block+i) = b;
	}
	return true;
}


bool receive() {
	if (digitalRead(IR_RECEIVER)) return false;
	digitalWrite(RED_LED, HIGH);

	Block1 b1;
	Block2 b2;
	Block3 b3;

	for (int i=0; i<5; i++)
		EXPECT_PULSE(PREFIX_PULSE);

	EXPECT_PULSE(START_BLOCK_DELAY);
	EXPECT(read_block(&b1, sizeof(b1)));
	EXPECT_PULSE(INTER_BLOCK_DELAY);
	EXPECT(read_block(&b2, sizeof(b2)));
	EXPECT_PULSE(INTER_BLOCK_DELAY);
	EXPECT(read_block(&b3, sizeof(b3)));
	
	//TODO: Verify checksum!

	ACstate s;
	blocks_to_state(&b1, &b2, &b3, &s);
	update_server_state(&s);
	
	return true;
}
