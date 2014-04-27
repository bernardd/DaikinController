#define RED_LED 13
#define LED 11

#define LED_LAG 300

#define ZERO_PULSE (170 + LED_LAG)
#define ONE_PULSE (1060 + LED_LAG)
#define ON_PULSE (700 - LED_LAG)
#define PWM_DUTY 255

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
  byte header[5]; //
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
  pinMode(LED, OUTPUT);
  pinMode(RED_LED, OUTPUT);
  digitalWrite(LED, LOW);
  digitalWrite(RED_LED, LOW);
  Serial.begin(115200);
  Serial.println("Initialised");
}

void loop() {
  digitalWrite(RED_LED, HIGH);
  
  block1 b1;
  block2 b2;
  block3 b3;

  init_b1(&b1);
  init_b2(&b2);
  init_b3(&b3);
 
  send_message(&b1, &b2, &b3);
  
  digitalWrite(RED_LED, LOW);
  delay(5000);
}

void send_message(struct block1 *b1, struct block2 *b2, struct block3 *b3)
{
  calc_checksum((byte*)b1, sizeof(struct block1));
  calc_checksum((byte*)b2, sizeof(struct block2));
  calc_checksum((byte*)b3, sizeof(struct block3));
  
  // Message opens with 5 leading 0s then a 25ms gap
  for (int i=0; i<5; i++)
    send(0);
  on_pulse();
  delay(26);
  
  send(1);
  send_block((byte*)b1, sizeof(struct block1));
  delay(34);

  send(1);
  send_block((byte*)b2, sizeof(struct block2));
  delay(34);

  send(1);
  send_block((byte*)b3, sizeof(struct block3));
}

void send_block(byte *b, unsigned int s)
{
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

void on_pulse() {
  // {Possibly TODO: pulse at carrier rate (38kHz?)
  analogWrite(LED, PWM_DUTY);
  delayMicroseconds(ON_PULSE);
  digitalWrite(LED, LOW);
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

// Simple modular-sum checksum
void calc_checksum(byte *d, unsigned int s) {
  byte sum = 0;
  for (int i=0; i<s-1; i++) {
    sum += *d;
    d++;
  }
  *d = sum;
}
