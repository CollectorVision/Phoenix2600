#include "keyboard.h"
#include "ps2.h"


#define NBUTTONS 17
#define HIGHMASK ((1 << NBUTTONS)-1)
unsigned numpad = HIGHMASK; // Stored state of 12 numeric buttons and 5 gamepad buttons

unsigned state  = HIGHMASK;	// current debounced state of keys
unsigned sample = HIGHMASK;	// Current sample as read from hw. Think about is as the direction where the key is going.
// sample_counts below calculate for how long the "sample" above stays stable.
unsigned short sample_counts[NBUTTONS] = { 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,0 };
unsigned short numpad_counts[NBUTTONS] = { 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,0 }; // for debugging only
#define COLECO_DEBOUNCE_COUNT 100
#define HOST_READ_NUMPAD    0xFFFFFFB4

int SampleColecoNumpad() {
	// Debounce by reading at least 100 times the same value.
	unsigned u = HIGHMASK & *(volatile unsigned *)HOST_READ_NUMPAD;
	int i;
	unsigned mask = 1;
	int ret = 0;
	for(i=0; i<NBUTTONS;i++) {
		if((u & mask) != (sample & mask)) {
			sample_counts[i] = 0;
			sample = (sample & ~mask) | (u & mask);
		}
		if(sample_counts[i] == COLECO_DEBOUNCE_COUNT) {
			// only increment keycount if state and sample differ for this specific key
			if ((state & mask) != (sample & mask))
				ret++;	// we have new key			
			// move the sample to the official key state
			state = (state & ~mask) | (sample & mask);
		} else if(sample_counts[i] < COLECO_DEBOUNCE_COUNT+1) {
			sample_counts[i]++;
		}
		mask <<= 1;
	}
	return ret;	// old state remains
}

unsigned char coleco_map[NBUTTONS] = { 
//  1        2            3          4      5
	KEY_ESC, KEY_A,       KEY_A,     KEY_A, KEY_A,
	///// KEY_ESC, KEY_UPARROW, KEY_ENTER, KEY_A, KEY_DOWNARROW, 
//  6      7       8      9      *      0      #
	KEY_A, KEY_F1, KEY_A, KEY_A, KEY_A, KEY_A, KEY_A,
// Gamepad buttons - bits in hardware register in this order (MSB to LSB direction): Fire Left Right Up Down
	KEY_DOWNARROW, KEY_UPARROW, KEY_PAGEDOWN, KEY_PAGEUP, KEY_ENTER
};

// We maintain a keytable which records which keys are currently pressed
// and which keys have been pressed and possibly released since the last test.
// For this we need 2 bits per key in the keytable.
// We'll use 32-bit ints to store the key statuses
// since that's more convienent for the ZPU.
// index(keycode) = Keycode>>4    (keycode range: 0-255)
// Each 2 bit tuple is shifted by (keycode & 15)*2.

unsigned int keytable[16]={0};

int HandlePS2RawCodes()
{
	int result=0;
	static int keyup=0;
	static int extkey=0;
	int updateleds=0;
	int key;

	while((key=PS2KeyboardRead())>-1)
	{
		if(key==KEY_KEYUP)
			keyup=1;
		else if(key==KEY_EXT)
			extkey=1;
		else
		{
			int keyidx=extkey ? 128+key : key;
			if(keyup)
				keytable[keyidx>>4]&=~(1<<((keyidx&15)*2));  // Mask off the "currently pressed" bit.
			else
				keytable[keyidx>>4]|=3<<((keyidx&15)*2);	// Currently pressed and pressed since last test.
			extkey=0;
			keyup=0;
		}
	}

	if (SampleColecoNumpad()) {
		// Find which key(s) changed
		int i;
		unsigned mask = 1;
		for(i=0; i<NBUTTONS;i++) {
			if ((numpad & mask) != (state & mask)) {
				// This key changed
				int keydown = !(state & mask); // If non-zero this key was pressed
				int keyidx = coleco_map[i];
				if(keydown) {	// key depressed
					keytable[keyidx>>4] |= 3<<((keyidx&15)*2);	// Currently pressed and pressed since last test.
					numpad_counts[i]++;
				} else {
					keytable[keyidx>>4] &= ~(1<<((keyidx&15)*2));  // Mask off the "currently pressed" bit.
				}

				if (keyidx == KEY_F1 && keydown)
					result = 1;	// Used to trigger Start in menu.c 
			}
			mask <<= 1;
		}
		numpad = state;
	}

	return(result);
}


void ClearKeyboard()
{
	int i;
	for(i=0;i<16;++i)
		keytable[i]=0;
}

int TestKey(int rawcode)
{
	int result;
	DisableInterrupts();	// Make sure a new character doesn't arrive halfway through the read
	result=3&(keytable[rawcode>>4]>>((rawcode&15)*2));
	keytable[rawcode>>4]&=~(2<<((rawcode&15)*2));	// Mask off the "pressed since last test" bit.
	EnableInterrupts();
	return(result);
}

