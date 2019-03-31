#include "keyboard.h"
#include "ps2.h"


unsigned numpad = 0xFFF;	// Stored state of 12 buttons
unsigned sample = 0xFFF;	// Current sample
int sample_count = 0;
#define COLECO_DEBOUNCE_COUNT 1000
#define HOST_READ_NUMPAD    0xFFFFFFB4
int SampleColecoNumpad() {
	// Debounce by reading at least 100 times the same value.
	unsigned u = 0xFFF & *(volatile unsigned *)HOST_READ_NUMPAD;
	if(u != sample) {
		sample = u;
		sample_count = 0;
	}
	if(sample_count == COLECO_DEBOUNCE_COUNT) {
		return 1;	// we have new key
	} else if(sample_count < COLECO_DEBOUNCE_COUNT+1) {
		sample_count++;
	}
	return 0;	// old state remains
}

//                             1        2            3          4      5
unsigned char coleco_map[] = { KEY_ESC, KEY_UPARROW, KEY_ENTER, KEY_A, KEY_DOWNARROW, KEY_A, KEY_A, KEY_A, KEY_A, KEY_A, KEY_A, KEY_A };

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
	int coleco;

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
		// Find which key changed
		int i;
		unsigned mask = 1;
		for(i=0; i<12;i++) {
			if ((numpad & mask) != (sample & mask)) {
				// This key changed
				keyup = sample & mask; // If non-zero this key was released
				int keyidx = coleco_map[i];
				if(keyup)
					keytable[keyidx>>4]&=~(1<<((keyidx&15)*2));  // Mask off the "currently pressed" bit.
				else
					keytable[keyidx>>4]|=3<<((keyidx&15)*2);	// Currently pressed and pressed since last test.
			}
			mask <<= 1;
		}
		numpad = sample;
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

