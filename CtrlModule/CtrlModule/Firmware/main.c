#include "host.h"

#include "osd.h"
#include "keyboard.h"
#include "menu.h"
#include "ps2.h"
#include "minfat.h"
#include "spi.h"
#include "fileselector.h"

fileTYPE file;

void Debug(int row);
void DebugCounterReset(int row);

int OSD_Puts(char *str)
{
	int c;
	while((c=*str++))
		OSD_Putchar(c);
	return(1);
}

/*
void TriggerEffect(int row)
{
	int i,v;
	Menu_Hide();
	for(v=0;v<=16;++v)
	{
		for(i=0;i<4;++i)
			PS2Wait();

		HW_HOST(REG_HOST_SCALERED)=v;
		HW_HOST(REG_HOST_SCALEGREEN)=v;
		HW_HOST(REG_HOST_SCALEBLUE)=v;
	}
	Menu_Show();
}
*/
void Delay()
{
	int c=16384; // delay some cycles
	while(c)
	{
		c--;
	}
}
void Reset(int row)
{
	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_RESET|HOST_CONTROL_DIVERT_KEYBOARD; // Reset host core
	Delay();
	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_DIVERT_KEYBOARD;
}

void Select(int row)
{
	
	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_SELECT|HOST_CONTROL_DIVERT_KEYBOARD; // Send select
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_DIVERT_KEYBOARD;
}

void Start(int row)
{
	
	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_START|HOST_CONTROL_DIVERT_KEYBOARD; // Send start
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();Delay();
	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_DIVERT_KEYBOARD;
}

void Boot(int row) {
	// Switch databus to loaded ROM, Reset
	// Value bit 1 below arms debug capture when reset is issued
	HW_HOST(REG_HOST_MUXCTRL) = 2 | (MENU_TOGGLE_VALUES & (1 << 6) ? 1 : 0);
	Reset(0);
	Delay();
	// Return to pacman, no capture.
//	HW_HOST(REG_HOST_MUXCTRL) = 0;
//	Reset(0);
}

static struct menu_entry topmenu[]; // Forward declaration.
/*
// RGB scaling submenu
static struct menu_entry rgbmenu[]=
{
	{MENU_ENTRY_SLIDER,"Red",MENU_ACTION(16)},
	{MENU_ENTRY_SLIDER,"Green",MENU_ACTION(16)},
	{MENU_ENTRY_SLIDER,"Blue",MENU_ACTION(16)},
	{MENU_ENTRY_SUBMENU,"Exit",MENU_ACTION(topmenu)},
	{MENU_ENTRY_NULL,0,0}
};


// Test pattern names
static char *testpattern_labels[]=
{
	"Test pattern 1",
	"Test pattern 2",
	"Test pattern 3",
	"Test pattern 4"
};
*/
// Our toplevel menu
char debug_title[68];

static struct menu_entry topmenu[]=
{
  {MENU_ENTRY_CALLBACK,debug_title,MENU_ACTION(&DebugCounterReset)},
  {MENU_ENTRY_CALLBACK,"Debug",MENU_ACTION(&Debug)},
	{MENU_ENTRY_CALLBACK,"Reset",MENU_ACTION(&Reset)},
//	{MENU_ENTRY_CYCLE,(char *)testpattern_labels,MENU_ACTION(4)},
//	{MENU_ENTRY_SUBMENU,"RGB Scaling \x10",MENU_ACTION(rgbmenu)},
	{MENU_ENTRY_TOGGLE,"Scanlines",MENU_ACTION(0)},
	{MENU_ENTRY_TOGGLE,"PAL / NTSC",MENU_ACTION(1)},
	{MENU_ENTRY_TOGGLE,"Color",MENU_ACTION(2)},
	{MENU_ENTRY_TOGGLE,"Difficulty A",MENU_ACTION(3)},
	{MENU_ENTRY_TOGGLE,"Difficulty B",MENU_ACTION(4)},
//	{MENU_ENTRY_TOGGLE,"Verify",MENU_ACTION(5)},	
	{MENU_ENTRY_TOGGLE,"ROM",MENU_ACTION(6)},	
	{MENU_ENTRY_CALLBACK,"boot",MENU_ACTION(&Boot)},	
	{MENU_ENTRY_CALLBACK,"Select",MENU_ACTION(&Select)},
	{MENU_ENTRY_CALLBACK,"Start",MENU_ACTION(&Start)},
//	{MENU_ENTRY_CALLBACK,"Animate",MENU_ACTION(&TriggerEffect)},
	{MENU_ENTRY_CALLBACK,"Load ROM \x10",MENU_ACTION(&FileSelector_Show)},
	{MENU_ENTRY_CALLBACK,"Exit",MENU_ACTION(&Menu_Hide)},
	{MENU_ENTRY_NULL,0,0}
};


// An error message
static struct menu_entry loadfailed[]=
{
	{MENU_ENTRY_SUBMENU,"ROM loading failed",MENU_ACTION(loadfailed)},
	{MENU_ENTRY_SUBMENU,"OK",MENU_ACTION(&topmenu)},
	{MENU_ENTRY_NULL,0,0}
};


#define HOST_READ_BASE 0xFFFFFA00
char debug[65];
char *debugp= debug;
char *hex = "0123456789ABCDEF";
static int debug_counter=0;
void DebugChar(char c)
{
  	if(debugp >= debug+sizeof(debug))
  		return;
	*debugp++ = c;	
	*debugp = 0;
}

void HexDebugByte(unsigned int k) {
	DebugChar(hex[(k >> 4) & 0xF]);
	DebugChar(hex[k & 0xF]);
}


static void mystrcpy(char *dst,const unsigned char *src)
{
	while(*dst++ = *src++)
		;
	*dst = 0;
}

static int LoadROM(const char *filename)
{
	int result=0;
	int opened;
  int i;
	int verify = MENU_TOGGLE_VALUES & (1 << 5);	// If non-zero, we make a verify
	int err_seen = 0;
	int addr = 0;

	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_RESET;
	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_DIVERT_SDCARD; // Release reset but take control of the SD card

	if((opened=FileOpen(&file,filename)))
	{
		int filesize=file.size;
		unsigned int c=0;
		int bits;

		bits=0;
		c=filesize-1;
		while(c)
		{
			++bits;
			c>>=1;
		}
		bits-=9;

		result=1;

		while(filesize>0)
		{
			OSD_ProgressBar(c,bits);
			if(FileRead(&file,sector_buffer)) {
				int i;
				unsigned char *p = sector_buffer;
				if(!verify) {
					for(i=0;i<512;i++)
					{
						unsigned char u = *p++;
						HW_HOST(REG_HOST_BOOTADDR)=addr++;
						HW_HOST(REG_HOST_BOOTDATA)=(unsigned)u;
					}
				} else {
					// perform verify
					for(i=0;i<512;i++)
					{
						unsigned char u = *p++;
						HW_HOST(REG_HOST_BOOTADDR)=addr++;
						unsigned int k = *((volatile unsigned int *)HOST_READ_BASE);
						if (u != k && !err_seen) {
							err_seen = 1;
							debugp = debug+4;
							mystrcpy(debug, "ERR ");
							HexDebugByte(addr-1);
							HexDebugByte((addr-1) >> 8);
							DebugChar(' ');
							HexDebugByte(u);
							DebugChar(' ');
							HexDebugByte(k);
							mystrcpy(debug_title, debug);
						}
					}
				}
			} else {
				result=0;
				filesize=512;
			}
			FileNextSector(&file);
			filesize-=512;
			++c;
		}
	}
	HW_HOST(REG_HOST_ROMSIZE) = file.size;

	if (!err_seen) {
		// debug[0] = verify ? 'V' : 'L';
		mystrcpy(debug, "OK ");
		debugp = debug+3;
		HexDebugByte(addr >> 8);
		HexDebugByte(addr);
		DebugChar(' ');
		HexDebugByte(file.size >> 8);
		HexDebugByte(file.size);
		mystrcpy(debug_title, debug);
	}
  
	Reset(0);
	
	if(result)
		Menu_Set(topmenu);
	else
		Menu_Set(loadfailed);
	return(result);
}

#define HOST_READ_NUMPAD 0xFFFFFFB4

void Debug(int row) {
  int i;
  unsigned u;
  debugp = debug;
  *debugp = 0;
  HexDebugByte(debug_counter);
  DebugChar(':');
	
	// Test code to read numpad
	u = *(volatile unsigned *)HOST_READ_NUMPAD;
	HexDebugByte((u >> 24) & 0xFF);
	HexDebugByte((u >> 16) & 0xFF);
	HexDebugByte((u >> 8) & 0xFF);
	HexDebugByte(u & 0xFF);
/*
  for(i=0; i<8; i++) {
		HW_HOST(REG_HOST_BOOTADDR) = debug_counter+i;
    unsigned int k = *((volatile unsigned int *)HOST_READ_BASE);
    HexDebugByte(k);
		DebugChar( ' ');
  }
*/  
  mystrcpy(debug_title, debug);
	Menu_Set(topmenu);
  debug_counter+=4;
}

void DebugCounterReset(int row)
{
  debug_counter=0;
}


int main(int argc,char **argv)
{
	int i;
	int dipsw=0;

	// Put the host core in reset while we initialise...
//	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_RESET;

	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_DIVERT_SDCARD;

	PS2Init();
	EnableInterrupts();
  
	OSD_Clear();
	for(i=0;i<4;++i)
	{
		PS2Wait();	// Wait for an interrupt - most likely VBlank, but could be PS/2 keyboard
		OSD_Show(1);	// Call this over a few frames to let the OSD figure out where to place the window.
	}
//	MENU_SLIDER_VALUE(&rgbmenu[0])=8;
//	MENU_SLIDER_VALUE(&rgbmenu[1])=8;
//	MENU_SLIDER_VALUE(&rgbmenu[2])=8;

 	HW_HOST(REG_HOST_CONTROL)=HOST_CONTROL_DIVERT_SDCARD; // Release reset but take control of the SD card
	OSD_Puts("Initializing SD card\n");
	if(!FindDrive())
		return(0);
  // OSD_Puts("Loading initial ROM...\n");
  // LoadROM("SPCINVADBIN");


//	OSD_Puts("Loading initial ROM...\n");

//	LoadROM("PIC1    RAW");

	FileSelector_SetLoadFunction(LoadROM);
  mystrcpy(debug_title, "CollectorVision");
	
	Menu_Set(topmenu);
	Menu_Show();

	while(1)
	{
		struct menu_entry *m;
		int visible;
		HandlePS2RawCodes();
		visible=Menu_Run();

		dipsw=MENU_CYCLE_VALUE(&topmenu[1]);	// Take the value of the TestPattern cycle menu entry.
		if(MENU_TOGGLE_VALUES&1)
			dipsw|=4;	// Add in the scanlines bit.
		if(MENU_TOGGLE_VALUES&2)
			dipsw|=2;	// Add in the PAL bit
		if(MENU_TOGGLE_VALUES&4)
			dipsw|=1;	// Add in the Color bit
		if(MENU_TOGGLE_VALUES&8)
			dipsw|=8;	// Add in the Diff A bit
		if(MENU_TOGGLE_VALUES&16)
			dipsw|=16;	// Add in the Diff B bit
		HW_HOST(REG_HOST_SW)=dipsw;	// Send the new values to the hardware.
//		HW_HOST(REG_HOST_SCALERED)=MENU_SLIDER_VALUE(&rgbmenu[0]);
//		HW_HOST(REG_HOST_SCALEGREEN)=MENU_SLIDER_VALUE(&rgbmenu[1]);
//		HW_HOST(REG_HOST_SCALEBLUE)=MENU_SLIDER_VALUE(&rgbmenu[2]);

		// If the menu's visible, prevent keystrokes reaching the host core.
		HW_HOST(REG_HOST_CONTROL)=(visible ?
									HOST_CONTROL_DIVERT_KEYBOARD|HOST_CONTROL_DIVERT_SDCARD :
									HOST_CONTROL_DIVERT_SDCARD); // Maintain control of the SD card so the file selector can work.
																 // If the host needs SD card access then we would release the SD
																 // card here, and not attempt to load any further files.
	}
	return(0);
}
