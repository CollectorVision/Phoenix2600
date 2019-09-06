#include "minfat.h"
#include "menu.h"

static int romindex=0;
static int romcount;

static int listroms();
static void selectrom(int row);
static void scrollroms(int row);
int (*loadfunction)(const char *filename); // Callback function

static char romfilenames[13][30];

static char back_string[40] = "Back";

static struct menu_entry rommenu[]=
{
	{MENU_ENTRY_CALLBACK,romfilenames[0],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[1],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[2],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[3],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[4],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[5],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[6],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[7],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[8],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[9],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[10],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[11],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_CALLBACK,romfilenames[12],MENU_ACTION(&selectrom)},
	{MENU_ENTRY_SUBMENU,back_string,MENU_ACTION(0)},
	{MENU_ENTRY_NULL,0,MENU_ACTION(scrollroms)}
};


static void copyname(char *dst,const unsigned char *src,int l)
{
	int i;
	for(i=0;i<l;++i)
		*dst++=*src++;
	*dst++=0;
}


static DIRENTRY *nthfile(int n)
{
	int i,j=0;
	DIRENTRY *p;
	int at_end = 0;
	for(i=0;(j<=n) && !at_end;++i)
	{
		p=NextDirEntry(i, &at_end);	
		if(p)
			++j;
	}
	return(p);
}


static void selectrom(int row)
{
	DIRENTRY *p=nthfile(romindex+row);
	if(p)
	{
		copyname(longfilename,p->Name,11);	// Make use of the long filename buffer to store a temporary copy of the filename,
											// since loading it by name will overwrite the sector buffer which currently contains it!
		if(loadfunction)
			(*loadfunction)(longfilename);
	}
}


static void selectdir(int row)
{
	DIRENTRY *p=nthfile(romindex+row);
	if(p)
		ChangeDirectory(p);
	romindex=0;
	listroms();
	Menu_Draw();
}

extern char *hex;
void mystrcpy(char *dst,const unsigned char *src);
void to_hex_str( char *p, int k)
{
	p[0] = hex[(k >> 12) & 0xF];
	p[1] = hex[(k >> 8) & 0xF];
	p[2] = hex[(k >> 4) & 0xF];
	p[3] = hex[k & 0xF];
}

static void scrollroms(int row)
{
	switch(row)
	{
		case ROW_LINEUP:
			if(romindex)
				--romindex;
			break;
		case ROW_PAGEUP:
			romindex-=12;
			if(romindex<0)
				romindex=0;
			break;
		case ROW_LINEDOWN:
			++romindex;
			break;
		case ROW_PAGEDOWN:
			romindex+=12;
			break;
	}
	listroms();
	Menu_Draw();
}


static int listroms()	// EP 2019-08-31 now this returns (for debugging) the last dir entry processed
{
	int i,j;
	int at_end = 0;
	j=0;
	for(i=0;(j<romindex) && !at_end;++i)
	{
		DIRENTRY *p=NextDirEntry(i, &at_end);
		if(p)
			++j;
	}

	at_end = 0;

	for(j=0;(j<12) && !at_end;++i)
	{
		DIRENTRY *p=NextDirEntry(i, &at_end);
		if(p)
		{
			// FIXME declare a global long file name buffer.
			if(p->Attributes&ATTR_DIRECTORY)
			{
				rommenu[j].action=MENU_ACTION(&selectdir);
				romfilenames[j][0]=16; // Right arrow
				romfilenames[j][1]=' ';
				if(longfilename[0])
					copyname(romfilenames[j++]+2,longfilename,28);
				else
					copyname(romfilenames[j++]+2,p->Name,11);
			}
			else
			{
				rommenu[j].action=MENU_ACTION(&selectrom);
				if(longfilename[0])
					copyname(romfilenames[j++],longfilename,28);
				else
					copyname(romfilenames[j++],p->Name,11);
			}
		}
		else
			romfilenames[j][0]=0;
	}
	for(;j<12;++j)
		romfilenames[j][0]=0;

	mystrcpy(back_string, "Back xxxx yyyy zzzz kkkkkkkk");
	to_hex_str(back_string+5, romindex);
	to_hex_str(back_string+10, dir_entries);
	to_hex_str(back_string+15, i);
	extern unsigned int last_next_directory_cluster;
	to_hex_str(back_string+20, last_next_directory_cluster >> 16);
	to_hex_str(back_string+24, last_next_directory_cluster & 0xFFFF);

	return i;
}


void FileSelector_Show(int row)
{
	romindex=0;
	listroms();
	rommenu[13].action=MENU_ACTION(Menu_Get()); // Set parent menu entry
	Menu_Set(rommenu);
}


void FileSelector_SetLoadFunction(int (*func)(const char *filename))
{
	loadfunction=func;
}

