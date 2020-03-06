#ifndef HOST_H
#define HOST_H

#define HOSTBASE 0xFFFFFFE0
#define HW_HOST(x) *(volatile unsigned int *)(HOSTBASE+x)

/* SPI registers */

/* Host Boot Data register */
#define REG_HOST_MUTECTRL  0x00
	// REG_HOST_MUTECTRL: bit 0: 1=mute
	//					  bit 1: 1=debug arm
	//                    bit 2: 1=use external cartridge ROM
#define REG_HOST_BOOTDATA 0x08
#define REG_HOST_BOOTADDR 0x04

/* Host control register - this is a write-only register */
#define REG_HOST_CONTROL 0x0C
// mask bits to the REG_HOST_CTRL register
#define HOST_CONTROL_RESET 1
#define HOST_CONTROL_DIVERT_KEYBOARD 2
#define HOST_CONTROL_DIVERT_SDCARD 4
#define HOST_CONTROL_SELECT 8
#define HOST_CONTROL_START 16

/* DIP switches / "Front Panel" controls - bits 15 downto 0 */
#define REG_HOST_SCALERED 0x10
#define REG_HOST_SCALEGREEN 0x14
#define REG_HOST_ROMSIZE 0x18
#define REG_HOST_SW 0x1C

// EP: Read only-registers.
#define HOST_READ_NUMPAD    0xFFFFFFB4
    // Bits here:
    //      11-0: Coleco controller numpad keys (12 of these)
    //      16-12: the 5 gamepad buttons: fire left right up down
    //      25-17: number of scanlines in last frame
    //      30-26: X
    //      31: status of cartridge mode in hardware. 1 = ext cart ROM enabled.
#define HOST_READ_NUMPAD_BIT_CART_MODE 31

// Register below removed from firmware.
// #define HOST_READ_SCANLINES 0xFFFFFFB8

#endif

