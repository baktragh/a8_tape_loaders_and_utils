/* Backup T/D
   Extractor
*/

#include <conio.h>
#include <atari.h>
#include <stdlib.h>
#include <string.h>

unsigned char cExtractCAS;
unsigned char cExtractXEX;
unsigned char cSourceDrive;
unsigned char cTargetDeviceLetter;
unsigned char cTargetDeviceNumber;
unsigned char sectorBuffer[128];


#define SNO_MARKING   33
#define SNO_PRISTINE  34
#define SNO_DATA      35


void paintMenu();
void clearKeyInput();
void selectTargetDevice();
unsigned char readSector(void* buffer,unsigned char unitNumber,unsigned int sectorNumber);
void listDisk();
void handleDiskError(unsigned char returnCode);

int main() {

char menuKey = ' ';

/*Initialize default configuration*/
cExtractCAS='Y';
cExtractXEX='Y';
cSourceDrive='1';
cTargetDeviceLetter='D';
cTargetDeviceNumber='1';

/*Pain main menu and clear key input*/

/*Menu main loop*/
while(1) {

paintMenu();
clearKeyInput();

menuKey = cgetc();

/*Flip binary file extraction*/
if (menuKey=='b' || menuKey=='B') {
        if (cExtractXEX=='Y') {
           cExtractXEX='N'; 
        }
        else {
           cExtractXEX='Y';
        }
        continue;
    }
/*Flip tape image extraction*/
else if (menuKey=='c' || menuKey=='C') {
        if (cExtractCAS=='Y') {
          cExtractCAS='N';
         }
        else {
          cExtractCAS='Y';
         }
        continue;
    }

else if (menuKey=='l' || menuKey=='L') {
listDisk();
}

/*Input disk drive selection*/
else if (menuKey>='1' && menuKey<='8') {
   cSourceDrive=menuKey;
   continue;
}

else if (menuKey=='t' || menuKey=='T') {
    selectTargetDevice();
}

}


return 0;


}



void paintMenu() {

/*Clear screen*/
clrscr();
cursor(0);


/*Title*/
gotoxy(2,0);
cputs("Backup T/D Extractor 0.01");
gotoxy(2,1);
cputs("(c) 2024 BAKTRA Software");

/*Menu items*/
gotoxy(2,3);
cputs("Main Menu");
gotoxy(2,5);
cputs("L   List files on Backup T/D disk");
gotoxy(2,6);
cputs(".   Extract files from disk");
gotoxy(2,7);
cprintf("B   Toggle binary file extaction [%c]",cExtractXEX);
gotoxy(2,8);
cprintf("C   Toggle tape image extraction [%c]",cExtractCAS);
gotoxy(2,10);
cprintf("1-8 Select BACKUP T/D disk drive [%c]",cSourceDrive);
gotoxy(2,11);
cprintf("T   Select target device [%c%c]",cTargetDeviceLetter,cTargetDeviceNumber);

}

void selectTargetDevice() {

unsigned char letter;
unsigned char number;
unsigned char menuKey;

letter=cTargetDeviceLetter;
number=cTargetDeviceNumber;

clrscr();
cputs("  Select target device\r\n");
cputs("\r\n");
cputs("  Press A..Z to select device letter\r\n");
cputs("  Press 0..9 to select device number\r\n");
cputs("\r\n");
cputs("  Target device: \r\n");
cputs("\r\n");
cputs("  RETURN to confirm, ESC to cancel");
cputs("\r\n");

while(1) {

gotoxy(17,5);
cputc(letter);
gotoxy(18,5);
cputc(number);

menuKey=cgetc();

if (menuKey>='0' && menuKey<='9') {
  number=menuKey;
  continue;
}
else if (menuKey>='a' && menuKey<='z') {
  letter = menuKey-32;
  continue;
}
else if (menuKey>='A' && menuKey<='Z') {
  letter = menuKey;
  continue;
}

else if (menuKey==CH_ESC) {
  return;
}
else if (menuKey==CH_ENTER) {
  cTargetDeviceLetter=letter;
  cTargetDeviceNumber=number;
  return;
}

}


}

/*List contents of the disk*/
void listDisk() {
unsigned char unitNumber = cSourceDrive-'0';
unsigned char rc = 0;
char nameBuffer[11];
char sizeBuffer[5];
unsigned int fileSize;
unsigned int currentSector;
unsigned int numSectors;
unsigned int remainderBytes;

/*Put zero terminators to strings*/
nameBuffer[10]=0;
sizeBuffer[4]=0;

clrscr();
cputs("  Listing disk...\r\n\r\n");

/*Read sector with marking*/
rc = readSector(&sectorBuffer,unitNumber,SNO_MARKING);

if (rc!=1) {
  handleDiskError(rc);
  return;
}

/*Check for the marking*/
if (memcmp("TURGEN BACKUP T/D 1.00",&sectorBuffer,22)!=0) {
  cputs("  Not a BACKUP T/D disk. Press any key.");
  cgetc();
  return; 
}

/*Position to the first sector with data*/
currentSector = SNO_DATA;

/*Now perform the listing*/
while(1) {

/*Read the sector*/
rc = readSector(&sectorBuffer,unitNumber,currentSector);
if (rc!=1) {
  handleDiskError(rc);
  return;
}

/*Check if the sector begins with 'H' or 'E'*/
if (sectorBuffer[0]!='H' && sectorBuffer[0]!='E') {
   cputs("  Sector mark H or E not found.\r\n");
   cputs("  Press any key.");
   cgetc();
   return;
}

/*If end marker found, listing is complete*/
if (sectorBuffer[0]=='E') break;

/*Get file name*/
memcpy(nameBuffer,sectorBuffer+4,10);


/*Go to the first data sector*/
++currentSector;
/*Read the data sector*/
rc = readSector(&sectorBuffer,unitNumber,currentSector);
if (rc!=1) {
  handleDiskError(rc);
  return;
}

/*Check if the marking is 'D'*/
if (sectorBuffer[0]!='D') {
   cputs("  Sector mark D not found.\r\n");
   cputs("  Press any key.");
   cgetc();
   return;
}

/*Get the file size*/
fileSize=sectorBuffer[1]+256*sectorBuffer[2];
itoa(fileSize,sizeBuffer,16);

/*Print file name and file size*/
cprintf("  %10s $%04X\r\n",nameBuffer,fileSize);

/*Calculate the next header sector*/
numSectors = (fileSize+3)/128;
remainderBytes = (fileSize+3)%128;
currentSector+=numSectors;
if (remainderBytes!=0) currentSector++;

}


cputs("  Listing complete. Press any key\r\n");
clearKeyInput();
cgetc();

}

void handleDiskError(unsigned char returnCode) {

cprintf("\r\n  Disk I/O Error $%02X. Press any key.\r\n",returnCode);    
cgetc();

}

unsigned char readSector(void* buffer,unsigned char unitNumber,unsigned int sectorNumber) {

OS.dcb.dunit = unitNumber;
OS.dcb.dcomnd = 'R';
OS.dcb.dstats = 0;
OS.dcb.dbuf = buffer;
OS.dcb.dtimlo = 10;
OS.dcb.daux = sectorNumber;

__asm__ (" pha ");
__asm__ (" jsr $E453 ");
__asm__ (" pla ");

return OS.dcb.dstats;
}


void clearKeyInput() {

}
