/* Backup T/D
   Extractor
*/
#include <conio.h>
#include <atari.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>


unsigned char cExtractCAS;
unsigned char cExtractXEX;
unsigned char cSourceDrive;
unsigned char cTargetDeviceLetter;
unsigned char cTargetDeviceNumber;
unsigned char cSequentialNaming;
unsigned char cConfirmEachExtraction;

unsigned char sectorBuffer[128];


#define SNO_MARKING   33
#define SNO_PRISTINE  34
#define SNO_DATA      35

#define RC_OK        0
#define RC_ERROR     8
#define DSKINV_RC_OK 1

#define RETRY_OK     0 
#define RETRY_RETRY  1
#define RETRY_SKIP   2
#define RETRY_ABORT  3


struct _turboHeader {
    unsigned char type;
    char turboName[10];
    unsigned int load;
    unsigned int length;
    unsigned int run;
};
typedef struct _turboHeader turboHeader_t;


void paintMenu();
void clearKeyInput();
void selectTargetDevice();
unsigned char readSector(void* buffer,unsigned char unitNumber,unsigned int sectorNumber);
void listDisk();
void handleDiskError(unsigned char returnCode);
void handleFileError(unsigned char errorCode);
void extractFiles();
unsigned char verifyDisk(unsigned char unitNumber);
unsigned char normalizeTurboName(char *turboName,unsigned char maxLength);
unsigned char retryPrompt(char* message);
unsigned char writeFile(char* name, unsigned int fileSize, unsigned int firstSector,unsigned char unitNumber,turboHeader_t *tHeader);
void flipYNOption(unsigned char* optionByte);

int main() {

char menuKey = ' ';

/*Initialize default configuration*/
cExtractCAS='N';
cExtractXEX='Y';
cSourceDrive='1';
cTargetDeviceLetter='H';
cTargetDeviceNumber='1';
cSequentialNaming='N';
cConfirmEachExtraction='N';

/*Pain main menu and clear key input*/

/*Menu main loop*/
while(1) {

paintMenu();
clearKeyInput();

menuKey = cgetc();

/*Flip binary file extraction*/
if (menuKey=='b' || menuKey=='B') {
flipYNOption(&cExtractXEX);        
}
/*Flip tape image extraction*/
/*else if (menuKey=='c' || menuKey=='C') {
     flipYNOption(&cExtractCAS);
}*/

/*Flip sequential naming*/
else if (menuKey=='s' || menuKey=='S') {
flipYNOption(&cSequentialNaming);
}

else if (menuKey=='y' || menuKey=='Y') {
flipYNOption(&cConfirmEachExtraction);
}

else if (menuKey=='l' || menuKey=='L') {
listDisk();
}

/*Input disk drive selection*/
else if (menuKey>='1' && menuKey<='8') {
   cSourceDrive=menuKey;
   continue;
}

/*Target device selection*/
else if (menuKey=='t' || menuKey=='T') {
    selectTargetDevice();
}

/*File extraction*/
else if  (menuKey=='e' || menuKey=='E') {
    if (cExtractXEX=='Y' || cExtractCAS=='Y') {
       extractFiles();
    }
}

/*Exit*/
else if (menuKey=='q' || menuKey=='Q') {
    clrscr();
    break;
}

}


return 0;


}



void paintMenu() {

/*Clear screen*/
clrscr();
cursor(0);


/*Title*/

cprintf("  Backup T/D Extractor 0.03\r\n");
cprintf("  (c) 2024 BAKTRA Software\r\n\r\n");

/*Menu items*/

cprintf("  Main Menu\r\n\r\n");
cprintf("  1-8 Select BACKUP T/D disk drive [%c]\r\n",cSourceDrive);
cprintf("  L   List files on Backup T/D disk\r\n\r\n");
cprintf("  T   Select target device [%c%c]\r\n",cTargetDeviceLetter,cTargetDeviceNumber);
cprintf("  E   Extract files from disk\r\n\r\n");
cprintf("  B   Extract binary/flat files [%c]\r\n",cExtractXEX);
cprintf("  .   Extract tape images       [%c]\r\n",cExtractCAS);
cprintf("  S   Use sequential naming     [%c]\r\n",cSequentialNaming);
cprintf("  Y   Confirm each extraction   [%c]\r\n\r\n",cConfirmEachExtraction);
cprintf("  Q   Quit\r\n");


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

/*Extract files from the disk*/
void extractFiles() {
unsigned char unitNumber = cSourceDrive-'0';
unsigned char rc=0;

char turboNameString[11];
char finalNameString[64];
char numberPrefixString[5];
char extensionString[5];

unsigned int currentSector;
unsigned int baseSector;
unsigned int len;
unsigned char retryCode;
turboHeader_t headerCopy;
char* extPtr;
unsigned int numSectors;
unsigned int remainderBytes;
unsigned int seqNumber=0;
char oneChar;

/*Tell that we are extracting files*/
clrscr();
cputs("  Extracting files...\r\n");

/*Verify the disk*/
if (verifyDisk(unitNumber)!=RC_OK) {
return;
}

/*Navigate to the first data sector*/
currentSector = SNO_DATA;

/*Do this for all files*/
while(1) {

/*Read the 'H'eader sector*/
rc = readSector(&sectorBuffer,unitNumber,currentSector);
if (rc!=DSKINV_RC_OK) {
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

/*If end marker found, extraction is complete*/
if (sectorBuffer[0]=='E') break;

/*Copy the header, and copy the turbo name*/
memcpy(&headerCopy,sectorBuffer+3,sizeof(turboHeader_t));
memcpy(turboNameString,&(headerCopy.turboName),10);
turboNameString[10]=0;

/*Remember sector where the file began*/
baseSector=currentSector+1;

/*Normalize the turbo name*/
len = normalizeTurboName(turboNameString,cTargetDeviceLetter=='H'?10:8);

/*Place file extension*/
switch (headerCopy.type) {
   case 0x03:
   case 0x04:
     extPtr=".XEX";
     break;
   case 0xFF:
   case 0xFE:
     extPtr=".BAS";
     break;
   default:
     extPtr=".DAT";
     break;
}

/*Construct the final output name*/
if (cSequentialNaming=='Y') {
   if (cTargetDeviceLetter=='H') {
     snprintf(numberPrefixString,5,"%03d_",seqNumber);
     memcpy(extensionString,extPtr,5); 
    }
    else {
      numberPrefixString[0]=0;
      oneChar=extPtr[1];
      snprintf(extensionString,5,".%c%02X",oneChar,seqNumber);
    }
}
else {
   numberPrefixString[0]=0;
   memcpy(extensionString,extPtr,5);
}

snprintf(finalNameString,64,"%c%c:%s%s%s",cTargetDeviceLetter,cTargetDeviceNumber,numberPrefixString,turboNameString,extensionString);

/*Print the file name and all information from the header*/
cprintf("\r\n  %-10s %02X $%04X $%04X $%04X\r\n",turboNameString,headerCopy.type,headerCopy.load, headerCopy.run, headerCopy.length);
cprintf("  ->%s\r\n",finalNameString);

/*If each extraction is to be confirmed, now it is good time to ask*/
if (cConfirmEachExtraction=='Y') {
cputs("\r\n  Confirm extraction Y/N?\r\n");
while(1) {
  oneChar = cgetc();
  if (oneChar=='n' || oneChar=='N') {
    goto skip_extraction;
  }
  else if (oneChar=='y' || oneChar=='Y') {
    break;
  }
}
}

/*Try to write the file*/
extract_retry:
rc = writeFile(finalNameString,headerCopy.length,baseSector,unitNumber,&headerCopy);
if (rc!=RC_OK) {
  retryCode = retryPrompt("Extraction failed.");
}
else {
  retryCode = RETRY_OK;
}

/*Based on result, decide how to continue*/
/*Extraction aborted, just terminate*/
if (retryCode==RETRY_ABORT) {
  cputs("  Extraction aborted. Press any key.\r\n");
  cgetc();
  return;
}

/*Retry writing the file*/
if (retryCode==RETRY_RETRY) {
   goto extract_retry;
}

/*Now either the extraction was ok, or file was skipped*/
if (retryCode==RETRY_SKIP) {
   cputs("  Extraction skipped\r\n");
}

skip_extraction:

/*Navigate to the beginning of the next file and continue*/
numSectors = (headerCopy.length+3)/128;
remainderBytes = (headerCopy.length+3)%128;
currentSector=baseSector+numSectors;
if (remainderBytes!=0) currentSector++;

++seqNumber;

} /*For all disk entries*/

/*Report that extraction is complete*/
cputs("\r\n  Extraction complete. Press any key.\r\n");
cgetc();

}

/*Write file*/
unsigned char writeFile(char* name, unsigned int fileSize, unsigned int firstSector, unsigned char unitNumber,turboHeader_t *tHeader) {

unsigned int currentSector=firstSector;
unsigned int bytesToGo=fileSize;
unsigned char firstFlag=1;
unsigned int rc;
unsigned int  sectorByteCount;
void* bufPtr;
unsigned int lastAddr;
unsigned char segmentBuffer[6];

FILE* f=0;

/*Try to open the file*/
f = fopen(name,"wb");
if (f==NULL) {
    handleFileError(errno);
    return RC_ERROR;
}

/*Do we need to encapsulate in a binary file?*/
if (tHeader->type==0x03) {

    segmentBuffer[0]=0xFF;
    segmentBuffer[1]=0xFF;
    /*First address*/
    segmentBuffer[2]=(tHeader->load&0x00FF);
    segmentBuffer[3]=(tHeader->load>>8);
    /*Last address*/
    lastAddr=tHeader->load+fileSize-1;
    segmentBuffer[4]=(lastAddr&0x00FF);
    segmentBuffer[5]=(lastAddr>>8);
    
    /*Write the header*/
    rc=fwrite(segmentBuffer,1,6,f);
    if (rc!=6) {
        handleFileError(errno);
        goto wf_term_bad;
    }
}

/*Now keep reading the sectors untill all bytes are read*/
while (bytesToGo>0) {

    /*Read a sector*/
    rc=readSector(&sectorBuffer,unitNumber,currentSector);
    if (rc!=DSKINV_RC_OK) {
      handleDiskError(rc);
      goto wf_term_bad;
    }
   
    /*First sector holds 'D' and requires special treatment*/
    if (firstFlag==1) {
       firstFlag=0;
       if (sectorBuffer[0]!='D') {
       cputs ("  Sector mark D not found\r\n");
       goto wf_term_bad;
       }
       bufPtr=sectorBuffer+3;
       sectorByteCount=128-3;
    }
    /*Other sectors hold up to 128 bytes*/
    else {
       bufPtr=sectorBuffer;
       sectorByteCount=128;
    }
    
    /*Count how many bytes we really need to get*/
    if (bytesToGo<sectorByteCount) {
       sectorByteCount=bytesToGo;
    }

    rc=fwrite(bufPtr,1,sectorByteCount,f);
    if (rc!=sectorByteCount) {
       handleFileError(errno);
       goto wf_term_bad;
    }

    bytesToGo-=sectorByteCount;
    currentSector++;
}

/*Write RUN segment, if needed*/
if (tHeader->type==0x03) {

    segmentBuffer[0]=0xE0;
    segmentBuffer[1]=0x02;
    segmentBuffer[2]=0xE1;
    segmentBuffer[3]=0x02;

    /*Run address*/
    segmentBuffer[4]=((tHeader->run)&0x00FF);
    segmentBuffer[5]=((tHeader->run)>>8);
    
    /*Write the segment*/
    rc=fwrite(segmentBuffer,1,6,f);
    if (rc!=6) {
       handleFileError(errno);
       goto wf_term_bad;
    }
}

    fclose(f);
    return RC_OK;

wf_term_bad:
    if (f!=0) fclose(f);
    return RC_ERROR;
}

/*List contents of the disk*/
void listDisk() {
unsigned char unitNumber = cSourceDrive-'0';
unsigned char rc = 0;
unsigned int currentSector;
unsigned int numSectors;
unsigned int remainderBytes;
char turboNameBuffer[11];
turboHeader_t headerCopy;
unsigned int fileSize;

clrscr();
cputs("  Listing disk...\r\n\r\n");

/*Verify the disk. If not our disk, return*/
if (verifyDisk(unitNumber)!=RC_OK) {
return;
}


/*Position to the first sector with data*/
currentSector = SNO_DATA;

/*Now perform the listing*/
while(1) {

/*Read the sector*/
rc = readSector(&sectorBuffer,unitNumber,currentSector);
if (rc!=DSKINV_RC_OK) {
  handleDiskError(rc);
  return;
}

/*Check if the sector begins with 'H' or 'E'*/
if (sectorBuffer[0]!='H' && sectorBuffer[0]!='E') {
   cputs("  Sector mark H or E not found.\r\n");
   cputs("  Press any key.\r\n");
   cgetc();
   return;
}

/*If end marker found, listing is complete*/
if (sectorBuffer[0]=='E') break;

/*Get header and name*/
memcpy(&headerCopy,sectorBuffer+3,sizeof(turboHeader_t));
memcpy(turboNameBuffer,&(headerCopy.turboName),10);
turboNameBuffer[10]=0;

/*Go to the first data sector*/
++currentSector;
/*Read the data sector*/
rc = readSector(&sectorBuffer,unitNumber,currentSector);
if (rc!=DSKINV_RC_OK) {
  handleDiskError(rc);
  return;
}

/*Check if the marking is 'D'*/
if (sectorBuffer[0]!='D') {
   cputs("  Sector mark D not found.\r\n");
   cputs("  Press any key.\r\n");
   cgetc();
   return;
}

/*Get the file size*/
fileSize=sectorBuffer[1]+256*sectorBuffer[2];

/*Display information on the file*/
cprintf("  %-10s %02X $%04X $%04X $%04X\r\n",turboNameBuffer,headerCopy.type,headerCopy.load, headerCopy.run, headerCopy.length);

/*Calculate the next header sector*/
numSectors = (fileSize+3)/128;
remainderBytes = (fileSize+3)%128;
currentSector+=numSectors;
if (remainderBytes!=0) currentSector++;

}


cputs("\r\n  Listing complete. Press any key.\r\n");
clearKeyInput();
cgetc();

}

void handleDiskError(unsigned char returnCode) {
cprintf("\r\n  Disk I/O Error $%02X. Press any key.\r\n",returnCode);    
cgetc();
}

void handleFileError(unsigned char errorCode) {
cprintf("\r\n  File I/O Error $%04X.\r\n",errorCode);    
}

/*Verify if the disk is BACKUP T/D disk*/
unsigned char verifyDisk(unsigned char unitNumber) {
unsigned char rc;

/*Read sector with marking*/
rc = readSector(&sectorBuffer,unitNumber,SNO_MARKING);

if (rc!=1) {
  handleDiskError(rc);
  return RC_ERROR;
}

/*Check for the marking*/
if (memcmp("TURGEN BACKUP T/D 1.00",&sectorBuffer,22)!=0) {
  cputs("  Not a BACKUP T/D disk. Press any key.\r\n");
  cgetc();
  return RC_ERROR; 
}

return RC_OK;

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

unsigned char normalizeTurboName(char *buffer,unsigned char maxLength) {
unsigned char length=maxLength;
int i;
unsigned char c;


/*Find the real valid length - blanks on right do not count*/
for (i=(maxLength-1);i>=0;i--) {
  if ((buffer[i]&0x7F)!=' ') {
     length=i+1;
     break;
  }
}

/*Normalize the name*/
for (i=0;i<length;i++) {
 
 c=buffer[i];
 if (c&0x80) c=c&0x7F;
 if (c==' ') c='_';
 if ((c>='a' && c<='z') || (c>='!' && c<='_')) {
 }
 else {
    c='_';
 } 
 
 buffer[i]=c;
}

 buffer[length]=0;

 return length;

}

unsigned char retryPrompt(char* message) {
  unsigned char c=' ';

  cputs("\r\n  ");
  cputs(message);
  cputs("\r\n  R Retry, A Abort, S Skip\r\n");

  while(1) {
    c = cgetc();
    if (c=='R' || c=='r') {
      return RETRY_RETRY;
    }
    if (c=='A' || c=='a' || c==CH_ESC) {
      return RETRY_ABORT;
    }
    if (c=='S' || c=='s') {
      return RETRY_SKIP;
    }
  }
}

void flipYNOption(unsigned char* optionByte) {
if (*optionByte=='N') {
   *optionByte='Y';
}
else {
   *optionByte='N';
}

}

void clearKeyInput() {

}
