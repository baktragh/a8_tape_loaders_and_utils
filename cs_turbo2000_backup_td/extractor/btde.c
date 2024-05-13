/* Backup T/D
   Extractor
*/

#include <conio.h>


unsigned char cExtractCAS;
unsigned char cExtractXEX;
unsigned char cSourceDrive;
unsigned char cTargetDeviceLetter;
unsigned char cTargetDeviceNumber;


void paintMenu();
void clearKeyInput();
void selectTargetDevice();

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
cputs("Backup T/D Extractor");
gotoxy(2,1);
cputs("(c) 2024 BAKTRA Software");

/*Menu items*/
gotoxy(2,3);
cputs("Main Menu");
gotoxy(2,5);
cputs("L   List files on Backup T/D disk");
gotoxy(2,6);
cputs("E   Extract files from disk");
gotoxy(2,7);
cputs("B   Toggle binary file extaction [ ]");
gotoxy(2,8);
cputs("C   Toggle tape image extraction [ ]");
gotoxy(2,10);
cputs("1-8 Select BACKUP T/D disk drive [ ]");
gotoxy(2,11);
cputs("T   Select target device [  ]");

/*Configuration*/
gotoxy(36,7);
cputc(cExtractXEX);
gotoxy(36,8);
cputc(cExtractCAS);
gotoxy(36,10);
cputc(cSourceDrive);

gotoxy(28,11);
cputc(cTargetDeviceLetter);
gotoxy(29,11);
cputc(cTargetDeviceNumber);

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

else if (menuKey==27) {
  return;
}
else if (menuKey==155) {
  cTargetDeviceLetter=letter;
  cTargetDeviceNumber=number;
  return;
}

}


}


void clearKeyInput() {
while(kbhit());
}
