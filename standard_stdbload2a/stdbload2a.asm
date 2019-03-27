;===============================================================================
;TURGEN SYSTEM - STDBLOAD 2a
;Binary loader for standard (FSK) tape records

;The author has placed this work in the Public Domain, thereby relinquishing all
;copyrights. Everyone is free to use, modify, republish, sell or give away this
;work without prior consent from anybody.

;This loader uses the "trailing EOF record trick" - Last 128 bytes of the
;loader are in the EOF block. These 128 bytes are moved from the cassette
;buffer to the intended memory location. The trick allows this loader to be
;only 4 records long

;The loader is ROM OS agnostic

;Using the LDRTYPE symbol, this loader can be assembled 
;to either boot (LDRTYPE=0) or binary (LDRTYPE=1) file. 
;===============================================================================

          .INCLUDE "equates.asm"
.IF LDRTYPE=0          
          LDR_START=[2048-24]
.ELSE
          LDR_START=2048
.ENDIF
          
          
          CIO1_OP   =$0342+16
          CIO1_STAT =$0343+16
          CIO1_BUFLO=$0344+16
          CIO1_BUFHI=$0345+16
          CIO1_LENLO=$0348+16
          CIO1_LENHI=$0349+16
          CIO1_AUX1 =$034A+16
          CIO1_AUX2 =$034B+16
          
          CIO0_OP   =$0342
          CIO0_STAT =$0343
          CIO0_BUFLO=$0344
          CIO0_BUFHI=$0345
          CIO0_LENLO=$0348
          CIO0_LENHI=$0349
          CIO0_AUX1 =$034A
          CIO0_AUX2 =$034B

          *=[LDR_START]
;-------------------------------------------------------------------------------
; Boot header
;-------------------------------------------------------------------------------
.IF LDRTYPE=0
BOOTHEAD  .BYTE 0
          .BYTE 3
          .WORD [LDR_START]
          .WORD FAKEINIT
          
          lda  #60                ;Motor off
          sta  PACTL
          ldx  #255               ;Clear pushdown store
          txs
;-------------------------------------------------------------------------------
; Move last portion of the loader code from cassette buffer
;-------------------------------------------------------------------------------
RELO_P2   ldx  #128               ;Move 128 bytes of the EOF block
RELO_P2_L lda  [1024-1],X         ;from cassette buffer
          sta  [LDR_START+384-1],X  ;to the intended place
          dex  
          bne  RELO_P2_L
.ENDIF         
;-------------------------------------------------------------------------------
; Loader mainline code
;-------------------------------------------------------------------------------
BL000     jsr  STARTUP            ;Display program name

BLTOP     lda #1                  ;Rest flags (first segment + no binary header)
          sta BLF_FIRST           
          sta BLF_NOBIN
          
          jsr FCLOSE
          jsr FOPEN               ;Call subroutine that opens C: file
          lda CIO1_STAT           ;Check for error
          bpl GETSEG              ;No error, continue
          jsr ERRHNDL             ;If error occured, go to handle it
          jmp BLTOP               ;Then start again

;-------------------------------------------------------------------------------
; Read a segment 
;-------------------------------------------------------------------------------
GETSEG    lda #<FAKEINIT          ;Set fake INIT vector to RTS
          sta 738
          lda #>FAKEINIT
          sta 739

;-------------------------------------------------------------------------------
; Get segment header
;-------------------------------------------------------------------------------
GS_STRTA  lda #<BLSEGHEAD         ;Read first two bytes of segment header
          sta CIO1_BUFLO
          lda #>BLSEGHEAD
          sta CIO1_BUFHI
          lda #2
          sta CIO1_LENLO
          lda #0
          sta CIO1_LENHI
          jsr GETBLK
           
          lda #255                ;Check for 255 255
          cmp BLSEGHEAD
          bne GS_ENDA
          cmp BLSEGHEAD+1
          bne GS_ENDA             ;If 255 255 not found, continue
          
          lda #0
          sta BLF_NOBIN
          jmp GS_STRTA            ;And then start over
          
          
GS_ENDA   lda #<[BLSEGHEAD+2]     ;Get rest of the segment header 
          sta CIO1_BUFLO
          lda #>[BLSEGHEAD+2]
          sta CIO1_BUFHI
          lda #2
          sta CIO1_LENLO
          lda #0
          sta CIO1_LENHI
          jsr GETBLK
          
;-------------------------------------------------------------------------------
; Processing specific for the first segment
; 255 255 header check
; RUNAD is set to point to the first segment
;-------------------------------------------------------------------------------
          lda BLF_NOBIN          ;Check if 255 255 header was found
          beq GS_FSRUN           ;It was, we can continue
          jmp ERRNOBIN           ;If not, we signalize an error and continue

GS_FSRUN  lda BLF_FIRST          ;Is this the first segment
          beq GS_CALCLN          ;No, just continue
          lda #0                 ;Reset first segment indication
          sta BLF_FIRST          ;RUNAD points to this segment
          lda BLSEGHEAD
          sta 736
          lda BLSEGHEAD+1
          sta 737
;-------------------------------------------------------------------------------
; Calculate length of the segment           
;-------------------------------------------------------------------------------          
GS_CALCLN sec                    ;Subtract start address from end address
          lda BLSEGHEAD+2        
          sbc BLSEGHEAD+0
          sta CIO1_LENLO
          bcs GS_LENHI
          dec BLSEGHEAD+3
GS_LENHI  sec
          lda BLSEGHEAD+3
          sbc BLSEGHEAD+1
          sta CIO1_LENHI
          
          clc                    ;Increase the difference by 1 to get length
          lda CIO1_LENLO
          adc #1
          sta CIO1_LENLO
          bcc GS_GETD
          inc CIO1_LENHI
;-------------------------------------------------------------------------------          
;Read segment data          
;-------------------------------------------------------------------------------          
GS_GETD   lda BLSEGHEAD
          sta CIO1_BUFLO
          lda BLSEGHEAD+1
          sta CIO1_BUFHI
          jsr GETBLK
;-------------------------------------------------------------------------------
; INIT segment handling
;-------------------------------------------------------------------------------          
          lda 738                ;Check if there was real INIT segment
          cmp #<FAKEINIT
          bne REALINI
          lda 739
          cmp #>FAKEINIT
          beq POSTINI
          
REALINI   lda #60                ;Switch off the motor
          sta PACTL 
          jsr DOINIT             ;Execute INIT segment code
          lda #52                ;Switch on the motor
          sta PACTL

POSTINI   jmp GETSEG             ;Get another segment

;===============================================================================
;Subroutine that gets a blocks using CIO. Buffer address and length of
;the block must be set by the caller.
;===============================================================================
GETBLK    ldx #16                   ;Channel 1
          lda #7                    ;Requesting CIO READ operation
          sta CIO1_OP             
          jsr CIOV                  ;Call CIO
          lda CIO1_STAT             ;Check for error
          bmi GBERR                 ;Error occured - handle it
          rts
          
GBERR     cmp #136                  ;Is this EOF ?
          bne GBERR_S               ;No - handle error
          ldx #255                  ;Yes, this is EOF
          txs                       ;Clear stack
          jsr FCLOSE                ;Close file
          jmp (736)                 ;Run the program
        
GBERR_S   jmp ERRHNDL
 
;===============================================================================
;Emulation of JSR(738)
;===============================================================================
DOINIT    jmp (738)
FAKEINIT  rts
;===============================================================================
;Main data area
;===============================================================================
BLSEGHEAD   .BYTE 0,0,0,0        ;Segment header and position pointer
BLF_FIRST   .BYTE 0              ;First segment to be loaded
BLF_NOBIN   .BYTE 0              ;No binary file header found yet

CDEV        .BYTE "C:",155       ;File name
;===============================================================================
;Subroutine that closes file
;===============================================================================
FCLOSE    ldx #16
          lda #12         ;Requesting CIO CLOSE operation with code 12
          sta CIO1_OP    
          jsr CIOV        ;Call CIO
          rts             
;===============================================================================
;Subroutine that opens file
;===============================================================================
FOPEN     ldx #16                ;IOCB 1
          lda #3                 ;Requesting CIO OPEN operation with code 3
          sta CIO1_OP
          lda #4                 ;Auxiliary value 4 - open for reading
          sta CIO1_AUX1
          
          lda #12                ;And also simulate key press
          sta CH                 
                       
          lda #128               ;Auxiliary value 128 - short IRGs
          sta CIO1_AUX2
          lda #<CDEV             ;Buffer- DEVICE:FILENAME ("C:")
          sta CIO1_BUFLO
          lda #>CDEV
          sta CIO1_BUFHI
          
          jsr CIOV               ;Call CIO
          rts 
;===============================================================================
;Error handling
;===============================================================================
ERRHNDL  lda #$24                ;I/O error - red background
         jsr ERRSIG
         jmp COLDSV              ;Cold start
         
ERRNOBIN lda #$0E                ;Not a binary file - white background
         jsr ERRSIG
         jmp WARMSV              ;Warm start         

;===============================================================================
; Auxiliary routines for error handling
;===============================================================================  
ERRSIG   sta COLOR4              ;Signalize error by changing background
         sta COLOR2
         sta COLBK
         sta COLPF2
         lda #60                 ;Switch off the motor
         sta PACTL  
         jsr WFORKEY
         rts
         
WFORKEY  lda #255                ;Wait for any key
         sta 764
WFORKEYL lda 764
         cmp #255
         beq WFORKEYL
         rts
;===============================================================================
; Loader startup
;===============================================================================
STARTUP   lda #0                  ;Reset cold start flag
          sta 580
          lda #1                  ;Indicate disk boot succeded
          sta 9
          jsr DINI
          
          lda #<DINI
          sta DOSINI
          lda #>DINI
          sta DOSINI+1
          
          lda #148                ;Set background
          sta COLOR2
          lda #3                  ;Noisy or silent I/O
          sta SOUNDR
          
          ldx #0                  ;Channel 0
          lda #9                  ;Requesting PRINT
          sta CIO0_OP
          lda #<PROGTITLE
          sta CIO0_BUFLO
          lda #>PROGTITLE
          sta CIO0_BUFHI
          lda #<[PROGNEND-PROGTITLE]
          sta CIO0_LENLO
          lda #>[PROGNEND-PROGTITLE]
          sta CIO0_LENHI
          jsr CIOV                ;Call CIO
          
DINI      lda #<BLTOP
          sta DOSVEC
          lda #>BLTOP
          sta DOSVEC+1
          rts

PROGTITLE .BYTE 125               ;Clear screen
PROGNAME  .BYTE 32                ;34 characters for program name
          .REPT [33]              
          .BYTE 32
          .ENDR
          .BYTE 32                ;Extra blank
          .BYTE "!"          ;Inverse video exclamation mark
PROGNEND  
          
;===============================================================================
; RUN segment
;===============================================================================
.IF LDRTYPE=1
          *=736
          .BYTE <BL000,>BL000
.ENDIF          
