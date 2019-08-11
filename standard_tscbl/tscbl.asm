;===============================================================================
;TURGEN SYSTEM - TSCBL
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

;2019-07-18 Initial version
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
BOOTHEAD  .BYTE 0                 ;Boot flag 0
          .BYTE 3                 ;3 blocks + 1 EOF blocks
          .WORD [LDR_START]       ;Load address
          .WORD DO_RTS            ;Initialization address - just RTS
;-------------------------------------------------------------------------------
; Boot continuation code
;-------------------------------------------------------------------------------          
          lda  #60                ;Motor off
          sta  PACTL
          ldx  #255               ;Clear pushdown store
          txs
;-------------------------------------------------------------------------------
; Move last portion of the loader code from cassette buffer
;-------------------------------------------------------------------------------
                                  ;Move 128 bytes of the EOF block from cassette
                                  ;buffer to the intended place.
                                  ;X = 255 from above, loop down to 128.
RELO_P2_L lda  [1024-128],X
          sta  [LDR_START+384-128],X
          dex  
          bmi  RELO_P2_L
.ENDIF         
;-------------------------------------------------------------------------------
; Loader mainline code
;-------------------------------------------------------------------------------
BL000     jsr  STARTUP            ;Establish this loader as "DOS"

;===============================================================================
; Loader Screen
;===============================================================================
BLTOP                             ;Set screen and display title:

SCREEN    lda CONFIG_BG           ;Set background
          sta COLOR2
          lda CONFIG_FG           ;Set foreground
          sta COLOR1
          lda CONFIG_SNDR         ;Set SOUNDR
          sta SOUNDR
          lda CONFIG_CRSR         ;Set cursor visibility
          sta CRSINH
          
          lda #9                  ;Requesting PRINT
          sta CIO0_OP
          lda #<CONFIG_TITLE
          sta CIO0_BUFLO
          lda #>CONFIG_TITLE
          sta CIO0_BUFHI
          lda #[1+34+1]
          sta CIO0_LENLO
          ldx #0                  ;Channel 0
          stx CIO0_LENHI
          jsr CIOV                ;Call CIO
          
                                  ;Reset flag indicating 255 255 header found
          stx BL_HDR_FOUND                          
          
          lda CONSOL              ;Check for SELECT key
          cmp #5
          bne BLFCLOS             ;If not pressed, continue
          jmp COLDSV              ;Otherwise perform cold start
          
BLFCLOS   jsr FCLOSE              ;Close channel #1 
          jsr FOPEN               ;Open C: file
          bmi ERRHNDL             ;If error occured, handle it

;===============================================================================
; Read a segment 
;===============================================================================
GETSEG    lda #255                ;Set fake INIT vector to $FFFF (fake value)
          sta INITAD
          sta INITAD+1

;-------------------------------------------------------------------------------
; Get segment header
;-------------------------------------------------------------------------------
GS_STRTA  lda #<BLSEGHEAD         ;Read the first two bytes of segment header
          sta CIO1_BUFLO
          lda #>BLSEGHEAD
          sta CIO1_BUFHI
          lda #2
          sta CIO1_LENLO
          lda #0
          sta CIO1_LENHI
          jsr GETBLK
           
          lda BLSEGHEAD           ;Check for 255 255
          and BLSEGHEAD+1
          cmp #255
          bne GS_ENDA             ;If 255 255 not found, continue
          
          sta BL_HDR_FOUND        ;Indicate header was found
          beq GS_STRTA            ;And then start over
          
          
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
; Header (255 255) check 
;-------------------------------------------------------------------------------
          lda BL_HDR_FOUND       ;Check if 255 255 header was found
          bne GS_CALCLN          ;It was, we can continue
                                 ;If not, this is not a binary load file
;===============================================================================
;Error handling
;===============================================================================
ERRNOBIN lda #$0E                ;Not a binary file - white background
         bne ERRSIG

ERRHNDL  lda #$24                ;I/O error - red background

ERRSIG   sta COLOR4              ;Signalize error by changing background
         sta COLOR2
         sta COLBK
         sta COLPF2
         lda #60                 ;Switch off the motor
         sta PACTL  
         
         lda #255                ;Wait for any key
         sta CH
WFORKEYL cmp CH
         beq WFORKEYL
         
ERRREST  jmp WARMSV              ;Perform warm reset 
         

;-------------------------------------------------------------------------------
; Calculate length of the segment           
;-------------------------------------------------------------------------------          
GS_CALCLN sec                    ;Subtract start address from end address
          lda BLSEGHEAD+2        
          sbc BLSEGHEAD+0
          sta CIO1_LENLO
          lda BLSEGHEAD+3
          sbc BLSEGHEAD+1
          sta CIO1_LENHI
          
                                 ;Increase the difference by 1 to get length
          inc CIO1_LENLO
          bne GS_GETD
          inc CIO1_LENHI
;-------------------------------------------------------------------------------          
; Read segment data to its location in the memory          
;-------------------------------------------------------------------------------          
GS_GETD   lda BLSEGHEAD
          sta CIO1_BUFLO
          lda BLSEGHEAD+1
          sta CIO1_BUFHI
          jsr GETBLK
;-------------------------------------------------------------------------------
; Perform jump through INITAD if needed
;-------------------------------------------------------------------------------          
          lda INITAD             ;Check if there was real INIT segment
          and INITAD+1
          cmp #255
          beq POSTINI            ;Skip INIT if $FFFF
          
REALINI   lda #60                ;Switch off the motor
          sta PACTL 
          jsr DOINIT             ;Execute INIT segment code
          lda #52                ;Switch on the motor
          sta PACTL

POSTINI   jmp GETSEG             ;Get another segment

;===============================================================================
;Handle errors in GETBLK.
;===============================================================================
GBERR     cpy #136                  ;Is this EOF ?
          bne ERRHNDL               ;No - handle error
          ldx #255                  ;Yes, this is EOF
          txs                       ;Clear stack
          jsr FCLOSE                ;Close file
          
          jmp (RUNAD)               ;Run the program
        
 
;===============================================================================
;Subroutine that gets a blocks using CIO. Buffer address and length of
;the block must be set by the caller.
;===============================================================================
GETBLK    ldx #16                   ;Channel 1
          lda #7                    ;Requesting CIO READ operation
          sta CIO1_OP             
          jsr CIOV                  ;Call CIO
                                    ;Check for error
          bmi GBERR                 ;Error occured - handle it
          rts
          
;===============================================================================
;Emulation of JSR(738)
;===============================================================================
DOINIT    jmp (INITAD)
;===============================================================================
;Main data area
;===============================================================================
BLSEGHEAD    .BYTE 0,0,0,0        ;Segment header and position pointer
BL_HDR_FOUND .BYTE 0              ;Binary file header flag (255 255)
CDEV         .BYTE "C:",155       ;File name
;===============================================================================
;Subroutine that closes file
;===============================================================================
FCLOSE    ldx #16
          lda #12         ;Requesting CIO CLOSE operation with code 12
          sta CIO1_OP    
          jmp CIOV        ;Call CIO and return

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
          
          jmp CIOV               ;Call CIO and return

;===============================================================================
; Loader startup
;===============================================================================
STARTUP   lda #0                  ;Reset cold start flag
          sta COLDST
          lda #1                  ;Indicate disk boot succeded
          sta BOOT
          
          lda #<DINI              ;Setup DOSINI
          sta DOSINI
          lda #>DINI
          sta DOSINI+1
                                  ;Falls trough DINI to also setup DOSVEC
          
DINI      lda #<BLTOP             ;DOSINI sets DOSVEC
          sta DOSVEC
          lda #>BLTOP
          sta DOSVEC+1
DO_RTS    rts

;-------------------------------------------------------------------------------
; Program title and configuration bytes
; Defaults: 148,202,1,0
;-------------------------------------------------------------------------------
CONFIG_EYE      .BYTE "@CBL"          ;Eye-catcher
CONFIG_TITLE    .BYTE 125             ;Clear screen
CONFIG_NAME     .BYTE "TSCBL                             "
CONFIG_NAME_EOL .BYTE 155             ;End of line
CONFIG_BG       .BYTE 148             ;Background color             
CONFIG_FG       .BYTE 202             ;Foreground color                  
CONFIG_SNDR     .BYTE 1               ;SOUNDR value                  
CONFIG_CRSR     .BYTE 0               ;Cursor on-off
;===============================================================================
; RUN segment
;===============================================================================
.IF LDRTYPE=1
          *=RUNAD
          .BYTE <BL000,>BL000
.ENDIF
