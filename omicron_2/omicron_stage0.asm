;===============================================================================
;Omicron Turbo - Stage 0 loader
;===============================================================================             
;
; Omicron turbo provides unified file format and loaders compatible with
; the following turbo upgrades:
;
; Group A - Switch to turbo mode by active COMMAND signal.
;         - Signal input from DATA IN
; 
; Group B - Switch to turbo mode by forced 0 at DATA OUT
;         - Signal input from DATA IN
;
; Group C - Switch to turbo mode by signal at joystick port
;         - Signal input from joystick port
;
; Signal input is detected automatically by alternating the input source
; until a pilot tone is detected.
;
;
;The stage 0 loader performs the following:
; 1. Determines the source of the signal
; 2. Loads 860 bytes of stage 1 loader data to address 8192 (2270 bd)
; 3. Executes the stage loader
;
;Three forms of this stage 0 loader can be assembled.
; LDRTYPE=0 - Cassette boot file with data in the EOF block
; LDRTYPE=1 - DOS 2 Binary Load File (.xex)
; LDRTYPE=2 - Cartridge
;
; File name
; Cassette boot file displays file name before loading the stage 1 loader
; Other forms display file name stored in the last 20 bytes of the stage 1
; loader.
;
;
;===============================================================================
            .INCLUDE "system_equates.asm" 


.IF LDRTYPE=1
.BANK
.ENDIF

            *=16384
;-----------------------------------------------
; Boot header
;-----------------------------------------------
            
.IF LDRTYPE=0            

BOOTHEAD   .BYTE 0                       ;Unused
BH_NBLK    .BYTE 3                       ;Number of blocks.
BH_LADDR   .WORD [16384]                 ;Load address
BH_IADDR   .BYTE 0,0                     ;Initialization address            
            
RELO_P2     ldx  #128                    ;Relocate 2nd portion
RELO_P2_L   lda  [1024-1],X              ;From cassette buffer
            sta  [16384+128+128+128-1],X ;past the three blocks
            dex  
            bne  RELO_P2_L
.ENDIF            


;----------------------------------------------------------
; Display message
; Cassette boot file - Program name set by external program
; Others - Omicron Turbo banner
;----------------------------------------------------------
            ldy #20                     ;Display title
NAMELOOP    lda [PARM_PROGNM-1],y
            sta (SAVMSC),y
            dey
            bne NAMELOOP
            
            sty RTCLOK+2                ;Wait 2 seconds
WAITLOOP    lda RTCLOK+2
            cmp #100
            bcc WAITLOOP            

;-----------------------------------------------
; Skip parameters
;-----------------------------------------------
            jmp  L05D2                  ;Skip over parameters
;-----------------------------------------------
; Parameters
;-----------------------------------------------
PARM_BLKFIRST .WORD 8192                 ;Set by external program
PARM_BLKLAST  .WORD [8192+860]           ;Set by external program
PARM_RUNADDR  .WORD 8192                 ;Set by external program

.IF LDRTYPE=0                            ;Cassette boot file
PARM_PROGNM   .REPT 20                   ;Set by external program
              .BYTE 0                    ;Internal code
              .ENDR
.ELSE                                    ;Binary file or cartridge
PARM_PROGNM   .SBYTE "Omicron Turbo       " ; Banner
;                    -12345678901234567890-               
.ENDIF
;-----------------------------------------------
; Reading single block
;-----------------------------------------------            
L05D2       lda PARM_BLKFIRST  ;Setting up buffer for single block loading
            sta BUFRLO
            lda PARM_BLKFIRST+1
            sta BUFRHI

            lda PARM_BLKLAST
            sta BFENLO
            lda PARM_BLKLAST+1
            sta BFENHI

            lda #224           ;First byte in the block should be 281 - Omicron
            jsr L0631          ;Call block decoding subroutine

            bcs L0622          ;If there was no error, jump to RUN
;-----------------------------------------------
; Handle error - Pink screen of death
;-----------------------------------------------            
            lda #1              ;Cold start after RESET
            sta COLDST     
            lda #88             ;Red/Purple screen
            sta COLBK
            sta COLOR4
ERRLOOP     jmp ERRLOOP         ;Wait for RESET   
;-----------------------------------------------
; Execute the loaded program            
;-----------------------------------------------
L0622
.IF LDRTYPE<>0
;-----------------------------------------------
; Display program name
;-----------------------------------------------
            ldy #20                     ;Display program name
NAMELOOP2   lda [8192+860-20-1],y
            sta (SAVMSC),y
            dey
            bne NAMELOOP2
            
            sty RTCLOK+2                ;Wait 2 seconds
WAITLOOP2   lda RTCLOK+2
            cmp #100
            bcc WAITLOOP2            
.ENDIF   
            jmp (PARM_RUNADDR)          ;RUN
;===============================================================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block should be present in the ;acumulator register before
;this subroutine is called        
;===============================================================================
L0631       sta LTEMP     ;Keep expected first byte

;------------------------------------------------------------------------------
; Switch to turbo mode
;------------------------------------------------------------------------------
            lda #52       ;Group A - COMMAND active
            sta PBCTL
            
            lda #$83      ;Group B - DATA-OUT forced to zero
            sta SKCTL     
            
            lda #$38      ;Group C - Input/Output from joystick port
            sta PACTL     ;Direction control mode
            lda #$60      ;Set direction contol bits
            sta PORTA
            lda #$34      ;PORTA addressing mode
            sta PACTL     ;Motor on
            
;------------------------------------------------------------------------------
; Disable interrupts and DMA
;------------------------------------------------------------------------------            
            lda #0
            sta POKMSK
            sta IRQEN
            clc
            ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            sty ICSTAZ    ;Pulse detection indicator (0,1)

;------------------------------------------------------------------------------
; Decode bits and bytes
;------------------------------------------------------------------------------            
            php                 
L0650       bne L06C2        ;If not zero, terminate decoding
L0652       jsr SWITCH_SIG   ;Switch signal
            jsr L06DB        ;Wait for edge
            bcc L0650        ;No edge, try again
            lda #0           ;Zero number of pilot tone pulses 
            sta ICAX5Z
            sta LTEMP+1      ;Indicate "looking for pilot tone"
L065D       ldy #180         ;Counter set for pilot tone pu;se
L065F       jsr L06D6        ;Measure width
            bcc L0650        ;No pulse, try again
            cpy #216         ;Check width of pulse
            bcc L0652        ;Witdh incorrect, try again
            inc ICAX5Z       ;Pilot tone pulse counted
            bne L065D        ;If less than 255, continue with pilot tone
            
            dec LTEMP+1      ;Indicate "Pilot tone found"
L066E       ldy #209         ;Set counter
            jsr L06DB        ;Wait for edge
            bcc L0650        ;No edge, try again
            cpy #222         ;Check width
            bcs L066E        ;Still pilot tone, continue
            jsr L06DB        ;Wait for edge
            bcc L06C2        ;No edge, try again
            ldy #198         ;Set counter
            jmp L069D        ;Skip to data decoding
            
L0683       plp                
            bne L068E        ;Not zero, just store to buffer
            lda LTEMP        ;Check id byte
            eor ICAX6Z       ;Update checksum
            bne L06C3        ;If not equal, terminate decoding
            beq L069A        ;Otherwise skip storing to buffer
            
L068E       ldy #0           ;Store byte to buffer
            lda ICAX6Z
            sta (BUFRLO),Y
            inc BUFRLO       ;Update buffer pointers
            bne L069A
            inc BUFRHI
L069A       ldy #200         ;Prepare counter
            php
            
L069D       lda #1          ;Prepare 1
            sta ICAX6Z      ;Store
L06A1       jsr L06D6       ;Measure width
            bcc L06C2       ;No pulse, terminate
            cpy #227        ;Check width and prepare CF
            rol ICAX6Z      ;Rotate bit with carry
            ldy #198        ;Set counter
            bcc L06A1       ;No carry after rotation, get next bit
            lda CHKSUM      ;Update checksum
            eor ICAX6Z
            sta CHKSUM
            lda BUFRLO      ;Check if last byte
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc L0683       ;If not last byte, continue decoding
            
            lda #0          ;Decoding is over
            cmp CHKSUM      ;Check checksum
;------------------------------------------------------------------------------
; Terminate decoding
;------------------------------------------------------------------------------            
L06C2       pla
L06C3       lda #192     
            sta NMIEN
            sta POKMSK
            sta IRQEN
            
            lda #60      ;Group A - COMMAND Inactive
            sta PBCTL
            
            lda #$03     ;Group B - Do not force DATA-OUT to 0
            sta SKCTL    
            
            lda #$38     ;Group C - Reset joystick port         
            sta PACTL              
            lda #$00               
            sta PORTA              
            
            lda #$3C     ;Motor off
            sta PACTL              
            
            rts
;------------------------------------------------------------------------------
; Detect edges
;------------------------------------------------------------------------------                        
L06D6       jsr L06DB
            bcc L06FF
L06DB       ldx #4
L06DD       dex
            bne L06DD
            lda STATUS
            lsr A
            and LTEMP+1
            sta COLBK
L06E8       iny
            beq L06FE
            lda BRKKEY
            beq L06FC
L_DET       lda SKSTAT
L_AND       and #16
            cmp STATUS
            beq L06E8
            sta STATUS
            sec
            rts
L06FC       dec BRKKEY
L06FE       clc
L06FF       rts  
;-----------------------------------------------------------------------------
; Switch signal source
;-----------------------------------------------------------------------------
SWITCH_SIG  php
            pha
            lda ICSTAZ         ;Check indicator         
            beq SW_TOSIO       ;If zero, switch to SIO
            
SW_TOJOY    dec ICSTAZ         ;Switch indicator
            lda #$00           ;$D300
            sta L_DET+1
            lda #$D3
            sta L_DET+2
            lda #128           ;Bit 7
            sta L_AND+1
            bne SW_END
        
SW_TOSIO    inc ICSTAZ         ;Switch indicator
            lda #$0F
            sta L_DET+1
            lda #$D2
            sta L_DET+2
            lda #16
            sta L_AND+1
            
SW_END      pla                ;Restore everything
            plp
            rts                ;Return
            
.IF LDRTYPE=1            
;-----------------------------------------------------------------------------
; RUN Vector
;-----------------------------------------------------------------------------
.BANK
            *=736
            .WORD 16384
.ENDIF            
