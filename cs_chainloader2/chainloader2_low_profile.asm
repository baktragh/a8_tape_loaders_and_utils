;===============================================
;TURGEN SYSTEM - ChainLoader 2
;Binary loader for Turbo 2000 (Czechoslovakia)
;
;Low-profile version that resides in the zero
;page and stack
;===============================================

;For each segment, there is a pair of T2000 blocks.
;
;Segment header - holds pair of addresses. ID byte is 125.
;Segment data   - holds segment data. ID byte is 126.
;
;File is terminated with a termination segment header that
;has addresses of 255,255,255,255.
;
;Loader tolerates first segment to be a RUN segment. If there
;is no RUN segment, unpredictable results may occur.
;
;In case of I/O error, red screen is displayed and the user
;must press RESET for cold start.
;
;Assemble with XASM

            OPT H-
            ICL 'equates_low_profile.asm'

            ORG $80

RESTART
;----------------------------------------------
; Get segment header
;----------------------------------------------
GETHEADER   jsr SETHDBUF
            lda #125              ;ID 125
            jsr GETBLOCK
            bcs PROCHEADER

;----------------------------------------------
; Handle errors - wait for cold start
;----------------------------------------------
LOADERROR   lda #24
            sta 712
            sta 580
INFLOOP     jmp INFLOOP
            
;----------------------------------------------
; Process segment header
;----------------------------------------------
PROCHEADER  ldx #4                ;Check for EOF
PH_EOFLOOP  lda [HEADBUF-1],X
            cmp #255
            bne PH_NOTEOF
            dex
            bne PH_EOFLOOP
            
            jmp (RUNAD)           ;Run program
            
            
PH_NOTEOF   lda #<FAKEINIT        ;Prepare fake init
            sta INITAD
            lda #>FAKEINIT
            sta INITAD+1
;----------------------------------------------
; Load and process segment data
;-----------------------------------------
LOADSEGDATA jsr SETDATABUF
            lda #126              ;ID 126
            jsr GETBLOCK
            bcs PROCSEG
            jmp LOADERROR
            
PROCSEG     jsr PROCINIT
            jmp GETHEADER

PROCINIT    jmp (INITAD)
            
            
;----------------------------------------------
; Set buffer for segment header
;----------------------------------------------
SETHDBUF    lda #<HEADBUF
            sta BUFRLO
            lda #>HEADBUF
            sta BUFRHI
            lda #<[HEADBUF+HEADLEN]
            sta BFENLO
            lda #>[HEADBUF+HEADLEN]
            sta BFENHI
            rts
            
;----------------------------------------------
; Set buffer for segment data
;----------------------------------------------
SETDATABUF  lda HEADBUF
            sta BUFRLO
            lda HEADBUF+1
            sta BUFRHI
            
            inc HEADBUF+2  ;Increase LO by one
            bne SDB_10     ;No 255->0, skip
            inc HEADBUF+3  ;Increase HI by one
SDB_10      lda HEADBUF+2
            sta BFENLO
            lda HEADBUF+3
            sta BFENHI
            
FAKEINIT    rts
            

;==============================================
;Block decoding subroutine
;Block is placed from: BUFRLO+256*BUFRHI
;                to:   (BFENLO+256*BFENHI)-1
;First byte of the block should be in
;acumulator register before subroutine
;is called        
;==============================================
    
GETBLOCK    sta LTEMP
            lda #52
            sta PACTL
            sta PBCTL
            lda #128
            sta POKMSK
            sta IRQEN
            clc
            ldy #0
            sty STATUS
            sty CHKSUM
            sty NMIEN
            sty DMACLT
            php
L0650       bne L06C2
L0652       jsr L06DB
            bcc L0650
            lda #0
            sta ICAX5Z
            sta LTEMP+1
L065D       ldy #180
L065F       jsr L06D6
            bcc L0650
            cpy #216
            bcc L0652
            inc ICAX5Z
            bne L065D
            dec LTEMP+1
L066E       ldy #209
            jsr L06DB
            bcc L0650
            cpy #222
            bcs L066E
            jsr L06DB
            bcc L06C2
            ldy #198
            jmp L069D
L0683       plp
            bne L068E
            lda LTEMP
            eor ICAX6Z
            bne L06C3
            beq L069A
L068E       ldy #0
            lda ICAX6Z
            sta (BUFRLO),Y
            inc BUFRLO
            bne L069A
            inc BUFRHI
L069A       ldy #200
            php
L069D       lda #1
            sta ICAX6Z
L06A1       jsr L06D6
            bcc L06C2
            cpy #227
            rol ICAX6Z
            ldy #198
            bcc L06A1
            lda CHKSUM
            eor ICAX6Z
            sta CHKSUM
            lda BUFRLO
            cmp BFENLO
            lda BUFRHI
            sbc BFENHI
            bcc L0683
            lda #0
            cmp CHKSUM
L06C2       pla
L06C3       lda #192
            sta NMIEN
            sta POKMSK
            sta IRQEN
            lda #60
            sta PACTL
            sta PBCTL
            rts
L06D6       jsr L06DB
            bcc L06FF
L06DB       ldx #4
L06DD       dex
            bne L06DD
            lda STATUS
            lsr @ 
            and LTEMP+1
            sta COLBK
L06E8       iny
            beq L06FE
            lda BRKKEY
            beq L06FC
            lda SKSTAT
            and #16
            cmp STATUS
            beq L06E8
            sta STATUS
            sec
            rts
L06FC       dec BRKKEY
L06FE       clc
L06FF       rts

HEADBUF     DTA b(0,0,0,0)
HEADLEN     EQU *-HEADBUF
           
