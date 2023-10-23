;===============================================================================
;KSO Turbo 2000 - Speedy 2700 binary loader wrapper
;===============================================================================

;-------------------------------------------------------------------------------
; Equates
;-------------------------------------------------------------------------------
            ICL 'equates.asm'

            LOADERENTRY=$0700
            LOADERSTART=$0700
;-------------------------------------------------------------------------------
; Start of code
;-------------------------------------------------------------------------------
            OPT H+
            ORG 8192



LOADER
            INS 'speedy2700_core.bin'

LOADER_END


LENPTR      .BYTE <[LOADER_END-LOADER],>[LOADER_END-LOADER]
            
            
; Entry - force WARM START and then continue
ENTRY       lda #0                        ;Warm start
            sta COLDST                    
            lda #02                       ;Cassette boot successfull
            sta BOOT
            lda #<BEGIN                   ;CASINI to loader entry
            sta CASINI
            lda #>BEGIN
            sta CASINI+1
            
            JMP WARMSV                    ;Warm start
; Display a title screen (optional)
BEGIN
.IF TITLE=1
            jsr TITLE_SCREEN
.ENDIF

; Relocate the loader to the desired location
            LDA #<LOADER
            STA BUFRLO
            LDA #>LOADER
            STA BUFRHI
            
            LDA #<LOADERSTART
            STA BFENLO
            LDA #>LOADERSTART
            STA BFENHI
            
            LDY #0
            LDX LENPTR+1
            BEQ MVPART
MVPAGE
            LDA (BUFRLO),Y
            STA (BFENLO),Y
            INY
            BNE MVPAGE
            INC BUFRLO+1
            INC BFENLO+1
            DEX
            BNE MVPAGE
MVPART
            LDX LENPTR
            BEQ MVEXIT
MVLAST
            LDA (BUFRLO),Y
            STA (BFENLO),Y
            INY
            DEX
            BNE MVLAST
MVEXIT

;-------------------------------------------------------------------------------
; Start loader
;-------------------------------------------------------------------------------
            JMP LOADERENTRY

;Display the title screen
.IF TITLE=1
;-------------------------------------------------------------------------------
; Title screen
;-------------------------------------------------------------------------------
           CIO0_OP   =$0342
           CIO0_STAT =$0343
           CIO0_BUFLO=$0344
           CIO0_BUFHI=$0345
           CIO0_LENLO=$0348
           CIO0_LENHI=$0349
           CIO0_AUX1 =$034A
           CIO0_AUX2 =$034B

TITLE_SCREEN   
           lda #$3C
           sta PACTL
 
           lda #9                  ;Requesting PRINT
           sta CRSINH
           sta CIO0_OP
           lda #<TITLE_CHARS
           sta CIO0_BUFLO
           lda #>TITLE_CHARS
           sta CIO0_BUFHI
           lda #<TITLE_CHARS_LEN
           sta CIO0_LENLO
           ldx #0                  ;Channel 0
           stx CIO0_LENHI
           jsr CIOV                ;Call CIO

           lda #0
           sta RTCLOK+2
           sta RTCLOK+1 
DTICK      lda RTCLOK+1
DLOOP      cmp RTCLOK+1
           beq DLOOP

           rts 

TITLE_CHARS .BYTE 125
            .BYTE 29,29,29,29,29
            .BYTE 31,31,31,31,31,31,31,31,31,31,31,31
            .BYTE 'SPEEDY 2700'
            .BYTE 29,29
            .BYTE 30,30,30,30,30,30,30,30,30,30,30,30,30,30,30
            .BYTE 'File''s Turbo Loader'
            .BYTE 29
            .BYTE 30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30
            .BYTE '-------------------'
            .BYTE 29
            .BYTE 30,30,30,30,30,30,30,30,30,30
            .BYTE 30,30,30,30,30,30,30,30,30,30,30
            .BYTE '(C)*AJEK  WARSZAWA 1990' 
            .BYTE 155
            
TITLE_CHARS_LEN = *-TITLE_CHARS
.ENDIF            
;-------------------------------------------------------------------------------
; Run segment
;-------------------------------------------------------------------------------

            ORG 736
            .BYTE <ENTRY,>ENTRY
          
