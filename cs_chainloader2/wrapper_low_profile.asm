;Chainloader 2 (Turbo 2000, Czechoslovakia)
;Low-profile version

;
; Equates
;
BUFRLO EQU 50
BUFRHI EQU 51
BFENLO EQU 52
BFENHI EQU 53
LOADERENTRY EQU $80
LOADERSTART EQU $80

;
; Start of code
;
            ORG  8192
LOADER            
            INS 'chainloader2_low_profile.bin'
LOADER_END
LENPTR      DTA b(<[LOADER_END-LOADER],>[LOADER_END-LOADER])
            
            
; Relocator routine
MOVDAT
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

;
; Start loader
;
            ldx #255
            txs
            lda #0
            sta 580 
            JMP LOADERENTRY

;
; Run segment
;
            ORG 736
            DTA b(<MOVDAT,>MOVDAT)
          