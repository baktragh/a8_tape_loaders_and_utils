;===============================================
;SuperChain 2 (Super Turbo, Czechoslovakia)
;===============================================

;
; Equates
;
            BUFRLO=50
            BUFRHI=51
            BFENLO=52
            BFENHI=53
            LOADERENTRY=1800
            LOADERSTART=1800
;
; Start of code
;
            *= 8192
LOADER            
            .INCBIN superchain2.bin
LOADER_END


LENPTR      .BYTE <[LOADER_END-LOADER],>[LOADER_END-LOADER]
            
            
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
            JMP LOADERENTRY

;
; Run segment
;
            *=736
            .BYTE <MOVDAT,>MOVDAT
          