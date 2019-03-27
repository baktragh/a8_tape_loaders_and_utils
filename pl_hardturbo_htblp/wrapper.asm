;===============================================
;HTBL+ (Hard Turbo, Poland)
;===============================================
;
;Enhanced binary loader for Hard Turbo
;Wrapper and relocator portion

;
; Equates
;
            BUFRLO=50
            BUFRHI=51
            BFENLO=52
            BFENHI=53

;
; Start of code
;
.BANK
            *= 8192
LOADER            
            .INCBIN htblp.bin
LOADER_END


LENPTR      .BYTE <[LOADER_END-LOADER],>[LOADER_END-LOADER]
            
            
; Relocator routine
MOVDAT
            LDA #<LOADER
            STA BUFRLO
            LDA #>LOADER
            STA BUFRHI
            
            LDA #<1999
            STA BFENLO
            LDA #>1999
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
            JMP 1999

;
; Run segment
;
.BANK
            *=736
            .BYTE <MOVDAT,>MOVDAT
          