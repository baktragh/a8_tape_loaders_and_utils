;======================================================
;MiniTBL (Turbo 2000 - Kilobyte blocks, Czechoslovakia)
;======================================================

;
; Equates
;
            BUFRLO=50
            BUFRHI=51
            BFENLO=52
            BFENHI=53
            
            LOADERSTART=2816
            ROTDEV_INIT=$0E53
            BLOAD_START=$0CCE

            OPT H+,F-
             

;
; Start of code
;
            ORG 8192
LOADER            
            INS "minitbl.bin"
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
            JSR ROTDEV_INIT
            JMP BLOAD_START
;
; Run segment
;
            ORG 736
            .WORD MOVDAT
