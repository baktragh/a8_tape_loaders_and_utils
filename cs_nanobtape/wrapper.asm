;======================================================
;NanoBTAPE (B-TAPE, Czech Republic)
;======================================================

;
; Equates
;
            BUFRLO=50
            BUFRHI=51
            BFENLO=52
            BFENHI=53
            
; Three variants of the loader            
.IF UNDER_ROM=0            
            LOADERSTART=2820
.ENDIF
.IF UNDER_ROM=1
            LOADERSTART=1796
.ENDIF
.IF UNDER_ROM=2
            LOADERSTART=1796
.ENDIF
            LOADERENTRY=LOADERSTART
            
            OPT H+,F-
;
; Start of code
;
            ORG 8192
LOADER            
.IF UNDER_ROM=0            
            INS "nanobtape.bin"
.ENDIF
.IF UNDER_ROM=1
            INS "nanobtape_ur.bin"
.ENDIF
.IF UNDER_ROM=2
            INS "nanobtape_u2.bin"
.ENDIF

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
            ORG 736
            .WORD MOVDAT
          
