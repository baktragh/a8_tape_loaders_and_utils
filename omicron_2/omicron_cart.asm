;===============================================================================
; Omicron Turbo - Cartridge loader
; This loader replaces omicron_miniblock
; Two variants of the cartridge:
; CARTTYPE=0 ; Phoenix-style 8KB cartridge
; CARTTYPE=1 ; Czechoslovak 2KB cartridge with button
;===============================================================================

;-------------------------------------------------------------------------------
; Equates
;-------------------------------------------------------------------------------
            BUFRLO=50
            BUFRHI=51
            BFENLO=52
            BFENHI=53
            LOADERENTRY=16384
            LOADERSTART=16384
            CRSINH=752
            DOSINI=12
            BASICF=1016
            BOOT=9
            GINTLK=1018
            COLDST=580
            SAVMSC=88
            WARMST=8
            AUXCODE=256
            WARMSV=58484
            
.IF CARTTYPE=0            
            CARTLEN=8192
            CARTBEGIN=40960+8192-CARTLEN     
            
.ENDIF
.IF CARTTYPE=1
            CARTLEN=2048
            CARTBEGIN=40960+8192-CARTLEN
.ENDIF
;-------------------------------------------------------------------------------
; Start of code
;-------------------------------------------------------------------------------
            *=CARTBEGIN

;-------------------------------------------------------------------------------
; Loader
;-------------------------------------------------------------------------------
LOADER            
            .INCBIN omicron_stage0.cbin
LOADER_END
LENPTR      .BYTE <[LOADER_END-LOADER],>[LOADER_END-LOADER]
            
;-------------------------------------------------------------------------------            
; Copy the loader to its real location
;-------------------------------------------------------------------------------
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
;-------------------------------------------------------------------------------            
; Prepare for reboot
;-------------------------------------------------------------------------------            
MVEXIT      LDA  #1            
            STA  CRSINH         ;Hide cursor
            STA  BOOT           ;Indicate disk boot successful
            STA  BASICF         ;Disable BASIC after warm start
            STA  WARMST
            LDA  #<LOADERENTRY  ;Set INIT address for disk boot
            STA  DOSINI
            LDA  #>LOADERENTRY
            STA  DOSINI+1
;-------------------------------------------------------------------------------            
; Place auxiliary code to RAM
;-------------------------------------------------------------------------------                        
            LDA  #$8D           ;Place STA $D500
            STA  AUXCODE+0
            LDA  #$00
            STA  AUXCODE+1
            LDA  #$D5
            STA  AUXCODE+2
            
            LDA  #$4C           ;Place JMP WARMSV
            STA  AUXCODE+3
            LDA  #<WARMSV
            STA  AUXCODE+4
            LDA  #>WARMSV
            STA  AUXCODE+5
            
            LDA  #$F0           ;Set infinite loop 
            STA  AUXCODE+6            
            LDA  #$FE
            STA  AUXCODE+7
            
;-------------------------------------------------------------------------------
; Pass control to the stage 0 loader
;-------------------------------------------------------------------------------
            LDA  #0             ;Indicate cartridge is not used
            STA  GINTLK   
            
.IF CARTTYPE=1            
;-------------------------------------------------------------------------------
; Now prompt for RESET
;-------------------------------------------------------------------------------
            ldy  #0
DISP_LOOP   lda  S_RESET,y
            sta  (SAVMSC),y
            iny  
            cpy  #[S_RESET_L]
            bne  DISP_LOOP
.ENDIF            
;-------------------------------------------------------------------------------
; Wait for RESET
;-------------------------------------------------------------------------------
.IF CARTTYPE=1
            JMP  AUXCODE+6        ;Go to loop and wait for reset
.ENDIF            
            
;-------------------------------------------------------------------------------
; Call loader directly
;-------------------------------------------------------------------------------            
.IF CARTTYPE=0
            JMP AUXCODE           ;Disconnect cartridge and call loader
.ENDIF            
;-------------------------------------------------------------------------------            
; Cartridge initialization
;-------------------------------------------------------------------------------            
CARTINIT    RTS                  ;Cartridge initialization - do nothing
;-------------------------------------------------------------------------------
; Strings
;-------------------------------------------------------------------------------
S_RESET     .SBYTE "Omicron Turbo Cartridge      Press "
            .SBYTE +$80,"RESET"
            S_RESET_L=[*-S_RESET]   
;-------------------------------------------------------------------------------
; End of used areas
;-------------------------------------------------------------------------------
CARTEND
;-------------------------------------------------------------------------------
; Filler
;-------------------------------------------------------------------------------            
            .DC [CARTLEN-6-[CARTEND-CARTBEGIN]] $55    
            
;-------------------------------------------------------------------------------            
; Cartridge trailer
;-------------------------------------------------------------------------------
            .WORD  MOVDAT        ;Cartridge RUN
            .BYTE  $00           ;Zero - cartridge present
            .BYTE  $04           ;Option - initialize and start cartridge
            .WORD  CARTINIT      ;Cartridge INIT
          
