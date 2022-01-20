;
; System equates
;
; OS EQUATES
; ----------
; 
; Syntax:
; Use '=' for addresses
; and '<' for read addresses (ex: KBCODE)
; and '>' for write addresses (ex: STIMER)
; and '#' for values
; 
; IO EQUATES
; 
ICHID       = $0000
ICDNO       = $0001
ICCOM       = $0002
ICSTA       = $0003
ICBAL       = $0004
ICBAH       = $0005
ICPTL       = $0006
ICPTH       = $0007
ICBLL       = $0008
ICBLH       = $0009
ICAX1       = $000A
ICAX2       = $000B
ICAX3       = $000C
ICAX4       = $000D
ICAX5       = $000E
ICAX6       = $000F
; 
; DISPLAY LIST EQUATES
; 
ADLI        = $0080
AVB         = $0040
ALMS        = $0040
AVSCR       = $0020
AHSCR       = $0010
AJMP        = $0001
AEMPTY1     = $0000
AEMPTY2     = $0010
AEMPTY3     = $0020
AEMPTY4     = $0030
AEMPTY5     = $0040
AEMPTY6     = $0050
AEMPTY7     = $0060
AEMPTY8     = $0070
; 
; OS VARIABLES FOR XL/XE
; 
; PAGE 0
; 
NGFLAG      = $0001
CASINI      = $0002
RAMLO       = $0004
TRAMSZ      = $0006
CMCMD       = $0007
WARMST      = $0008
BOOT        = $0009
DOSVEC      = $000A
DOSINI      = $000C
APPMHI      = $000E
POKMSK      = $0010
BRKKEY      = $0011
RTCLOK      = $0012
BUFADR      = $0015
ICCOMT      = $0017
DSKFMS      = $0018
DSKUTL      = $001A
ABUFPT      = $001C
ICHIDZ      = $0020
ICDNOZ      = $0021
ICCOMZ      = $0022
ICSTAZ      = $0023
ICBALZ      = $0024
ICBAHZ      = $0025
ICPTLZ      = $0026
ICPTHZ      = $0027
ICBLLZ      = $0028
ICBLHZ      = $0029
ICAX1Z      = $002A
ICAX2Z      = $002B
ICAX3Z      = $002C
ICAX4Z      = $002D
ICAX5Z      = $002E
ICAX6Z      = $002F
XSTATUS      = 137
XCHKSUM      = 128
XBUFRLO      = 129
XBUFRHI      = 130
XBFENLO      = 131
XBFENHI      = 132
XLTEMP       = 133
XBUFRFL      = 135
XRECVDN      = 136
XMTDON      = $003A
CHKSNT      = $003B
NOCKSM      = $003C
BPTR        = $003D
FTYPE       = $003E
FEOF        = $003F
FREQ        = $0040
SOUNDR      = $0041
CRITIC      = $0042
FMSZPG      = $0043
ZCHAIN      = $004A
DSTAT       = $004C
ATRACT      = $004D
DRKMSK      = $004E
COLRSH      = $004F
TEMP        = $0050
HOLD1       = $0051
LMARGN      = $0052
RMARGN      = $0053
ROWCRS      = $0054
COLCRS      = $0055
DINDEX      = $0057
SAVMSC      = $0058
OLDROW      = $005A
OLDCOL      = $005B
OLDCHR      = $005D
OLDADR      = $005E
FKDEF       = $0060
PALNTS      = $0062
LOGCOL      = $0063
ADRESS      = $0064
MLTTMP      = $0066
SAVADR      = $0068
RAMTOP      = $006A
BUFCNT      = $006B
BUFSTR      = $006C
BITMSK      = $006E
SHFAMT      = $006F
ROWAC       = $0070
COLAC       = $0072
ENDPT       = $0074
DELTAR      = $0076
DELTAC      = $0077
KEYDEF      = $0079
SWPFLG      = $007B
HOLDCH      = $007C
INSDAT      = $007D
COUNTR      = $007E
LOMEM       = $0080
; 
; PAGE 2
; 
VDSLST      = $0200
VPRCED      = $0202
VINTER      = $0204
VBREAK      = $0206
VKEYBD      = $0208
VSERIN      = $020A
VSEROR      = $020C
VSEROC      = $020E
VTIMR1      = $0210
VTIMR2      = $0212
VTIMR4      = $0214
VIMIRQ      = $0216
CDTMV1      = $0218
CDTMV2      = $021A
CDTMV3      = $021C
CDTMV4      = $021E
CDTMV5      = $0220
VVBLKI      = $0222
VVBLKD      = $0224
CDTMA1      = $0226
CDTMA2      = $0228
CDTMF3      = $022A
SRTIMR      = $022B
CDTMF4      = $022C
INTEMP      = $022D
CDTMF5      = $022E
SDMCTL      = $022F
SDLSTL      = $0230
SDLSTH      = $0231
SSKCTL      = $0232
SPARE       = $0233
LPENH       = $0234
LPENV       = $0235
BRKKY       = $0236
VPIRQ       = $0238
CDEVIC      = $023A
CCOMND      = $023B
CAUX1       = $023C
CAUX2       = $023D
TMPSIO      = $023E
ERRFLG      = $023F
DFLAGS      = $0240
DBSECT      = $0241
BOOTAD      = $0242
COLDST      = $0244
RECLEN      = $0245
DSKTIM      = $0246
PDVMSK      = $0247
SHPDVS      = $0248
PDMSK       = $0249
RELADR      = $024A
PPTMPA      = $024C
PPTMPX      = $024D
CHSALT      = $026B
VSFLAG      = $026C
KEYDIS      = $026D
FINE        = $026E
GPRIOR      = $026F
PADDL0      = $0270
PADDL1      = $0271
PADDL2      = $0272
PADDL3      = $0273
PADDL4      = $0274
PADDL5      = $0275
PADDL6      = $0276
PADDL7      = $0277
STICK0      = $0278
STICK1      = $0279
STICK2      = $027A
STICK3      = $027B
PTRIG0      = $027C
PTRIG1      = $027D
PTRIG2      = $027E
PTRIG3      = $027F
PTRIG4      = $0280
PTRIG5      = $0281
PTRIG6      = $0282
PTRIG7      = $0283
STRIG0      = $0284
STRIG1      = $0285
STRIG2      = $0286
STRIG3      = $0287
HIBYTE      = $0288
WMODE       = $0289
BLIM        = $028A
IMASK       = $028B
JVECK       = $028C
NEWADR      = $028E
TXTROW      = $0290
TXTCOL      = $0291
TINDEX      = $0293
TXTMSC      = $0294
TXTOLD      = $0296
CRETRY      = $029C
HOLD3       = $029D
SUBTMP      = $029E
HOLD2       = $029F
DMASK       = $02A0
TMPLBT      = $02A1
ESCFLG      = $02A2
TABMAP      = $02A3
LOGMAP      = $02B2
INVFLG      = $02B6
FILFLG      = $02B7
TMPROW      = $02B8
TMPCOL      = $02B9
SCRFLG      = $02BB
HOLD4       = $02BC
DRETRY      = $02BD
SHFLOC      = $02BE
BOTSCR      = $02BF
PCOLR0      = $02C0
PCOLR1      = $02C1
PCOLR2      = $02C2
PCOLR3      = $02C3
COLOR0      = $02C4
COLOR1      = $02C5
COLOR2      = $02C6
COLOR3      = $02C7
COLOR4      = $02C8
RUNADR      = $02C9
HIUSED      = $02CB
ZHIUSE      = $02CD
GBYTEA      = $02CF
LOADAD      = $02D1
ZLOADA      = $02D3
DSCTLN      = $02D5
ACMISR      = $02D7
KRPDER      = $02D9
KEYREP      = $02DA
NOCLIK      = $02DB
HELPFG      = $02DC
DMASAV      = $02DD
PBPNT       = $02DE
PBUFSZ      = $02DF
RUNAD       = $02E0
INITAD      = $02E2
RAMSIZ      = $02E4
MEMTOP      = $02E5
MEMLO       = $02E7
HNDLOD      = $02E9
DVSTAT      = $02EA
CBAUDL      = $02EE
CBAUDH      = $02EF
CRSINH      = $02F0
KEYDEL      = $02F1
CH1         = $02F2
CHACT       = $02F3
CHBAS       = $02F4
NEWROW      = $02F5
NEWCOL      = $02F6
ROWINC      = $02F8
COLINC      = $02F9
CHAR        = $02FA
ATACHR      = $02FB
CH          = $02FC
FILDAT      = $02FD
DSPFLG      = $02FE
SSFLAG      = $02FF
; 
; PAGE 3
; 
DDEVIC      = $0300
DUNIT       = $0301
DCOMND      = $0302
DSTATS      = $0303
DBUFLO      = $0304
DBUFHI      = $0305
DTIMLO      = $0306
DUNUSE      = $0307
DBYTLO      = $0308
DBYTHI      = $0309
DAUX1       = $030A
DAUX2       = $030B
TIMER1      = $030C
ADDCOR      = $030E
CASFLG      = $030F
TIMER2      = $0310
TEMP1       = $0312
TEMP2       = $0314
TEMP3       = $0315
SAVIO       = $0316
TIMFLG      = $0317
STACKP      = $0318
TSTAT       = $0319
HATABS      = $031A
PUPBT1      = $033D
PUPBT2      = $033E
PUPBT3      = $033F
IOCB0       = $0340
IOCB1       = $0350
IOCB2       = $0360
IOCB3       = $0370
IOCB4       = $0380
IOCB5       = $0390
IOCB6       = $03A0
IOCB7       = $03B0
PRNBUF      = $03C0
SUPERF      = $03E8
CKEY        = $03E9
CASSBT      = $03EA
CARTCK      = $03EB
DERRF       = $03EC
ACMVAR      = $03ED
BASICF      = $03F8
MINTLK      = $03F9
GINTLK      = $03FA
CHLINK      = $03FB
CASBUF      = $03FD
; 
; HARDWARE REGISTERS
; 
; GTIA
; 
M0PF        = $D000
HPOSP0      = $D000
M1PF        = $D001
HPOSP1      = $D001
M2PF        = $D002
HPOSP2      = $D002
M3PF        = $D003
HPOSP3      = $D003
P0PF        = $D004
HPOSM0      = $D004
P1PF        = $D005
HPOSM1      = $D005
P2PF        = $D006
HPOSM2      = $D006
P3PF        = $D007
HPOSM3      = $D007
M0PL        = $D008
SIZEP0      = $D008
M1PL        = $D009
SIZEP1      = $D009
M2PL        = $D00A
SIZEP2      = $D00A
M3PL        = $D00B
SIZEP3      = $D00B
P0PL        = $D00C
SIZEM       = $D00C
P1PL        = $D00D
GRAFP0      = $D00D
P2PL        = $D00E
GRAFP1      = $D00E
P3PL        = $D00F
GRAFP2      = $D00F
TRIG0       = $D010
GRAFP3      = $D010
TRIG1       = $D011
GRAFM       = $D011
TRIG2       = $D012
COLPM0      = $D012
TRIG3       = $D013
COLPM1      = $D013
PAL         = $D014
COLPM2      = $D014
COLPM3      = $D015
COLPF0      = $D016
COLPF1      = $D017
COLPF2      = $D018
COLPF3      = $D019
COLBK       = $D01A
PRIOR       = $D01B
VDELAY      = $D01C
GRACTL      = $D01D
HITCLR      = $D01E
CONSOL      = $D01F
; 
; POKEY
; 
POT0        = $D200
AUDF1       = $D200
POT1        = $D201
AUDC1       = $D201
POT2        = $D202
AUDF2       = $D202
POT3        = $D203
AUDC2       = $D203
POT4        = $D204
AUDF3       = $D204
POT5        = $D205
AUDC3       = $D205
POT6        = $D206
AUDF4       = $D206
POT7        = $D207
AUDC4       = $D207
ALLPOT      = $D208
AUDCTL      = $D208
KBCODE      = $D209
STIMER      = $D209
RANDOM      = $D20A
SKREST      = $D20A
POTGO       = $D20B
SERIN       = $D20D
SEROUT      = $D20D
IRQST       = $D20E
IRQEN       = $D20E
SKSTAT      = $D20F
SKCTL       = $D20F
; 
; PIA
; 
PORTA       = $D300
PORTB       = $D301
PACTL       = $D302
PBCTL       = $D303
; 
; ANTIC
; 
DMACLT      = $D400
CHACTL      = $D401
DLISTL      = $D402
DLISTH      = $D403
HSCROL      = $D404
VSCROL      = $D405
PMBASE      = $D407
CHBASE      = $D409
WSYNC       = $D40A
VCOUNT      = $D40B
PENH        = $D40C
PENV        = $D40D
NMIEN       = $D40E
NMIST       = $D40F
NMIRES      = $D40F
; 
; FLOATING POINT ROUTINES
; 
AFP         = $D800
FASC        = $D8E6
IFP         = $D9AA
FPI         = $D9D2
ZFR0        = $DA44
ZF1         = $DA46
FSUB        = $DA60
FADD        = $DA66
FMUL        = $DADB
FDIV        = $DB28
PLYEVL      = $DD40
FLD0R       = $DD89
FLD0P       = $DD8D
FLD1R       = $DD98
FLD1P       = $DD9C
FSTOR       = $DDA7
FSTOP       = $DDAB
FMOVE       = $DDB6
EXP         = $DDC0
EXP10       = $DDCC
LOG         = $DECD
LOG10       = $DED1
; 
; ROM VECTORS
; 
DSKINV      = $E453
CIOV        = $E456
SIOV        = $E459
SETVBV      = $E45C
SYSVBV      = $E45F
XITVBV      = $E462
SIOINV      = $E465
SENDEV      = $E468
INTINV      = $E46B
CIOINV      = $E46E
SELFSV      = $E471
WARMSV      = $E474
COLDSV      = $E477
RBLOKV      = $E47A
CSOPIV      = $E47D
PUPDIV      = $E480
SELFTSV     = $E483
PENTV       = $E486
PHUNLV      = $E489
PHINIV      = $E48C
GPDVV       = $E48F
;
; Code equates
;
L0400       = $0400
L08FE       = $08FE
L212F       = $212F
L616F       = $616F
L6570       = $6570
LE410       = $E410
LE411       = $E411
LE424       = $E424
LE425       = $E425

            
;==============================================================================
; Turbo 6000 - Chaos Loader
;==============================================================================

; Start of code
            *= $0700
; Boot header
            .byte $00          ; Boot flag
            .byte $06          ; Number of blocks
L0701       .byte $00,$07      ; Load address
            .byte $08,$07      ; CASINI address
            
;Boot INIT 
L0706       clc                    ; Indicate boot OK
L0707       rts                    ; And return

;Initialization
L0708       lda #$3C               ; Motor off
            sta PACTL              ; 
            lda #$08               ; Reset CONSOL
            sta CONSOL             ; 
            lda CONSOL             ; Check CONSOL
            and #$01               ; OPTION+SELECT pressed?
            bne L071C              ; Yes, skip
            jmp COLDSV             ; Otherwise cold start
            
L071C       lda #$70               ; Set DOSVEC to $0770
            sta DOSVEC             ; 
L0720       lda #$07               ; 
            sta DOSVEC+1           ; 
            
            lda #$00               ; Set MEMLO to $0A00
            sta MEMLO              ; 
            lda #$0A               ; 
            sta MEMLO+1            ; 
            rts                    ; Return
            nop                    ; EA
            
;Setup display list subroutine            
L0730       lda #$98               ; Setup display list to $0880
L0732       sta L0892              ; 
            lda #$80               ; 
            sta SDLSTL             ; 
            lda #$08               ; 
            sta SDLSTH             ; 
            
L073F       dey                    ; Decrement Y
            bne L073F              ; IF not zero, loop
L0742       dex                    ; Decrement X
            bne L073F              ; IF not zero, loop
            rts                    ; Return
            
            bne L0742              ; If not zero, loop

;Wait for key           
WFORKEY     lda #$40               ; Set all caps
            sta SHFLOC             ; 
            jsr L0757              ; Store K: handler vector to stack
            iny                    ; Increment Y
            dey                    ; Decrement Y
            bmi WFORKEY            ; If minus, loop
            and #$7F               ; And A with $7F
            rts                    ; Return

L0757       lda LE425              ; 
            pha                    ; 
            lda LE424              ; 
            pha                    ; 
            rts                    ; 

;             
GODEC_H_OR_M sei                    ; Disable interrupts
             lda #$00               ; Disable NMI
             sta NMIEN              ; 
             jsr DEC_H_OR_M         ; Decode file
             lda #$40               ; Enable VBI
             sta NMIEN              ; 
             cli                    ; Re-enable interrupts
             rts                    ; Return

;------------------------------------------------------------------------------
; Entry point            
;------------------------------------------------------------------------------
; Initialization
L0770       cld                    ; Clear the decimal flag
            ldx #$FF               ; Reset stack pointer
            txs                    ; 
            jsr L0730              ; Setup display list
            nop                    ; 
            ldx #$01               ; X=1
            stx BOOT               ; Boot successful
            dex                    ; X=0
            ldy L0701              ; Y=0
            
L0780       stx L0701              ; Store X to $0701 (zero)
            bne L0780              ; If X not zero, loop

;Clear display            
            ldy #$00               ; Y=0
L0787       lda L08D0,Y            ; Move 20 bytes from $08D0 to $08E0
            sta L08E0,Y            ; 
            iny                    ; 
            cpy #$14               ; 
            bne L0787              ; 


;Decode header
            ldy #$14               ; Y=20
            sty XBUFRFL             ; XBUFRFL=20 (Decode 20 bytes)
            stx XBUFRHI             ; XBUFRHI=0 (Indicates decoding header)
            lda #$E0               ; Header is placed to $08E0
            sta XLTEMP              ; 
            lda #$08               ; 
            sta XLTEMP+1            ; 
            
            jsr GODEC_H_OR_M       ; Decode header
            
            lda #$AA               ; A=$AA
            jsr L0732              ; Display
            
;Decode main block            
            jsr L07D0              ; Setup buffer for the main block
            jsr GODEC_H_OR_M       ; Go to decode file
            lda XSTATUS             ; Check status
            bne L0770              ; If not zero, start over
            
            lda #$B8               ; A=$B8
            jsr L0732              ; Store to display (?)
            lda XBFENHI             ; Check XBFENHI
            cmp RAMSIZ             ; Compare with RAMSIZ
            bcs L0800              ; B0 42
            jsr L07C4              ; Call K: handler function
            jmp L0800              ; 4C 00 08
            
;Call another K: handler function
L07C4       lda LE411              ; AD 11 E4
            pha                    ; 48
            lda LE410              ; AD 10 E4
            pha                    ; 48
            rts                    ; 60
            
            nop                    ; EA
            nop                    ; EA
            nop                    ; EA
            
;Prepare buffer for decoding            
L07D0       lda RAMSIZ             ; Check RAM size
            cmp XBFENHI             ; Compare with buffer end high byte
            bcs L07DD              ; If buffer below ram size, continue
            dec XBUFRHI             ; Otherwise lower the buffer
            dec XBFENHI             ; 
            bne L07D0              ; And if not zero, try again
            
L07DD       jsr WFORKEY            ; Wait for key to be pressed
            cmp #$56               ; Was that 'X' >?
            bne L07FE              ; No, then skip all this
            
            sec                    ; Set carry
            lda XBUFRLO             ; Get XBUFRLO
            sbc #$E0               ; Subtract $E0
            sta XBUFRLO             ; And update XBUFRLO
            lda XBUFRHI             ; Get XBUFRHI
            sbc #$03               ; Subtract $03
            sta XBUFRHI             ; And update XBUFRHI
            sec                    ; Set carry
            lda XBFENLO             ; Get XBFENLO
            sbc #$E0               ; Subtract $E0
            sta XBFENLO             ; Update XBFENLO
            lda XBFENHI             ; Get XBFENHI
            sbc #$03               ; Subtract $03
            sta XBFENHI             ; Update XBFENHI
L07FE       rts                    ; Return
            rts                    ; 60
            
;Move first 256 bytes from the buffer to 1024...            
L0800       ldx #$00               ; X=0
L0802       jsr BF_GBYT            ; Get next byte
            sta L0400,X            ; Store the byte
            inx                    ; Increment X
            bne L0802              ; If not zero, keep getting bytes
            
            inx                    ; X=2
            inx                    ; 
            stx XSTATUS             ; Store X to status.Status = 2
            nop                    ; Do nothing
            
L0810       ldx #$7F               ; X=$7F
            lda #$08               ; A=$08
            stx INITAD             ; Set INITAD to $087F (RTS)
            sta INITAD+1           ; 
            
            jsr L0869              ; Get segment header
L081D       jsr L0844              ; Get next segment byte
            bcs L083D              ; If all bytes done, run the program
            
            sta (XLTEMP),Y          ; Store segment byte 
            inc XLTEMP              ; Update pointer
            bne L082A              ; 
            inc XLTEMP+1            ; 
L082A       ldy XBUFRFL             ; Check for end of segment
            lda XRECVDN             ; 
            cpy XLTEMP              ; 
            sbc XLTEMP+1            ; 
            bcs L081D              ; No end yet, continue
            
            jsr L083A              ; Perform INIT vector
            jmp L0810              ; Reset INIT vector
L083A       jmp (INITAD)           ; 
L083D       jmp (RUNAD)            ; Run the loaded program



; Get two bytes. First in X, second in A
L0840       jsr L0844              ; Get next byte
            tax                    ; Transfer the byte to X
            
L0844       ldy XBUFRLO             ; Get pointer LO
            lda XBUFRHI             ; Get Pointer HI
            cpy XBFENLO             ; Compare with buffer end Lo
            sbc XBFENHI             ; Subtract buffer end Hi
            bcs L087F              ; All bytes done. just return
            ldy XSTATUS             ; Get XSTATUS
            beq BF_GBYT            ; If zero, then get next byte
            lda L0400,Y            ; Otherwise get from the 256 bytes buffer
            inc XSTATUS             ; Increase pointer in the 256 bytes buffer
            bcc L0866              ; If not over 256 bytes, return
            
;Get next byte from the buffer            
BF_GBYT     lda (XBUFRLO),Y         ; Get byte from buffer
            pha                    ; Push the byte to stack
            tya                    ; A=Y
            sta (XBUFRLO),Y         ; Zero the byte in the buffer
            pla                    ; Restore byte from stack
            inc XBUFRLO             ; Update buffer pointer
            bne L0866              ; 
            inc XBUFRHI             ; 
L0866       ldy #$00               ; Y=0
            rts                    ; Return
            
;Get segment header (XLTEMP,XLTEMP+1 - XBUFRFL,XRECVDN)            
L0869       jsr L0840              ; Get next two bytes
            bcs L087F              ; If carry set, return
            stx XLTEMP              ; Store to XLTEMP
            sta XLTEMP+1            ; Store to XLTEMP+1
            and XLTEMP              ; And bytes
            cmp #$FF               ; Check for $FF,$FF
            beq L0869              ; Yes, then continue
            jsr L0840              ; No, Get next two bytes
            stx XBUFRFL             ; Store X to XBUFRFL
            sta XRECVDN             ; Store A to XRECVDN
L087F       rts                    ; Return

; Display list and screen data
            bvs L08F2              ; 70 70
            bvs L08F4              ; 70 70
            bvs L08F6              ; 70 70
            bvs L08F8              ; 70 70
            bvs L08D1              ; 70 47
            cpx #$08               ; E0 08
            bvs L08FE              ; 70 70
            bvs DEC_H_OR_M              ; 70 70
            bvs L08D8              ; 70 46
L0892       tya                    ; 98
            php                    ; 08
            bvs L08D7              ; 70 41
            .byte $80
            php                    ; 08
            .byte $00
            php                    ; 08
            .byte $23
            ora #$00               ; 09 00
            .byte $33,$34
            and (ICSTAZ,X)         ; 21 23
            plp                    ; 28
            asl A                  ; 0A
            .byte $33,$2F
            rol XBFENLO             ; 26 34
            .byte $07
            clc                    ; 18
            ora $0000,Y            ; 19 00 00
            .byte $00,$00,$00,$00,$00,$00
            bit L212F              ; 2C 2F 21
            bit $00                ; 24 00
            .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$33,$34
            and (XBUFRLO,X)         ; 21 32
            .byte $34,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
L08D0       .byte $00
L08D1       .byte $00,$00,$00,$74
            adc COLAC,X            ; 75 72
L08D7       .byte $62
L08D8       .byte $6F,$00
            jmp (L616F)            ; 6C 6F 61
            .byte $64
            adc COLAC              ; 65 72
L08E0       .byte $00,$74
            adc L6570,Y            ; 79 70 65
            .byte $00,$32
            and XBUFRHI             ; 25 33
            and XBFENLO             ; 25 34
            .byte $00,$74,$6F,$00
            jmp (L616F)            ; 6C 6F 61
L08F2       .byte $64,$00
L08F4       .byte $00,$00


L08F6       lda #$43               ; A9 43
L08F8       sta DOSVEC+1           ; 85 0B
            jmp L0720              ; 4C 20 07
            jsr L0707              ; 20 07 07
            
;================================================================================
; Decode file - Header or Main block. Depends on XBUFRHI
; Header (If XBUFRHI is zero):
;  Pilot tone $02,$02,...
;  Synchro sequence $09,$08,..,$01
;  5 bytes XCHKSUM,XBUFRLO,XBUFRHI,XBFENLO,XBFENHI
;  Other bytes (number of bytes given by XBUFRFL)
;
; Main block
;  Pilot tone $02,$02,...
;  Synchro sequence $09,$08,..,$01
;  Bytes of the block (Begins at XLTEMP,XLTEMP+1 ends at XBFENLO,XBFENHI)
;  Checksum byte (eor of all bytes, initialized to zero)
;================================================================================
DEC_H_OR_M  jsr PREP_SPORT         ; Initialize serial port
            lda XBUFRHI             ; Check XBUFRHI
            beq FINDBEG            ; If zero, skip
            sta XLTEMP+1            ; Otherwise Store to XLTEMP+1
            bne L0930              ; And adjust XBUFRLO

;------------------------------------------------------------------------------
; Decode header
;------------------------------------------------------------------------------
; Find beginning of a block            
FINDBEG     jsr BEGIN_DEC          ; Begin decoding of a block
            lda ICAX6Z             ; Check last byte of the synchro sequence
            cmp #$00               ; Compare with zero
            beq FINDBEG            ; If zero, then loop (expecting $01)

; Decode 5 more bytes
; XBUFRLO,XBUFRHI,XBFENLO,XBFENHI (and first data byte)
L0914       sta XCHKSUM,Y           ; Store the byte to checksum. Y is zero.
            jsr DEC_BYTE           ; Decode another byte
            iny                    ; Increment Y
            cpy #$05               ; IF Y not 5
            bne L0914              ; Then continue with another byte
            ldy #$00               ; Y=0

; Decode bytes. Number given by XBUFRFL            
L0921       sta (XLTEMP),Y          ; Store to XLTEMP,Y
            jsr DEC_BYTE           ; Decode byte
            iny                    ; Increment Y
            dec XBUFRFL             ; Decrement XBUFRFL
            bne L0921              ; IF not zero, loop
            sta XSTATUS             ; Store the byte to XSTATUS
            beq L0968              ; IF zero, terminate decoding with success
            nop                    ; 
            
L0930       lda XBUFRLO             ; Move XBUFRLO
            sta XLTEMP              ; To XLTEMP
            
;------------------------------------------------------------------------------
; Decode main block
;------------------------------------------------------------------------------            
DECF_MAIN   jsr BEGIN_DEC          ; Begin decoding of a block
            tax                    ; Beginning not found?
            bne DECF_MAIN          ; Right, try again
            
            sty XCHKSUM             ; Zero checksum
            dex                    ; Decrement X
            
            stx COLBK              ; Background
L0940       jsr DEC_BYTE           ; Decode byte
            sta (XLTEMP),Y          ; Store byte to buffer
            cpy #$00               ; IF Y=0
            beq L094D              ; Then skip comparison
            cmp (XLTEMP),Y          ; Compare the same byte
            bne L096A              ; IF not equal, terminate with error
            
L094D       eor XCHKSUM             ; Update checksum
            sta XCHKSUM             ; 
            inc XLTEMP              ; Update buffer pointer
            bne L0957              ; 
            inc XLTEMP+1            ; 
L0957       lda XLTEMP              ; Check buffer pointer
            cmp XBFENLO             ; Low byte
            lda XLTEMP+1            ; High byte
            sbc XBFENHI             ; 
            bcc L0940              ; If not end of file, continue
            jsr DEC_BYTE           ; Decode checksum byte
            cmp XCHKSUM             ; Compare with checksum
            sty XSTATUS             ; Store result to XSTATUS
L0968       beq STOP_SPORT         ; If zero, terminate with success

L096A       ldx #$FF               ; Status = $FF
            stx XSTATUS             ; 
            bne STOP_SPORT              ; Reinitialize
;------------------------------------------------------------------------------
; Begin decoding a block
; Returns Y=0 and last decoded byte
;------------------------------------------------------------------------------
BEGIN_DEC   lda #$70               ; Setup timers
            sta AUDCTL             ; 
            lda #$D2               ; 
            sta AUDF1              ; 
            lda #$01               ; 
            sta AUDF2              ; 
            nop                    ; 

; Wait for pilot tone (series of $02 bytes)            
W_PILOT     jsr DEC_BIT            ; Decode bit
            rol ICAX6Z             ; Rotate byte being decoded
            lda ICAX6Z             ; Check the byte
            cmp #$02               ; If not 2
            bne W_PILOT            ; Then loop
            
;Wait for synchronization sequence ($09,$08,...$01)            
            ldy #$09               ; Set Y=9
L098D       jsr DEC_BYTE           ; Decode byte
            cmp #$02               ; Decoded byte is 2
            beq L098D              ; If yes, then loop
L0994       cpy ICAX6Z             ; If decoded byte is not Y
            bne W_PILOT            ; Then start over
            jsr DEC_BYTE           ; Deocde byte
            dey                    ; Decrement Y
            bne L0994              ; If not zero, continue decoding
            rts                    ; Return
            nop                    ; 

;------------------------------------------------------------------------------
; Decoding bits and bytes
;------------------------------------------------------------------------------

; Decode one byte
DEC_BYTE    lda #$08               ; Set bit counter to 8
            sta NGFLAG             ; Store to NGFLAG
L09A4       jsr DEC_BIT            ; Decode bit
            rol ICAX6Z             ; Rotate byte being decoded
            dec NGFLAG             ; Decrement bit counter
            bne L09A4              ; If not 8 bits, decode next bit
            lda ICAX6Z             ; Get the byte decoded
            rts                    ; Return
            
; Decode BIT            
DEC_BIT     lda #$01               ; Check IRQ status
L09B2       bit IRQST              ; Timer 1 IRQ?
            bne L09B2              ; No, loop
            
W_EDGE      lda PORTA              ; Touch PORTA
            lda #$80               ; Prepare mask
L09BC       bit PACTL              ; Check PACTL for interrupt status bit
            beq L09BC              ; If zero, keep looping
            lda IRQST              ; Check POKEY interrupt status
            pha                    ; Push to stack
            ldx #$00               ; Disable all interrupts
            stx IRQEN              ; 
            dex                    ; Re-enable all interrupts
            stx IRQEN              ; 
            stx STIMER             ; Start counting again
            lda PORTA              ; Touch PORTA
            pla                    ; Restore POKEY interrupt status
            eor #$03               ; Reverse bits
            lsr A                  ; Logical shift right 
            lsr A                  ; Logical shift right (CF has bit value of timer 2)
            rts                    ; Return
            
            nop                    ; EA
            nop                    ; EA
            nop                    ; EA
            nop                    ; EA
            nop                    ; EA
            nop                    ; EA
            
;------------------------------------------------------------------------------
; Decoding termination
;------------------------------------------------------------------------------
STOP_SPORT  lda #$3C               ; A9 3C

;------------------------------------------------------------------------------
; Decoding initialization - Serial port and DMA
;------------------------------------------------------------------------------
;Wait and prepare to disable dma
WAIT_SPORT  tay                    ; Y=A
L09E3       dex                    ; Decrement X
            bne L09E3              ; IF not zero, loop
            iny                    ; Increment Y
            bne L09E3              ; If not zero, loop
            sta PACTL              ; MOTOR ON + Enable Proceed interrupt
            lda #$22               ; DMA Enable value
            bne SET_DMA              ; Go to set DMA enable value
            
PREP_SPORT  lda #$36               ; Motor ON + Enable Proceed Interrupt
            sta PACTL        
            jsr WAIT_SPORT         ; Wait for a while and set DMA
            jsr W_EDGE             ; Wait for edge
            tya                    ; A=Y (Set zero)
;Set DMA            
SET_DMA     sta DMACLT             ; Control DMA
            rts                    ; and return
            
LRUNIT      jsr L0708
            jmp (DOSVEC)
;
            *= $02E0

            .word L0770
;
         
