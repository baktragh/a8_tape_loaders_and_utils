;===============================================================================
; Standard Tape Records Self-extractor skeleton   
; Assemble with the MADS assembler
;===============================================================================
                 ICL "equates.asm" 
                 OPT H+,F-
                 
;===============================================================================
; Global constants
;===============================================================================
                 ICL "../commons/gconstants.asm"                          
;===============================================================================
; Private constants
;===============================================================================
                START_ADDR      = $0AF0
                
                ZP_TAB_PTR_LO   = 128
                ZP_TAB_PTR_HI   = 129
                
                ZP_BLOCKFLAG    = 130
                
                ZP_SIO_BUFRLO   = 131
                ZP_SIO_BUFRHI   = 132
                ZP_SIO_LENLO    = 133
                ZP_SIO_LENHI    = 134

                ZP_IRG_LO       = 135
                ZP_IRG_HI       = 136
                ZP_BAUD_LO      = 137
                ZP_BAUD_HI      = 138
;=======================================================================
; INITITALIZATION CODE 
;=======================================================================
                ICL "../commons/preinit.asm"            
;=======================================================================
; MAINLINE CODE
;=======================================================================
                   ORG START_ADDR
                   jmp ENTRY_ADDR
;-----------------------------------------------------------------------
; Screen lines
;-----------------------------------------------------------------------
                ICL "../commons/screen.asm"
;-----------------------------------------------------------------------
; Configuration
;-----------------------------------------------------------------------
                ICL "../commons/gconfig.asm"
;------------------------------------------------------------------------
;Initialization
;------------------------------------------------------------------------
ENTRY_ADDR         
                ICL "../commons/screeni.asm"                                       
;-----------------------------------------------------------------------
; Prepare to save
;-----------------------------------------------------------------------
READY_SAVE         lda #<DATA_TABLE        ;Reset the table and counter
                   sta ZP_TAB_PTR_LO
                   lda #>DATA_TABLE
                   sta ZP_TAB_PTR_HI

                   bit CFG_FLAGS
                   bmi SKIP_START         ;If $80 (composite)
                   jsr WAIT_FOR_START     ;Wait for START key
SKIP_START         jsr BEEP
;-------------------------------------------------------------------------------
;Initiate recording
; - Motor ON
; - Silence for file separation
; - Setup POKEY for writing cassette frames with customized IRGs
;-------------------------------------------------------------------------------
                   jsr WAIT_FOR_VBLANK
                   jsr TWIO_Init
            
                   lda #52
                   sta PACTL

                   ldy CFG_SAFETY_DELAY   ;Presume just safety delay
                   bit CFG_FLAGS          ;Check if long separator requested
                   bvc NORM_SEP           ;No, stick with safety delay
                   ldy CFG_SEP_DURATION   ;Use delay for long separator
NORM_SEP           jsr DELAY_CUSTOM_Y     ;Make the delay
;-------------------------------------------------------------------------------
                   
;-------------------------------------------------------------------------------      
SAVE_LOOP          ldy #0                 ;Get buffer range
                   lda (ZP_TAB_PTR_LO),Y
                   sta BUFRLO
                   iny
                   lda (ZP_TAB_PTR_LO),Y
                   sta BUFRHI
                   iny 
                   lda (ZP_TAB_PTR_LO),Y
                   sta BFENLO
                   iny
                   lda (ZP_TAB_PTR_LO),Y
                   sta BFENHI

                   iny                   ;Get block flag
                   lda (ZP_TAB_PTR_LO),Y
                   sta ZP_BLOCKFLAG 
                                             
                   iny                   ;Get extended block flags
                   lda (ZP_TAB_PTR_LO),Y
                   sta ZP_BAUD_LO
                   iny
                   lda (ZP_TAB_PTR_LO),Y
                   sta ZP_BAUD_HI
                   iny
                   lda (ZP_TAB_PTR_LO),Y
                   sta ZP_IRG_LO
                   iny
                   lda (ZP_TAB_PTR_LO),Y
                   sta ZP_IRG_HI

                   lda BUFRLO             ;Check for terminator (all $FFs)
                   and BUFRHI
                   and BFENLO
                   and BFENHI
                   cmp #$FF
                   beq SAVE_TERM
    
SAVE_DOBLOCK       jsr WRITE_BLOCK

                   clc                          ;Increment table pointer
                   lda #9                       ;Table is 9 bytes long
                   adc ZP_TAB_PTR_LO
                   sta ZP_TAB_PTR_LO
                   bcc @+
                   inc ZP_TAB_PTR_HI  
@
SAVE_NEXTBLOCK     jmp SAVE_LOOP                ;Continue saving.
;-------------------------------------------------------------------------------
; Terminate the recording
; - Motor off
; - RESET POKEY
;-------------------------------------------------------------------------------
SAVE_TERM          lda #60                      ;Motor OFF
                   sta PACTL

                   jsr TWIO_TermPokey          ;Terminate POKEY

                   bit CFG_FLAGS                ;Check for composite flg ($80)
                   bmi SAVE_QUIT                ;If composite, quit
                   lda CFG_FLAGS                ;Check for alarm
                   and #CFG_F_ALARM                
                   beq SAVE_AGAIN               ;No alarm, just skip

SAVE_ALARM         jsr BEEP                     ;Three beeps for alarm
                   jsr BEEP
                   jsr BEEP
SAVE_AGAIN         jmp READY_SAVE               ;Otherwise start over    
SAVE_QUIT          rts          
;=======================================================================
; Common Auxiliary Subroutines
;=======================================================================
                   ICL "../commons/aux.asm"                   
;=======================================================================
; Private Auxiliary Subroutines
;=======================================================================                   
;===============================================================================
; Write block of data
; Inputs:
; BUFRLO,BUFRHI  - First byte
; BFENLO,BFENHI  - Last byte
;===============================================================================
WRITE_BLOCK        


WB_RANGE
;Calculate the length of the block for TWIO call
                   sec
                   lda BFENLO
                   sbc BUFRLO
                   sta DBYTLO
                   lda BFENHI
                   sbc BUFRHI
                   sta DBYTHI

                   inc DBYTLO
                   bne @+
                   inc DBYTHI
@
;Setup the buffer pointer for the TWIO call.
                   lda BUFRLO
                   sta DBUFLO
                   lda BUFRHI
                   sta DBUFHI

;Now prepare the TWIO call
                   lda ZP_IRG_LO         ;Set IRG duration       
                   sta DDEVIC
                   lda ZP_IRG_HI
                   sta DUNIT             
          
                   lda ZP_BAUD_LO        ;Set baud rate
                   sta DAUX1
                   lda ZP_BAUD_HI
                   sta DAUX2    
                   jsr TWIO_Entry         ;Call SIO 

                   rts
;===============================================================================
; Tape related subroutines
; TWIO - SIO routine stripped off everything that is not related to writing
;        cassette frames, some modification for adjustable baud rate. The code
;        was taken from the AltirraOS sources.
;===============================================================================
                   TWIOSuccess = $01
;-------------------------------------------------------------------------------
; TWIO Entry point
; DBUFLO, DBUFHI - Buffer pointer
; DBYTLO, DBYTHI - Number of bytes
; DAUX1 - Baud rate lo (pokey settings)
; DAUX2 - Baud rate hi (pokey settings)
; DDEVIC - IRG lo (number of VBLs)
; DUNIT - IRG hi (number of VBLs)
;-------------------------------------------------------------------------------
TWIO_Entry
                    ;set retry counters
                    mva #$01 dretry
                    ;enter critical section
                    sta critic

                    tsx
                    stx stackp
                    
                    ;Set timeout timer address -- MUST be done on each call 
                    jsr TWIO_SetTimeoutVector

                    ;Init POKEY hardware
                    jsr TWIO_SE_InitHw

                    ;Go do cassette now
                    jmp TWIO_Cassette                    
                    
TWIO_Exit:
                    ldx stackp
                    txs

                    lda #0
                    sta critic

                    cpy #0               
                    sty dstats
                    sty status
                    rts

                    ldy #TWIOSuccess
                    bne TWIO_Exit

;-------------------------------------------------------------------------------
; Initialize TWIO
;-------------------------------------------------------------------------------
TWIO_Init           ;turn off POKEY init mode so polynomial counters and audio run
                    mva #3 skctl
                    sta sskctl
                    
                    ;enable noisy sound, documented to be 3
                    sta soundr

                    lda #<TWIO_OutputReadyHandler
                    sta VSEROR
                    lda #>TWIO_OutputReadyHandler
                    sta VSEROR+1

                    lda #<TWIO_OutputCompleteHandler
                    sta VSEROC
                    lda #>TWIO_OutputCompleteHandler
                    sta VSEROC+1

                    rts
;-------------------------------------------------------------------------------
; Set timeout vector
;-------------------------------------------------------------------------------'
TWIO_SetTimeoutVector

                    mwa #TWIO_Countdown1Handler cdtma1
                    rts
;-------------------------------------------------------------------------------
;TWIO send enable routine
;
; This is one of those routines that Atari inadvisably exposed in the OS jump
; table even though they shouldn't. Responsibilities of this routine are:
; - Hit SKCTL to reset serial hardware and init for sending
; - Hit SKRES to clear status
; - Enable send interrupts
; - Configure AUDF3/AUDF4 frequency (19200 baud or 600 baud)
; - Set AUDC3/AUDC4 for noisy or non-noisy audio
; - Set AUDCTL
;
; It does not init any of the TWIO variables, only hardware/shadow state.
;-------------------------------------------------------------------------------

TWIO_SendEnable
                    ;enable serial output ready IRQ and suppress serial output complete IRQ
                    lda pokmsk
                    ora #$10
                    and #$f7
                    sta pokmsk
                    sta irqen
TWIO_SendEnableNoIRQ
                    ;clear forced break mode and reset serial clocking mode to timer 4
                    ;synchronous; also enable two-tone mode if in cassette mode
                    lda sskctl
                    and #$0f
                    ora #$20
                    ldx #$FF
                    ora #$08
                    sta sskctl
                    sta skctl

TWIO_SE_InitHw      ldx #8

                    ;load POKEY audio registers
                    ldy #8
                    mva:rpl regdata_cassette_write,x- audf1,y-

                    ;Override the baud rate
                    lda DAUX1
                    sta AUDF3
                    lda DAUX2
                    sta AUDF4

                    lda #$a8
                    sta audc4
                    lda #$10
                    sta audc1
                    sta audc2
                    ;reset serial status
                    sta skrest
                    rts

regdata_cassette_write:
                    dta $05 ;audf1
                    dta $a0 ;audc1
                    dta $07 ;audf2
                    dta $a0 ;audc2
                    dta $cc ;audf3
                    dta $a0 ;audc3
                    dta $05 ;audf4
                    dta $a0 ;audc4
                    dta $28 ;audctl

;-------------------------------------------------------------------------------
; Setup buffer pointers
;-------------------------------------------------------------------------------
TWIO_SetupBufferPointers

                    clc
                    lda dbuflo
                    sta bufrlo
                    adc dbytlo
                    sta bfenlo
                    lda dbufhi
                    sta bufrhi
                    adc dbythi
                    sta bfenhi
                    rts

;-------------------------------------------------------------------------------
;TWIO send routine
;-------------------------------------------------------------------------------
TWIO_Send
                    ;configure serial port for synchronous transmisTWIOn
                    ;enable transmission IRQs
                    sei
                    jsr TWIO_SendEnable
                    
                    ldy #0
                    sty xmtdon
                    sty status
                    sty chksnt
                    
                    ;send first byte and set checksum (must be atomic)
                    lda (bufrlo),y
                    sta serout
                    sta chksum

                    ;unmask IRQs
                    cli
                    
                    ;wait for transmit to complete or Break to be pressed
wait:
                    lda brkkey
                    beq break_detected
                    lda xmtdon
                    beq wait
                    bne send_completed

break_detected:
                    ldy #$80
                    sty status
                    dec brkkey ;reset brkkey to $FF (init value)
                    
send_completed:
                    ;shut off transmisTWIOn IRQs
                    sei
                    lda pokmsk
                    and #$e7
                    sta pokmsk
                    sta irqen
                    cli

                    ;we're done
                    tya
                    rts
;-------------------------------------------------------------------------------
; TWIO serial output ready routine
; BUFRLO/BUFRHI: On entry, points to one LESS than the next byte to write.
; BFENLO/BFENHI: Points to byte immediately after buffer.
; CHKSUM: Holds running checksum as bytes are output.
; CHKSNT: $00 if checksum not yet sent, $FF if checksum sent.
; POKMSK: Used to enable the serial output complete IRQ after sending checksum
;-------------------------------------------------------------------------------
TWIO_OutputReadyHandler
                    ;increment buffer pointer
                    inc bufrlo
                    bne addrcc
                    inc bufrhi
addrcc:
                    ;compare against buffer end
                    lda bufrlo
                    cmp bfenlo
                    lda bufrhi
                    sbc bfenhi                                                            ;set flags according to (dst - end)
                    bcs doChecksum

                    ;save Y
                    tya
                    pha

                    ;send out next byte
                    ldy #0
                    lda (bufrlo),y
                    sta serout
                    
                    ;update checksum
                    adc chksum
                    adc #0
                    sta chksum

                    ;restore registers and exit
                    pla
                    tay
                    pla
                    rti
                    
doChecksum:
                    ;send checksum
;                    lda chksum
;                    sta serout
                    
                    ;set checksum sent flag
                    mva #$ff chksnt
                    
                    ;enable output complete IRQ and disable serial output IRQ
                    lda pokmsk
                    ora #$08
                    and #$ef
                    sta pokmsk
                    sta irqen
                    
                    pla
                    rti


;==============================================================================
TWIO_OutputCompleteHandler
                    ;check that we've sent the checksum
                    lda chksnt
                    beq TWIO_OCH_Exit
                    
                    ;we're done sending the checksum
                    sta xmtdon
                    
                    ;need to shut off this interrupt as it is not latched
                    lda pokmsk
                    and #$f7
                    sta pokmsk
                    sta irqen

TWIO_OCH_Exit
                    pla
                    rti
;==============================================================================
TWIO_Countdown1Handler
                    ;signal operation timeout
                    mva #0 timflg
                    rts
;==============================================================================
TWIO_Cassette
                    jsr TWIO_CassetteWriteFrame
                    jmp TWIO_Exit
;==============================================================================
TWIO_CassetteWriteFrame
                    ;wait for pre-record write tone or IRG read delay
                    jsr TWIO_SendEnableNoIRQ
                    ldx #2
                    jsr TWIO_CassetteWait

                    ;set up to transmit
                    jsr TWIO_SendEnable
                    
                    ;setup buffer pointers
                    jsr TWIO_SetupBufferPointers
                    
                    ;send data frame
                    jsr TWIO_Send
                    
                    ;all done
                    jmp TWIO_Exit

;-------------------------------------------------------------------------------
; Wait to generate IRG
;-------------------------------------------------------------------------------
TWIO_CassetteWait
                    jsr TWIO_SetTimeoutVector
                    ldy DDEVIC                  ;Delay VBLs LO
                    lda DUNIT                   ;Delay VBLs HI
                    tax
                    lda #1
                    sta timflg
                    jsr SETVBV
                    lda:rne timflg
                    rts
;-------------------------------------------------------------------------------
; Terminate pokey
;-------------------------------------------------------------------------------
TWIO_TermPokey      lda #0
                    ldx #9
@                   sta AUDF1-1,X
                    dex
                    bne @-
                    rts 
;=======================================================================
; DISPLAY list
;=======================================================================
               ICL "../commons/dlist.asm"
;===============================================================================
; DATA AREAS
;===============================================================================
;===============================================================================
; Block data table
;===============================================================================
            DATA_TABLE=*
            SFX_CAPACITY = 49152-DATA_TABLE-9-9-1
            START = START_ADDR     
