; 
            icl 'rambit.inc'
;
; Start of code
;
            
;
LB12D       lda #$09
            ldx #$FF
            ldy #$B2
            jsr LB5A0
LB136       jsr LB6C8
            cmp #$59
            beq LB14C
            cmp #$4E
            bne LB136
            lda #$30
            sta LB783+1
            lda #$87
            ldy #$07
            bne LB155
LB14C       lda #$24
            sta LB783+1
            lda #$07
            ldy #$87
LB155       sta LB775+1
            sty LB792+1
            jsr LB6C8
            ldy #$00
            rts
LB161       lsr L0098
            ror L00C4
            ror L00C3
            lda L00C3
            bne LB171
            lda L00C4
            beq LB182
            lda L00C3
LB171       clc
            adc L00C0
            adc L00C1
            sta L0094
            lda L00C4
            adc L00C2
            jsr LB30D
            sta L0095
            rts
LB182       jmp LB7D1
            nop
            jmp LB51D
            jsr LB31E
            sty L00C1
            sta L00C2
            lda #$00
            sta L00C3
            lda #$B1
            sta L00C4
            lda L009F
            cmp #$03
            beq LB1C1
            cmp #$07
            beq LB1B5
            cmp #$0C
            beq LB1A9
            jmp LB258
LB1A9       ldx #$05
            jsr LB32B
            jsr LB30D
            sty L00C3
            sta L00C4
LB1B5       ldx #$00
            jsr LB32B
            jsr LB30D
            sty L00C1
            sta L00C2
LB1C1       lda #$00
            tay
            sta (L00C1),Y
            inc L00C1
            bne LB1CC
            inc L00C2
LB1CC       lda L00C1
            cmp L00C3
            bne LB1C1
            lda L00C2
            cmp L00C4
            bne LB1C1
            jmp LB20C
            nop
            nop
            nop
            nop
            nop
LB1E0       nop
            jsr L5245
            .byte $52,$4F,$52
            jsr L4C7F
            .byte $4F
            eor (FMSZPG+1,X)
            eor #$4E
            .byte $47,$7F
            lsr FMSZPG+2,X
            .byte $52
            eor #$46
            eor L4E49,Y
            .byte $47
            nop
            nop
            nop
LB1FD       .byte $43,$44
            lsr FMSZPG+4
            jmp L534D
            lsr SAVMSC,X
            .byte $5A
            nop
            nop
            jsr LB5B3
LB20C       lda #$00
            sta L009F
            lda #$9B
            jsr LB5B8
            jsr LB3F1
LB218       jsr LB6C8
            nop
            nop
            nop
            cmp #$20
            beq LB218
            cmp #$9B
            beq LB232
            jsr LB578
            ldx COLCRS
            stx L009F
            sta L009D,X
            clc
            bcc LB218
LB232       lda #$1F
            cmp L009F
            bcc LB246
            ldy #$00
            lda L00A0
LB23C       cmp LB1FD,Y
            beq LB24C
            iny
            cpy #$0A
            bcc LB23C
LB246       jsr LB3E6
            clc
            bcc LB20C
LB24C       tya
            asl
            tay
            lda LB25E,Y
            pha
            lda LB25D,Y
            pha
            rts
LB258       ldx #$FF
            txs
            bne LB246
LB25D       .byte $A7
LB25E       .byte $B3
            sty L00B2
            .byte $FC,$B3,$EF
            lda DOSVEC,X
            .byte $B7
            sbc #$B5
            txs
            .byte $B7
            sbc LFFB3,Y
            ldy L0088,X
LB270       lda (L0085),Y
            cpy L0084
            .byte $C3
            cmp L00C2
            beq LB27C
            bcc LB280
LB27B       rts
LB27C       cpy L00C1
            bcs LB27B
LB280       pla
            pla
            jmp LB246
            lda L009F
            cmp #$0C
            beq LB291
            jsr LB30A
            clc
            bcc LB299
LB291       ldx #$05
            jsr LB37D
            jsr LB329
LB299       sty L00C1
            sta L00C2
LB29D       lda #$20
            sta L00E0
            lda L00C2
            jsr LB5C3
            sty L00E1
            sta L00E2
            lda L00C1
            jsr LB5C3
            sty L00E3
            sta L00E4
            ldy #$00
            ldx #$05
LB2B7       lda #$20
            sta L00E0,X
            inx
            tya
            pha
            lda (L00C1),Y
            jsr LB5C3
            sty L00E0,X
            inx
            sta L00E0,X
            inx
            pla
            tay
            iny
            cpy #$08
            bne LB2B7
            lda #$1D
            ldx #$E0
            ldy #$00
            jsr LB5A0
            lda L009F
            cmp #$08
            bcc LB2FC
            clc
            lda L00C1
            adc #$08
            sta L00C1
            tay
            bcc LB2F1
            lda L00C2
            cmp L00C4
            beq LB2F1
            inc L00C2
LB2F1       tya
            cmp L00C3
            bcc LB29D
            lda L00C2
            cmp L00C4
            bcc LB29D
LB2FC       jmp LB20C
            cli
            .byte $43
            and (CHKSUM),Y
            jsr L2FD9
            dec LEA3F
            nop
LB30A       jsr LB318
LB30D       cmp #$B1
            bcc LB317
            ldy #$93
            clc
            jmp LB4C2
LB317       rts
LB318       lda L009F
            cmp #$03
            bne LB325
LB31E       lda MEMLO+1
            ldy MEMLO
            rts
LB325       cmp #$07
            bne LB374
LB329       ldx #$00
LB32B       jsr LB33B
            bcc LB331
            pha
LB331       jsr LB33B
            ldy #$00
            bcc LB339
            tay
LB339       pla
            rts
LB33B       lda #$00
            sta L0097
            jsr LB36B
            cmp #$20
            bne LB34F
            jsr LB36B
            cmp #$20
            bne LB35B
            clc
            rts
LB34F       jsr LB362
            asl
            asl
            asl
            asl
            sta L009A
            jsr LB36B
LB35B       jsr LB362
            ora L009A
            sec
            rts
LB362       cmp #$3A
            bcc LB368
            adc #$08
LB368       and #$0F
            rts
LB36B       lda L00A1,X
            jsr LB588
            cmp #$47
            bcc LB377
LB374       jmp LB258
LB377       inx
            rts
            nop
            nop
            nop
            nop
LB37D       jsr LB32B
            sty L00C3
            jsr LB30D
            sta L00C4
            lda L00C3
            pha
            and #$0F
            cmp #$09
            bcc LB39F
            pla
            and #$F0
            clc
            adc #$10
            sta L00C3
            lda L00C4
            adc #$00
            sta L00C4
            rts
LB39F       pla
            and #$F0
            clc
            adc #$08
            sta L00C3
            rts
            lda L009F
            cmp #$0A
            bcc LB3DF
            lda #$3C
            cmp L00A5
            bne LB3DF
            jsr LB329
            sta L00C2
            sty L00C1
            dec L009F
            dec L009F
            ldy #$FF
            ldx #$05
LB3C3       iny
            cpy #$08
            beq LB3DC
LB3C8       lda L00A1,X
            inx
            cpx L009F
            beq LB3DC
            cmp #$2C
            beq LB3C3
            dex
            jsr LB33B
            sta (L00C1),Y
            clc
            bcc LB3C8
LB3DC       jmp LB20C
LB3DF       jmp LB258
            nop
            nop
            nop
            nop
LB3E6       lda #$9B
            jsr LB5B8
            lda #$03
            ldx #$F3
            bne LB3F5
LB3F1       lda #$0B
            ldx #$E8
LB3F5       ldy #$BB
            jmp LB5A0
            clc
            bcc LB3FE
            sec
LB3FE       php
            jsr LB30A
            sty L00C1
            sty L0092
            sta L00C2
            sta L0093
            plp
            lda #$34
            sta PACTL
            ldx #$04
            bcs LB42A
LB414       lda LB4C5+2,X
            sta LB484+1,X
            dex
            bne LB414
            lda #$0A
            ldx #$F0
            ldy #$B1
            jsr LB5A0
            ldx #$00
            beq LB43E
LB42A       lda LB4CA+1,X
            sta LB484+1,X
            dex
            bne LB42A
            lda #$08
            ldx #$E8
            ldy #$B1
            jsr LB5A0
            ldx #$01
LB43E       stx L00C0
            nop
            ldy #$02
            sty L00FF
            ldy #$FF
            sty L0095
LB449       dex
            bne LB449
            dey
            bne LB449
            sty L00C3
            sty L0098
            sty L0099
            sei
            sty NMIEN
            sty IRQEN
            lda #$D0
            sta VIMIRQ
            lda #$B4
            sta VIMIRQ+1
            lda #$35
            sta PBCTL
            lda PORTB
            ldx #$12
            stx AUDF1
            lda #$5A
            cli
LB476       cmp LOMEM
            bne LB476
            ldx #$08
LB47C       cpx #$00
            bne LB47C
            lda LOMEM
            ldx #$08
LB484       sta L0082
            eor (L0092),Y
            beq LB48C
            sta LOMEM+1
LB48C       lda L00FF
            beq LB4A4
            dec L00FF
            lda L00FF
            beq LB49D
            lda L0082
            sta L0098
            clc
            bcc LB4A4
LB49D       lda L0082
            sta L00C4
            jsr LB6D1
LB4A4       inc L0092
            bne LB4AA
            inc L0093
LB4AA       lda L0092
            cmp L0094
            bne LB47C
            lda L0093
            cmp L0095
            bne LB47C
LB4B6       cpx #$00
            bne LB4B6
            sei
            cpx LOMEM+1
            beq LB4E0
            sec
            ldy #$8F
LB4C2       jsr LB531
LB4C5       jmp LB6A4
            eor (L0092),Y
LB4CA       beq LB4CE
            sta (L0092),Y
LB4CE       eor LOMEM+1
            pha
            sty LOMEM+1
            sta STIMER
            lda #$B9
            sta VIMIRQ+1
            lda #$26
            jmp LB91C
LB4E0       jsr LB51D
LB4E3       lda #$04
            ldx #$EF
            ldy #$B4
            jsr LB5A0
            jmp LB20C
            jsr L4B4F
            and (ICHIDZ,X)
            lda TRAMSZ
            eor CMCMD
            beq LB567+1
            lda #$00
            sta BOOT
            beq LB567
            lda L9FFD
            and #$04
            beq LB50D
            jsr LB51D
            jmp (L9FFA)
LB50D       lda LBFFD
            and #$04
            beq LB51A
            jsr LB51D
            jmp (LBFFA)
LB51A       jmp LB246
LB51D       clc
LB51E       bcc LB531
            lda VIMIRQ
            sta LB537+1
            lda VIMIRQ+1
            sta LB53C+1
            lda #$11
            sta LB51E+1
;-------------------------------------------------------------------------------
; Terminate writing - restore IRQs etc.
;-------------------------------------------------------------------------------            
LB531       sei
            lda #$B1
            sta MEMTOP+1
LB537       lda #$30
            sta VIMIRQ
LB53C       lda #$C0
            sta VIMIRQ+1
            jsr INTINV
            lda #$3C
            sta PACTL
            sta PBCTL
            lda #$00
            sta IRQEN
            ldx #$80
LB553       sta L0000,X
            inx
            bne LB553
            lda POKMSK
            sta IRQEN
            lda #$09
            sta DOSVEC
            lda #$B2
            sta DOSVEC+1
            lda #$86
LB567       sta CASINI
            lda #$B1
            sta CASINI+1
            sta DOSINI+1
            cli
            rts
;-------------------------------------------------------------------------------            
            pla
            pla
            pla
            cli
            jmp LB20C
LB578       cmp #$80
            beq LB59F
            cmp #$2C
            beq LB59F
            cmp #$3C
            beq LB59F
            cmp #$1C
            bcc LB59C
LB588       cmp #$20
            bcc LB59F
            cmp #$30
            bcc LB59C
            cmp #$3A
            bcc LB59F
            cmp #$41
            bcc LB59C
            cmp #$5B
            bcc LB59F
LB59C       jmp LB258
LB59F       rts
LB5A0       stx IOCB0+ICBAL
            sty IOCB0+ICBAH
            sta IOCB0+ICBLL
            lda #$09
            ldx #$00
            sta IOCB0+ICCOM
            jmp CIOV
LB5B3       jsr LB531
            lda #$7D
LB5B8       tax
            lda LE407
            pha
            lda LE406
            pha
            txa
            rts
LB5C3       pha
            lsr
            lsr
            lsr
            lsr
            jsr LB5D3
            tay
            pla
            and #$0F
            jsr LB5D3
            rts
LB5D3       ora #$30
            cmp #$3A
            bcc LB5DB
            adc #$06
LB5DB       rts
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            jsr LB600
            jmp LB20C
            jsr LB318
            pha
            tya
            sec
            sbc #$01
            tay
            pla
            sbc #$00
            pha
            tya
            pha
            rts
LB600       ldy #$00
            lda #$11
            cmp L009F
            bne LB65C
            lda #$3C
            cmp L00A5
            bne LB65C
            ldx #$00
            jsr LB32B
            sta L00C6
            sty L00C5
            ldx #$05
            jsr LB32B
            sta L00C2
            sty L00C1
            ldx #$0A
            jsr LB32B
            jsr LB270+1
LB628       ldy #$00
            lda L00C2
            cmp L00C6
            bcc LB65F
            bne LB638
            lda L00C1
            cmp L00C5
            bcc LB65F
LB638       lda (L00C1),Y
            sta (L00C5),Y
            inc L00C5
            bne LB647
            inc L00C6
            lda L00C6
            jsr LB30D
LB647       inc L00C1
            bne LB64D
            inc L00C2
LB64D       lda L00C2
            cmp L00C4
            bne LB638
            lda L00C1
            cmp L00C3
            bne LB638
LB659       rts
            nop
            nop
LB65C       jmp LB246
LB65F       lda L00C3
            sec
            sbc L00C1
            sta L00C7
            lda L00C4
            sbc L00C2
            sta L00C8
            clc
            lda L00C7
            adc L00C5
            sta L00C7
            lda L00C8
            adc L00C6
            sta L00C8
            jsr LB30D
LB67C       dec L00C7
            lda L00C7
            cmp #$FF
            bne LB686
            dec L00C8
LB686       dec L00C3
            lda L00C3
            cmp #$FF
            bne LB690
            dec L00C4
LB690       lda (L00C3),Y
            sta (L00C7),Y
            lda L00C4
            cmp L00C2
            bne LB67C
            lda L00C3
            cmp L00C1
            bne LB67C
            beq LB659
            ldy #$FF
LB6A4       lda #$20
            sta L00E9
            tya
            jsr LB5C3
            sty L00E7
            sta L00E8
            ldx #$07
LB6B2       lda LB1E0,X
            sta L00DF,X
            dex
            bne LB6B2
            dex
            txs
            lda #$0A
            ldx #$E0
            ldy #$00
            jsr LB5A0
            jmp LB20C
LB6C8       lda LE405
            pha
            lda LE404
            pha
            rts
LB6D1       lda L0098
            cmp #$02
            bcc LB6DD
            cmp #$FE
            bcs LB6DD
            sty L0098
LB6DD       jmp LB161
            lsr LEAEA,X
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            jmp LB6A4
LB6F4       ldx #$10
            lda #$03
            sta IOCB0+ICCOM,X
            sta IOCB0+ICBAH,X
            lda #$80
            sta IOCB0+ICAX2,X
            lda #$1D
            sta IOCB0+ICBAL,X
            jmp CIOV
            jsr LB30A
            sty L0094
            sta L0095
            ldy #$00
            sty L0099
            sty L0098
            lda #$04
            sta IOCB1+ICAX1
            jsr LB6F4
            bmi LB759
LB722       lda #$07
            sta IOCB0+ICCOM,X
            lda L0094
            sta IOCB0+ICBAL,X
            lda L0095
            sta IOCB0+ICBAH,X
            lda #$80
            sta IOCB0+ICBLL,X
            lda #$00
            sta IOCB0+ICBLH,X
            jsr CIOV
            inc L0099
            bne LB744
            inc L0098
LB744       cpy #$01
            bne LB759
            clc
            lda L0094
            adc #$80
            sta L0094
            lda L0095
            adc #$00
            sta L0095
            cmp #$B1
            bcc LB722
            
LB759       tya
            pha
            lda #$0C
            sta IOCB0+ICCOM,X
            jsr CIOV
            pla
            cmp #$88
            beq LB76C
            tay
            jmp LB6A4
LB76C       jmp LB4E3
;-------------------------------------------------------------------------------
; Interrupt handler - writing signal
;-------------------------------------------------------------------------------
            pha
            tya
            pha
            sta STIMER
LB775       lda #$87
            sta SKCTL
            lda #$00
            sta IRQEN
            asl LOMEM
            lda #$01
LB783       ldy #$30
            bcs LB78B
            lda #$02
            ldy #$16
LB78B       dey
            bne LB78B
            dex
            sta IRQEN
LB792       lda #$07
            sta SKCTL
            pla
            tay
            pla
            rti
            
            
            lda #$B9
            sta L0096
            lda #$07
            sta AUDF2
            lda #$05
            sta AUDF1
            jsr LB30A
            sty L00C1
            sty L0092
            sta L00C2
            sta L0093
            jsr LB12D
            sty L0097
            sty L00C0
            sty L00C4
            sty L0091
            lda #$FF
            cmp (L0092),Y
            bne LB7E5
            iny
            cmp (L0092),Y
            bne LB7F0
            lda L0099
            clc
            adc L0098
            bne LB7D6
LB7D1       ldy #$FF
            jmp LB4C2
LB7D6       sty L0091
            lda L0099
            sta (L0092),Y
            dey
            lda L0098
            eor #$FE
            sta (L0092),Y
            bne LB7ED
LB7E5       lda (L0092),Y
            cmp #$FE
            bcc LB800
            sty L0098
LB7ED       iny
            bne LB7F2
LB7F0       sty L0098
LB7F2       lda (L0092),Y
            sta L00C4
            lda #$BA
            sta L0096
            lda #$7F
            sta L0097
            bne LB80B
LB800       cmp #$02
            bcs LB806
            sta L0098
LB806       iny
            lda (L0092),Y
            sta L00C4
LB80B       jsr LB161
            lda L0091
            beq LB817
            jsr LBB76
            dec L0091
LB817       iny
            lda (L0092),Y
            sta LB910
            clc
            adc #$06
            sta LB906+1
            iny
            lda (L0092),Y
            sta LB911
            adc #$00
            sta LB90A+1
            iny
            lda (L0092),Y
            sta LB904
            iny
            lda (L0092),Y
            sta LB905
            clc
            lda LB910
            adc #$02
            adc L00C3
            sta LB912
            lda LB911
            adc L00C4
            sta LB913
            lda #$08
            sta IOCB1+ICAX1
            jsr LB6F4
            bmi LB87D
            lda #$0B
            sta IOCB0+ICCOM,X
            lda #$00
            sta IOCB0+ICBAL,X
            lda L0096
            sta IOCB0+ICBAH,X
            lda L0097
            sta IOCB0+ICBLL,X
            lda #$01
            sta IOCB0+ICBLH,X
            jsr CIOV
            bmi LB87D
            lda #$0C
            sta IOCB0+ICCOM,X
            jsr CIOV
LB87D       bmi LB8FD

;-------------------------------------------------------------------------------
;Prepare for writing
; - Disable interrupts
; - Set serial port for writing
; - Switch on the motor
; - Set interrupt handler for writing signal
; - Set pokey frequencies
;-------------------------------------------------------------------------------
            sei
            ldx #$00
            stx NMIEN
            lda #$87
            sta SKCTL
            lda #$34
            sta PACTL
            lda #$6F
            sta VIMIRQ
            lda #$B7
            sta VIMIRQ+1
            ldy #$40
            stx DMACLT
            stx IRQEN
            inx
            stx IRQEN
            ldx #$17
            lda #$0B
            stx AUDF1
            sta AUDF2
            cli
;-------------------------------------------------------------------------------
; Write pilot tone ($FFs)
;-------------------------------------------------------------------------------            
LB8B0       dey
            beq LB8C3
            lda #$FF
LB8B5       cpx #$00
            bne LB8B5
            sta LOMEM
            ldx #$08
            cpy #$00
            beq LB8C7
            bne LB8B0
            
;-------------------------------------------------------------------------------
; Write synchronization sequence
;-------------------------------------------------------------------------------            
LB8C3       lda #$5A
            bne LB8B5
            
;-------------------------------------------------------------------------------
; Write data block (buffer pointer at $0092, buffer end at $0094)
;-------------------------------------------------------------------------------            
LB8C7       lda (L0092),Y
LB8C9       cpx #$00
            bne LB8C9
            sei
            sta LOMEM
            ldx #$08
            cli
            eor LOMEM+1
            sta LOMEM+1
            inc L0092
            bne LB8DD
            inc L0093
LB8DD       lda L0092
            cmp L0094
            bne LB8C7
            lda L0093
            cmp L0095
            bne LB8C7
;-------------------------------------------------------------------------------
; Write checksum
;-------------------------------------------------------------------------------            
            lda LOMEM+1
LB8EB       cpx #$00
            bne LB8EB
            sta LOMEM
            ldx #$FF
LB8F3       cpx #$00
            bne LB8F3
;-------------------------------------------------------------------------------            
            jsr LB531
            jmp LB4E3
LB8FD       jmp LB759
            ora (CASINI,X)
            .byte $80,$00
LB904       .byte $33
LB905       clc
LB906       ldx #$06
            stx RAMLO
LB90A       lda #$18
            sta RAMLO+1
            bne LB947
LB910       .byte $00
LB911       clc
LB912       .byte $02
LB913       asl L8448,X
            sta (L008D,X)
            ora #$D2
            lda #$A6
LB91C       sta VIMIRQ
            lda PORTB
            lda #$01
            bne LB93D
            
;-------------------------------------------------------------------------------
; Interrupt handler - reading signal            
;--------------------------------------------------------------------------------
            pha
            lda #$01
            bit IRQST
            beq LB93D
            sta STIMER
            cpy L008B
            rol LOMEM
            sta L008B
            dex
            lda PORTB
            pla
            rti
LB93D       sty IRQEN
            sty L008B
            sta IRQEN
            pla
            rti
            
            
;-------------------------------------------------------------------------------
; Setup interrupt handler to $0094
;-------------------------------------------------------------------------------            
LB947       sei
            ldy #$00
            ldx #$01
            lda VIMIRQ
            pha
            lda VIMIRQ+1
            pha
            sty DMACLT
            lda #$63
            sta DLISTL
            stx DLISTH
            lda #$22
            sta DMACLT
            lda #$94
            sta VIMIRQ
            sty IRQEN
            sty NMIEN
            sty VIMIRQ+1
            lda #$35
            sta PBCTL
            lda PORTB
            ldx #$12
            stx AUDF1
            cli
            
;-------------------------------------------------------------------------------
; Wait for $5A
;-------------------------------------------------------------------------------            
            lda #$5A
LB982       cmp LOMEM
            bne LB982
            
;-------------------------------------------------------------------------------
; Read data to buffer pointed to by $90,$91
;-------------------------------------------------------------------------------            
            ldx #$08
LB988       cpx #$00
            bne LB988
            lda LOMEM
            ldx #$08
            sta (L0090),Y
            eor LOMEM+1
            sta LOMEM+1
            inc L0090
            bne LB99C
            inc L0091
LB99C       lda L0091
            sta L016D
            cmp L0093
            bne LB988
            lda L0090
            cmp L0092
            bne LB988
LB9AB       cpx #$00
            bne LB9AB
;-------------------------------------------------------------------------------
; Restore interrupt handler
;-------------------------------------------------------------------------------            
            sei
            pla
            sta VIMIRQ+1
            pla
            sta VIMIRQ
            sty AUDF1
            sty IRQEN
            lda PORTB
            lda POKMSK
            sta IRQEN
            lda #$40
            sta NMIEN
            lda #$3C
            sta PACTL
            sta PBCTL
            cli
            lda LOMEM+1
            bne LB9D9
            clc
LB9D9       ldx #$26
LB9DB       sta.w OLDROW,X
            inx
            bne LB9DB
            bcc LB9E4
            rts
;-------------------------------------------------------------------------------
; Run the program
;-------------------------------------------------------------------------------          
LB9E4       jmp (RAMLO)
            .byte $47
            adc L7001,Y
            .byte $47,$00,$04,$07,$07,$07,$07,$07,$07,$07,$07
            eor (MLTTMP+1,X)
            ora (ICAX3Z,X)
            .byte $2F
            and (ICBALZ,X)
            and #$2E
            .byte $27,$00,$03,$00,$07
            asl CMCMD
            
;-------------------------------------------------------------------------------
; Set interrupt handler to $0816
;-------------------------------------------------------------------------------            
            sei
            ldy #$00
            sty NMIEN
            sty INITAD
            sty INITAD+1
            lda #$34
            sta PACTL
            lda VIMIRQ
            pha
            lda VIMIRQ+1
            pha
            sty DMACLT
            lda #$51
            sta DLISTL
            lda #$08
            sta DLISTH
            sta VIMIRQ+1
            lda #$21
            sta DMACLT
            lda #$16
            sta VIMIRQ
            sty IRQEN
            lda #$35
            sta PBCTL
            lda PORTB
            ldx #$12
            stx AUDF1
            ldy #$04
            lda #$5A
            cli
;-------------------------------------------------------------------------------
; Read block - segment of a binary file
;-------------------------------------------------------------------------------            
LBA4E       cmp FTYPE
            bne LBA4E
            ldx #$18
LBA54       cpx #$00
            bne LBA54
            lda FTYPE
            ldx #$08
            cpy #$00
            bne LBA90
            sta (BUFRFL),Y
            lda RECVDN
            sta L0857
            cmp CHKSNT
            bcc LBA73
            bne LBA7B
            lda XMTDON
            cmp BUFRFL
            beq LBA7B
LBA73       inc BUFRFL
            bne LBA54
            inc RECVDN
            bne LBA54
LBA7B       lda #$38
            sta L0799
            lda INITAD+1
            bne LBA88
            lda INITAD
LBA88       sta NOCKSM
            bne LBAAA
            ldy #$04
            bne LBA54
LBA90       cpy #$03
            bne LBA98
            cmp #$FF
            beq LBA7B
LBA98       sta BUFRFL
            inc L0799
            dey
            bne LBA54
            cpy CHKSNT
            bne LBA54
            cpy XMTDON
            bne LBA54
            sty NOCKSM
;-------------------------------------------------------------------------------
;Restore original interrupt handler
;-------------------------------------------------------------------------------
LBAAA       sei
            jsr SIOINV
            pla
            sta VIMIRQ+1
            pla
            sta VIMIRQ
            sty COLDST
            sty AUDF1
            ldx POKMSK
            stx IRQEN
            ldx #$40
            stx NMIEN
            ldx NOCKSM
            beq LBAD0
            jsr L07E8
            jmp L0706
LBAD0       cli
;-------------------------------------------------------------------------------
; Run the loaded program
;-------------------------------------------------------------------------------
            lda RUNAD+1
            bne LBADB
            lda RUNAD
            beq LBAEB
LBADB       nop
            nop
            nop
            nop
            jsr L07E5
LBAE2       jmp WARMSV
            jmp (RUNAD)
            jmp (INITAD)
            
;-------------------------------------------------------------------------------
; Relocate data BF80 to page zero
;-------------------------------------------------------------------------------            
LBAEB       ldx #$10
LBAED       dex
            lda LBF80,X
            sta L0000,X
            cpx #$00
            bne LBAED
            dex
            cpx WARMST
            beq LBAE2
            lda BOOT
            cmp #$01
            bcc LBB0D
            beq LBB0A
            jsr L0813
            clc
            bcc LBB0D
LBB0A       jsr L0810
LBB0D       jmp (DOSVEC)
            jmp (DOSINI)
            jmp (CASINI)
            
;-------------------------------------------------------------------------------
;Interrupt handler - god knows what?
;-------------------------------------------------------------------------------            
            pha
            tya
            pha
            sta STIMER
            lda #$2A
            sta VIMIRQ
            lda PORTB
            lda #$01
            ldy #$00
            beq LBB47
            pha
            tya
            pha
            ldy #$00
            lda #$01
            bit IRQST
            beq LBB47
            sta STIMER
            cpy BPTR
            rol FTYPE
            sta BPTR
            dex
            lda PORTB
LBB43       pla
            tay
            pla
            rti
            
LBB47       sty IRQEN
            sty BPTR
            sta IRQEN
            beq LBB43
;-------------------------------------------------------------------------------            
            .byte $47,$63
            php
            bvs LBB9D
            .byte $00,$02,$07,$07,$07,$07,$07,$07,$07,$07
            eor (HOLD1,X)
            php
            .byte $00,$00,$00,$00
            bit L21AF
            ldy ICBLHZ
            ldx.w ICPTHZ
            .byte $00,$00,$00,$00,$00,$00,$00
LBB76       ldy #$00
            inc L00C1
            bne LBB7E
            inc L00C2
LBB7E       lda L00C1
            cmp L0094
            bne LBB8A
            lda L00C2
            cmp L0095
            beq LBBE7
LBB8A       ldx #$08
            lda (L00C1),Y
            cmp #$E0
            beq LBB98
            ldx #$06
            cmp #$E2
            bne LBB76
LBB98       iny
            lda (L00C1),Y
            cmp #$02
LBB9D       bne LBB76
            iny
            lda (L00C1),Y
            cmp #$E3
            bne LBB76
            clc
            txa
            adc L00C1
            sta L00C1
            sta L00C5
            sta L00C9
            lda L00C2
            adc #$00
            sta L00C2
            sta L00CA
            sta L00C6
            inc L00C6
            lda L0094
            sta L00C3
            lda L0095
            sta L00C4
            inc L0095
            jsr LB628
LBBC9       lda #$FF
            cpy #$FD
            bne LBBD1
            lda #$5A
LBBD1       sta (L00C9),Y
            dey
            bne LBBC9
            iny
            lda (L0092),Y
            clc
            adc #$02
            sta (L0092),Y
            dey
            lda (L0092),Y
            adc #$00
            sta (L0092),Y
            ldy #$01
LBBE7       rts
;
         
