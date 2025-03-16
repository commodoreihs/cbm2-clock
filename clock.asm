*=$2400

; bank 15 labels
CLKHI     = #$DC
CLKTHSLO  = #$08
CLKSECLO  = #$09
CLKMINLO  = #$0A
CLKHRLO   = #$0B
STXPOS0LO = #$20
STXPOS0HI = #$D3
STXPOS1LO = #$70
STXPOS1HI = #$D3
STXPOS2LO = #$C0
STXPOS2HI = #$D3
STXPOS3LO = #$10
STXPOS3HI = #$D4
STXPOS4LO = #$60
STXPOS4HI = #$D4
IND       = $1D
TSR       = $0700
INT       = $DE07
SCNKEY    = $040E ; check this address if re-assembling init.asm
CHROUT    = $0432 ; check this address if re-assembling init.asm

; bank 1 labels
BLCNT   = $06FF
SONESEC = $07F0
STENSEC = $07F1
MSEC    = $07F2
SONEMIN = $07F3
STENMIN = $07F4
SONEHR  = $07F5
STENHR  = $07F6
XOFFSET = $07F7
TMPX    = $07F8
IRQV    = $FFFE  ; kernal IRQ vector
EDFLG   = $06FE  ; EDFLG is the "are we editing" flag
                 ; Bit 0 = seconds are being edited.
                 ; Bit 1 = minutes are being edited.
                 ; Bit 2 = hours are being edited.
                 ; Bit 3 = blink enabled (turn off)

; zero page locations for writing to bank 15
W15LO   = $E6      ; Low byte of address to write to in bank 15
W15HI   = $E7      ; High byte of address to write to in bank 15

; zero page locations for reading from bank 15
R15LO   = $E8      ; Low byte of address to read from in bank 15
R15HI   = $E9      ; High byte of address to read from in bank 15

START  JSR CLRSCR
       LDA #<INT     ; set up indirect indexed for updating the
       STA IND       ; 6252 interrupt register
       LDA #>INT
       STA IND+1
       JSR SETIRQ
       LDY #$00
       STY EDFLG   ; clear the edit flag
PRL    LDA INS,Y   ; print the instructions
       BEQ ENDPR
       JSR CHROUT
       INY
       JMP PRL
ENDPR  JSR BANK15
SETCLK LDY #$00
       LDA #$00    ; initialize seconds, ms and minutes to 0
       LDX CLKTHSLO ; do MS first
       STX W15LO
       LDX CLKHI
       STX W15HI
       STA (W15LO),Y
       LDA #$01      ; initialize hour to 1
       LDX CLKHRLO 
       STX W15LO
       LDX CLKHI
       STX W15HI
       STA (W15LO),Y
       LDA #$00      ; initialize minutes to 0
       LDX CLKMINLO
       STX W15LO
       LDX CLKHI
       STX W15HI
       STA (W15LO),Y
       LDA #$00      ; initialize seconds to 0
       LDX CLKSECLO
       STX W15LO
       LDX CLKHI
       STX W15HI
       STA (W15LO),Y
       JSR BANK1
SETHR  LDA EDFLG
       AND #%00001000
       ORA #%00000100
       STA EDFLG
       JSR LDTOD
       JSR DISPLAY
       JSR SCNKEY
       CMP #$91   ; cursor up
       BEQ INCHR
       CMP #$11   ; cursor down
       BEQ DECHR
       CMP #$1D   ; cursor right
       BEQ SETMN
       CMP #$20
       BEQ RUNCLK
       JMP SETHR
SETMN  LDA EDFLG
       AND #%00001000
       ORA #%00000010 
       STA EDFLG
       JSR LDTOD
       JSR DISPLAY
       JSR SCNKEY
       CMP #$91   ; cursor up
       BEQ INCMN
       CMP #$11   ; cursor down
       BEQ DECMN
       CMP #$9D   ; cursor left
       BEQ SETHR
       CMP #$1D   ; cursor right
       BEQ SETSE
       CMP #$20
       BEQ RUNCLK
       JMP SETMN
SETSE  LDA EDFLG
       AND #%00001000
       ORA #%00000001
       STA EDFLG
       JSR LDTOD
       JSR DISPLAY
       JSR SCNKEY
       CMP #$91   ; cursor up
       BEQ INCSE
       CMP #$11   ; cursor down
       BEQ DECSE
       CMP #$9D   ; cursor left
       BEQ SETMN
       CMP #$20
       BEQ RUNCLK
       JMP SETSE
; routine to increment hours
INCHR  JSR BANK15
       SED
       LDX CLKHRLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       AND #$7F
       CMP #$12
       BPL SETHR
       CLC
       ADC #$01
       LDX CLKHRLO 
       STX W15LO
       LDX CLKHI
       STX W15HI
       LDY #$00
       STA (W15LO),Y
       CLD
       JSR BANK1
       JMP SETHR

; routine to read hours. only purpose is to stop the clock
RDHR   JSR BANK15
       SED
       LDX CLKHRLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       LDX CLKHRLO
       STX W15LO
       LDX CLKHI
       STX W15HI
       LDY #$00
       STA (W15LO),Y
       CLD
       JSR BANK1
       JMP SETHR

; routine to decrement hours
DECHR  JSR BANK15
       SED
       LDX CLKHRLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       AND #$7F
       CMP #$01
       BEQ SETHR
       SEC
       SBC #$01
       LDX CLKHRLO
       STX W15LO
       LDX CLKHI
       STX W15HI
       LDY #$00
       STA (W15LO),Y
       CLD
       JSR BANK1
       JMP SETHR

INCMN  JSR BANK15
       SED
       LDX CLKMINLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       CMP #$59
       BPL SETMN
       CLC
       ADC #$01
       LDX CLKMINLO
       STX W15LO
       LDX CLKHI
       STX W15HI
       LDY #$00
       STA (W15LO),Y
       CLD
       JSR BANK1
       JMP SETMN

DECMN  JSR BANK15
       SED
       LDX CLKMINLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       AND #$7F
       CMP #$01
       BEQ SETMN
       SEC
       SBC #$01
       LDX CLKMINLO
       STX W15LO
       LDX CLKHI
       STX W15HI
       LDY #$00
       STA (W15LO),Y
       CLD
       JSR BANK1
       JMP SETMN

INCSE  JSR BANK15
       SED
       LDX CLKSECLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       CMP #$59
       BPL SETSE
       CLC
       ADC #$01
       LDX CLKSECLO
       STX W15LO
       LDX CLKHI
       STX W15HI
       LDY #$00
       STA (W15LO),Y
       CLD
       JSR BANK1 
       JMP SETSE

DECSE  JSR BANK15
       SED
       LDX CLKSECLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       AND #$7F
       CMP #$01
       BEQ SETSE
       SEC
       SBC #$01
       LDX CLKSECLO
       STX W15LO
       LDX CLKHI
       STX W15HI
       LDY #$00
       STA (W15LO),Y
       CLD
       JSR BANK1
       JMP SETSE

RUNCLK LDY #$00
       LDA #$00
       STA EDFLG
       JSR BANK15
       LDX CLKTHSLO
       STX W15LO
       LDX CLKHI
       STX W15HI
       STA (W15LO),Y
       JSR BANK1
LOOP   JSR LDTOD
       JSR DISPLAY
       JSR SCNKEY
       CMP #$20
       BEQ RDHR
       JMP LOOP

LDTOD  JSR BANK15   ; read CIA TOD and store in memory as BCD
       SED 
; start with milliseconds
       LDX CLKTHSLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       AND #$0F 
       STA MSEC
; read and store ones of seconds
       LDX CLKSECLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       AND #$0F 
       STA SONESEC 
; read and store tens of seconds
       LDX CLKSECLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       JSR SHIFTNIBR
       STA STENSEC
; read and store ones of minutes
       LDX CLKMINLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       AND #$0F
       STA SONEMIN
; read and store tens of minutes
       LDX CLKMINLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       JSR SHIFTNIBR
       STA STENMIN
; read and store ones of hours
       LDX CLKHRLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       AND #$0F 
       STA SONEHR
; read and store tens of hours
       LDX CLKHRLO
       STX R15LO
       LDX CLKHI
       STX R15HI
       LDY #$00
       LDA (R15LO),Y
       JSR BANK1
       AND #$7F    ; bit 7 stores am/pm so mask it off
       JSR SHIFTNIBR
       STA STENHR
       RTS 

DISPLAY LDX #$0B        ; default to space (blinked) 
        LDA EDFLG
        CMP #%00001100
        BEQ SK1
        LDX STENHR      ; load tens digit of hour into x register
SK1     LDA CHAROFF,X   ; load the char index of that digit into the accumulator
        LDY #$15        ; this is the x-axis offset 
        STY XOFFSET
        JSR PRINTCH
        LDX #$0B         ; default to blinked...
        LDA EDFLG
        CMP #%00001100
        BEQ SK2
        LDX SONEHR      ; ones digit of the hour
SK2     LDA CHAROFF,X
        LDY #$19
        STY XOFFSET
        JSR PRINTCH
        LDX #$0A        ; colon between hours and minutes
        LDA CHAROFF,X
        LDY #$1D
        STY XOFFSET
        JSR PRINTCH
        LDX #$0B
        LDA EDFLG
        CMP #%00001010
        BEQ SK3
        LDX STENMIN     ; tens digit of minutes
SK3     LDA CHAROFF,X
        LDY #$21
        STY XOFFSET
        JSR PRINTCH
        LDX #$0B
        LDA EDFLG
        CMP #%00001010
        BEQ SK4
        LDX SONEMIN     ; ones digit of minutes
SK4     LDA CHAROFF,X
        LDY #$25
        STY XOFFSET
        JSR PRINTCH
        LDX #$0A        ; colon between hours and minutes
        LDA CHAROFF,X
        LDY #$29
        STY XOFFSET
        JSR PRINTCH
        LDX #$0B
        LDA EDFLG
        CMP #%00001001
        BEQ SK5
        LDX STENSEC     ; tens digit of seconds
SK5     LDA CHAROFF,X
        LDY #$2D
        STY XOFFSET
        JSR PRINTCH
        LDX #$0B
        LDA EDFLG
        CMP #%00001001
        BEQ SK6
        LDX SONESEC     ; ones digit of seconds
SK6     LDA CHAROFF,X
        LDY #$31
        STY XOFFSET
        JSR PRINTCH
        LDX #$0A        ; colon between seconds and ms
        LDA CHAROFF,X
        LDY #$35
        STY XOFFSET
        JSR PRINTCH
        LDX MSEC        ; milliseconds
        LDA CHAROFF,X
        LDY #$39
        STY XOFFSET
        JSR PRINTCH
        RTS

PRINTCH JSR BANK15
        TAX     ;accumulator contains the offset into the char table 
; position 0
        LDY XOFFSET    ;used as the offset from base xpos
        LDA STXPOS0LO
        STA W15LO
        LDA STXPOS0HI
        STA W15HI
        LDA CHARS,X ;x register contains the starting character address
        STA (W15LO),Y
        INX
        INY
        LDA CHARS,X
        STA (W15LO),Y
        INX
        INY
        LDA CHARS,X
        STA (W15LO),Y
        INX
        INY
        LDA CHARS,X
        STA (W15LO),Y ;each char is a 4x5 matrix so done with top row
        INX
; position 1
        LDY XOFFSET
        LDA STXPOS1LO
        STA W15LO
        LDA STXPOS1HI
        STA W15HI
        LDA CHARS,X
        STA (W15LO),Y
        INX
        INY
        LDA CHARS,X
        STA (W15LO),Y
        INX
        INY
        LDA CHARS,X
        STA (W15LO),Y
        INX
        INY
        LDA CHARS,X
        STA (W15LO),Y 
        INX
; position 2
        LDY XOFFSET
        LDA STXPOS2LO
        STA W15LO
        LDA STXPOS2HI
        STA W15HI
        LDA CHARS,X
        STA (W15LO),Y
        INX 
        INY 
        LDA CHARS,X
        STA (W15LO),Y
        INX 
        INY 
        LDA CHARS,X
        STA (W15LO),Y
        INX 
        INY
        LDA CHARS,X
        STA (W15LO),Y 
        INX
; position 3
        LDY XOFFSET
        LDA STXPOS3LO
        STA W15LO
        LDA STXPOS3HI
        STA W15HI
        LDA CHARS,X
        STA (W15LO),Y
        INX 
        INY 
        LDA CHARS,X
        STA (W15LO),Y
        INX 
        INY 
        LDA CHARS,X
        STA (W15LO),Y
        INX 
        INY
        LDA CHARS,X
        STA (W15LO),Y 
        INX
; position 4
        LDY XOFFSET
        LDA STXPOS4LO
        STA W15LO
        LDA STXPOS4HI
        STA W15HI
        LDA CHARS,X
        STA (W15LO),Y
        INX 
        INY 
        LDA CHARS,X
        STA (W15LO),Y
        INX 
        INY 
        LDA CHARS,X
        STA (W15LO),Y
        INX 
        INY
        LDA CHARS,X
        STA (W15LO),Y 
        JSR BANK1
        RTS

BANK15  PHA
        LDA #$0F ; sets 6509 data indirection byte to bank15
        STA $01  ; does not set execution byte
        PLA
        RTS

BANK1   PHA
        LDA #$01 ; sets 6509 data indirection byte to bank1
        STA $01  ; does not set execution byte
        PLA
        RTS

SHIFTNIBR
      LSR A
      LSR A
      LSR A
      LSR A
      RTS

CLRSCR  JSR BANK15
        LDA #$20   ; load petscii space into accumulator
        LDY #$00   ; use Y as the loop iterator
SCLOOP  LDX #$00   ; low byte of our screen memory address
        STX W15LO  ; store in zero page for indirect indexed addressing
        LDX #$D0   ; high byte of our screen memory address
        STX W15HI  ; store in zero page for indirect...
        STA (W15LO),Y
        LDX #$D1
        STX W15HI 
        STA (W15LO),Y
        LDX #$D2
        STX W15HI 
        STA (W15LO),Y
        LDX #$D3
        STX W15HI
        STA (W15LO),Y
        LDX #$D4
        STX W15HI
        STA (W15LO),Y
        LDX #$D5
        STX W15HI
        STA (W15LO),Y
        LDX #$D6
        STX W15HI
        STA (W15LO),Y
        LDX #$D7
        STX W15HI
        STA (W15LO),Y
        DEY
        BNE SCLOOP
        JSR BANK1
        RTS

SVCIRQ PHA
       TYA
       PHA
       TXA
       PHA
       CLD
       LDX $01 ; save i6509 in a register before switching banks
       LDA #$0F
       STA $01
       TXA     ; now we can push it onto the stack in bank 15
       PHA
       LDY #$00
       LDA ($1D),Y
       STA TSR
       BEQ EGRESS
       LDX BLCNT  ; load our blink countdown
       DEX
       STX BLCNT
       BEQ BLINK
       LDY #$00
       LDA TSR
       LDY #$00
       STA ($1D),Y
       PLA
       STA $01
       PLA
       TAX
       PLA
       TAY
       PLA
       RTI
BLINK  LDA EDFLG
       EOR #%00001000
       LDX #$20
       STX BLCNT
       STA EDFLG
EGRESS PLA 
       STA $01
       PLA
       TAX
       PLA
       TAY 
       PLA 
       RTI
 
SETIRQ LDA #$20
       STA BLCNT    ; initialize blink count
       SEI
       LDA #<SVCIRQ
       STA IRQV
       LDA #>SVCIRQ
       STA IRQV+1
       CLI
       RTS

; VARIOUS STRINGS
INS     .null "       USE CRSR KEYS TO SET TIME. PRESS SPACE TO START/STOP THE CLOCK."

;CHARACTER OFFSETS FROM STARTING CHAR ADDRESS
CHAROFF .byte $0,$14,$28,$3C,$50,$64,$78,$8C,$A0,$B4,$C8,$DC

;CHARACTER DEFINITIONS

CHARS .byte $55,$40,$40,$49    ;ZERO offset=$00
      .byte $5D,$20,$20,$5D
      .byte $5D,$20,$20,$5D
      .byte $5D,$20,$20,$5D
      .byte $4A,$46,$46,$4B

      .byte $20,$20,$20,$6E    ;ONE offset=$14
      .byte $20,$20,$20,$5D
      .byte $20,$20,$20,$5D
      .byte $20,$20,$20,$5D
      .byte $20,$20,$20,$71

      .byte $55,$40,$40,$49    ;TWO offset=$28
      .byte $20,$20,$20,$5D
      .byte $55,$43,$43,$4B
      .byte $5D,$20,$20,$20
      .byte $4A,$46,$46,$4B 

      .byte $55,$40,$40,$49    ;THREE offset=$3C
      .byte $20,$20,$20,$5D
      .byte $20,$43,$43,$73
      .byte $20,$20,$20,$5D
      .byte $4A,$46,$46,$4B

      .byte $72,$20,$20,$72    ;FOUR offset=$50
      .byte $5D,$20,$20,$5D
      .byte $6D,$43,$43,$73
      .byte $20,$20,$20,$5D
      .byte $20,$20,$20,$71

      .byte $70,$43,$43,$6E    ;FIVE offset=$64
      .byte $5D,$20,$20,$20
      .byte $6D,$43,$43,$49
      .byte $20,$20,$20,$5D
      .byte $4A,$46,$46,$4B

      .byte $55,$40,$40,$49    ;SIX offset=$78
      .byte $5D,$20,$20,$20
      .byte $6B,$43,$43,$49
      .byte $5D,$20,$20,$5D
      .byte $4A,$46,$46,$4B

      .byte $70,$43,$43,$6E    ;SEVEN offset=$8C
      .byte $20,$20,$20,$5D
      .byte $20,$20,$20,$5D
      .byte $20,$20,$20,$5D
      .byte $20,$20,$20,$71

      .byte $55,$40,$40,$49    ;EIGHT offset=$A0
      .byte $5D,$20,$20,$5D
      .byte $6B,$43,$43,$73
      .byte $5D,$20,$20,$5D
      .byte $4A,$46,$46,$4B

      .byte $55,$40,$40,$49    ;NINE offset=$B4
      .byte $5D,$20,$20,$5D
      .byte $4A,$46,$46,$73
      .byte $20,$20,$20,$5D
      .byte $4A,$46,$46,$4B

      .byte $20,$55,$49,$20    ;COLON offset=$C8
      .byte $20,$4A,$4B,$20
      .byte $20,$20,$20,$20
      .byte $20,$55,$49,$20
      .byte $20,$4A,$4B,$20

      .byte $20,$20,$20,$20    ;SPACE offset=$DC
      .byte $20,$20,$20,$20
      .byte $20,$20,$20,$20
      .byte $20,$20,$20,$20
      .byte $20,$20,$20,$20
