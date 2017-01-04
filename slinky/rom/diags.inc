;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Internal Diagnostics
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; By Eric Larson Last Modified; 19 Apr. 85
; Modified by Rich williams 9 May 85
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Entry Conditions; Y-Reg has the value of MSlot (screen hole offset)
; X-Reg has the value of DevNo (hardware offset)
; card size is in NumBanks,Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Equates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MPtr      equ $42             ; Indirect pointer to messages
TestNum   equ $00
CompData  equ $45
Limit     equ $46
Value     equ $47
LoopCount equ $49
Dot       equ $AE             ; ASCII period
Bell      equ $07             ; ASCII Bell character
CR        equ $0D             ; ASCII carraige return
ESC       equ $1B             ; ASCII escape character
Kbd       equ $C000
KStrobe   equ $C010
ClrEop    equ $FC42
Home      equ $FC58
ClrEol    equ $FC9C           ; Clear to end of line
Crout     equ $FD8E           ; Print a RETURN character
PrByte    equ $FDDA           ; Print a hex byte

StartTest                     ; Entry point for self diagnostics
          lda #0
          sta LoopCount       ; Clear counter
          sta LoopCount+1
          sta powerup,y       ; Marks card as having no directory
          sta power2,y
          lda numbanks,y      ; Get result
          and #$0F
          sta Limit
          jsr Home
          lda #8
          jsr Print           ; "MEMORY CARD TEST<CR>ESC T0 EXIT<CR>TEST WILL TAKE "
          lda Limit
          lsr a
          lsr a               ; Divide by 4 (0-3)
          pha                 ; Save size index
          ora #4              ; 0-3 > 4-7
          jsr Print           ; 45, 90, 135, or 180
          lda #9
          jsr Print           ; " SECONDS<CR>CARD SIZE = "
          pla                 ; Size index
          jsr Print           ; 256K, 512K, 768K, 1 MEG
          jsr Crout

AddressTest
          lda #5              ; Read & write to address
          sta cv              ; Cursor vertical position
          jsr Crout
          lda #$10
          jsr Print           ; "PASSES = "
          lda LoopCount+1
          jsr PrByte
          lda LoopCount
          jsr PrByte
          jsr NxtLine
          lda #1
          sta TestNum         ; Start test number at 1
          ldy #5              ; Index into data patterns
at1       lda Patterns,y
          jsr setaddr         ; Set address to pattern
          cmp addrl,x         ; Read register back
          bne atf             ; They didn't match
          cmp addrm,x
          bne atf
          ora #$F0            ; Fill high 4 bits
          cmp addrh,x
          bne atf
          dey                 ; Index to next pattern
          bpl at1
          bmi RollOverTest
atf       jmp Fail

RollOverTest                  ; Test 2, Do address counters roll over
          inc TestNum         ; Addrl, m, h = $FF from previous test
          dec addrl,x         ; Start with address SFFFFE.
          lda data,x          ; Dec ok since SFF -> $FE doesn't carry
          sta data,x          ; Address should now be $$0000
          lda addrh,x
          and #$0F            ; Mask off upper 4 bits
          ora addrm,x
          ora addrl,x
          beq AddBusTest      ; Address was indeed $00000
          jmp Fail

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Walk a 1 through the address registers to test for bus shorts
; assumes addresses = 0 from previous test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
AddBusTest                    ; Check for address buss shorts
          inc TestNum         ; Test 3
          lda #1
          sta CompData
          txa                 ; Make pointer to addrl
          clc
          adc #<addrl
          sta MPtr
          lda #$C0
          sta MPtr+1
          lda Limit           ; How many bits used in high address?
          beq ab1             ; If 1M then test D3210
          cmp #$0C            ; If 768K then test D3210
          bne ab2
ab1       lda #$10
ab2       lsr a               ; If 512K then test D210 if 256K then D10
          pha                 ; Save it for later
          ldy #2              ; Walk a one thru high med and low address
ab3       pha
          jsr clraddr         ; Clear address in case of false carries
          pla
          sta (MPtr),y        ; Store pattern in address
          pha
          lda CompData        ; Get value to store
          sta data,x
          inc CompData        ; Each address gets a different value
          pla                 ; Get pattern back
          lsr a               ; Move the 1 over, $80 -> $40 etc.
          bne ab3             ; Until all bits tested
          sta (MPtr),y        ; 2ero out current byte
          ror a               ; 0 -> $80
          dey
          bpl ab3             ; Loop through all 3 address registers
          lda #1              ; Now read em all back
          sta CompData
          pla                 ; Get start value for high byte
          ldy #2
ab4       pha
          jsr clraddr         ; Clear address in case of false 0
          pla
          sta (MPtr),y        ; Set address
          sta Value           ; Don't pha since we might abort
          lda data,x
          cmp CompData        ; Right data?
          bne abFail
          inc CompData
          lda Value
          lsr a
          bne ab4
          sta (MPtr),y
          ror a               ; 0 -> $80
          dey
          bpl ab4
          bmi ClearTest
abFail    jmp Fail

ClearTest                     ; See if all locations clear to zero
          jsr clraddr         ; Set address and A to 0
FillTest                      ; Loop & see if all locations fill to ones
          inc TestNum         ; Test 4 = 00s, Test 5 = FFs
          sta CompData        ; Value to fill RAM with
f1        lda CompData
          sta data,x          ; Write data out
          sta data,x
          sta data,x
          sta data,x
          lda addrl,x
          bne f1
          ora addrm,x         ; Are addrl & addrm both zero?
          bne f1              ; No, keep going
          jsr PrDot           ; Z = 1 if done
          bne f1              ; No, keep going
          jsr NxtLine         ; Go to next line and clear address
cp1       lda data,x          ; Read data back
          cmp CompData
          bne abFail          ; Failed if ne
          lda data,x          ; Do 2 per loop for speed
          cmp CompData
          bne abFail
          lda addrl,x
          bne cp1
          ora addrm,x         ; Are addrl & addrm both zero?
          bne cp1             ; No, keep going
          jsr PrDot           ; Z = 1 if done
          bne cp1             ; No, keep going
          jsr NxtLine         ; Go to next line and clear address
          lda CompData
          eor #$FF            ; 0 -> FF
          bne FillTest

Computed                      ; Each byte gets computed value
          inc TestNum         ; Test 6
          lda #$55            ; Starting data pattern
          sta CompData        ; Address left at 0 from last test
c1        jsr getvalue        ; Value = addrm + addrh + $55, A = 0
c2        clc
          adc Value
          adc CompData
          Sta data,x
          sta CompData        ; Save for next add
          lda addrl,x
          bne c2
          lda addrm,x         ; Time to print a dot?
          bne c1
          jsr PrDot           ; Z = 1 if done
          bne c1
          jsr NxtLine         ; Go to next line and clear address
          lda #$55            ; Starting data pattern
          sta CompData
c3        jsr getvalue        ; Now read em back
c4        clc
          adc Value
          adc CompData
          sta CompData
          lda data,x
          cmp CompData        ; Is it right?
          bne Fail
          lda addrl,x
          bne c4
          lda addrm,x         ; Time to print a dot?
          bne c3
          jsr PrDot           ; Z = 1 if done
          bne c3

Pass                          ; Passed all the tests
          lda #$0B
          jsr Print           ; "CARD OK"
          sed
          lda LoopCount
          clc
          adc #1
          sta LoopCount
          lda LoopCount+1
          adc #0
          sta LoopCount+1
          cld
          jmp AddressTest     ; Loop until first failure

Fail                          ; Display failure message
          pha                 ; Save actual data
          jsr ClrEop
          lda #$0A
          jsr Print           ; "CARD FAILED!<CR>"
          lda TestNum
          cmp #3
          bcs DataErr         ; Not an addressing problem
          pla                 ; There is no failling data really
          lda #$0C
          jsr Print           ; "ADDRESS ERROR"
          jmp ErrCommon
DataErr   lda #$0D
          jsr Print           ; "DATA ERROR "
          sec
          lda addrl,x
          sbc #1              ; Set back to actual failing value
          pha
          lda addrm,x
          sbc #0              ; Propagate borrows (if any)
          pha
          lda addrh,x
          and #$0F            ; Mask off high 4 bits
          sbc #0
          jsr PrByte          ; Print as two hex digits
          pla
          jsr PrByte          ; Print addrm as two hex digits
          pla
          jsr PrByte          ; Print addrl as two hex digits
          lda #$0E
          jsr Print           ; " - "
          pla                 ; Actual data
          eor CompData
          jsr PrByte          ; Print failing data as two hex digits
ErrCommon lda #$0F
          jsr Print           ; "<CR>SEE DEALER FOR SERVICE<CR>"
          rts

PrDot     lda #Dot
          jsr cout
          lda Kbd             ; Is escape pressed?
          cmp #ESC+$80
          bne noesc
          pla                 ; Pop current return address
          pla
          sta KStrobe
noesc     lda addrh,x         ; Test if last dot
          and #$0F
          cmp Limit           ; Z = 1 if last dot
          rts
getvalue  clc
          lda addrm,x
          adc addrh,x
          adc #$55
          sta Value
          lda #0
          rts
NxtLine   jsr Crout           ; Go to next line and clear address
          jsr ClrEol
          ; Fall into clraddr
clraddr   lda #0              ; Clears the address registers
setaddr   sta addrl,x         ; Sets the address registers to A, must do in this order
          sta addrm,x         ; To avoid false carry
          sta addrh,x
          rts

Print     tay                 ; Print message to the screen
          lda Messages,y
          tay
pr1       lda M0,y
          pha
          ora #$80            ; All characters must have high bit set
          jsr cout
          iny                 ; Index to next character
          pla
          bpl pr1             ; Last character had high bit set
          rts

Messages                      ; Table of pointers to actual messages
          dfb M0-M0
          dfb M1-M0
          dfb M2-M0
          dfb M3-M0
          dfb M4-M0
          dfb M5-M0
          dfb M6-M0
          dfb M7-M0
          dfb M8-M0
          dfb M9-M0
          dfb M0A-M0
          dfb M0B-M0
          dfb M0C-M0
          dfb M0D-M0
          dfb M0E-M0
          dfb M0F-M0
          dfb M10-M0
M0        asc "1 ME",'G'+128
M1        asc "256", 'K'+128
M2        asc "512", 'K'+128
M3        asc "768", 'K'+128
M4        asc "18",'0'+128
M5        asc "4", '5'+128
M6        asc "9", '0'+128
M7        asc "13",'5'+128
M8        asc "MEMORY CARD TEST"
          dfb CR
          asc "ESC TO EXIT"
          dfb CR
          asc "TEST WILL TAKE",' '+128
M9        asc " SECONDS"
          dfb CR
          asc "CARD SIZE =",' '+128
M0A       dfb CR,CR
          asc "CARD FAILED"
          dfb CR,Bell,Bell,Bell+128
M0B       dfb CR,CR
          asc "CARD OK"
          dfb CR+128
M0C       asc "ADDRESS ERRO",'R'+128
M0D       asc "DATA ERROR",' '+128
M0E       asc " -",' '+128
M0F       dfb CR
          asc "SEE DEALER FOR SERVICE"
          dfb CR+128
M10       asc "PASSES =",' '+128
Patterns  dfb $FF,$CC,$AA,$55,$33,$00 ; Data buss patterns
          asc 'Rich WilCiams'
