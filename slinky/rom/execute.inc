;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; execution routines. These routines must begin in the same page
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; EXECUTE - Does command in command
;  Inputs: a = command x = devno
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
execute   sta command         ; Save command
          stx devno           ;  and hardware index
          lda #0              ; Clear error flag
          sta error
          jsr format          ; Do we need to format?
          ldy command         ; Get command
          lda parmtbl,y       ; Check parameter count
          bmi exec2           ; If neg. no parm check
          cmp pparm
          bne pzcnt
exec2     lda #>xstatus       ; All entry points on same page
          pha
          lda cmdtbl,y
          pha
          ldy mslot
          ldx devno
          rts
pzcmd     lda #badcmd         ; Invalid command
          bne pzcnt2
pzcnt     lda #badpcnt
pzcnt2    sta error
iorts     rts
pwrite2   jmp pwrite          ; Entry point in this page
dosconv2  jmp dosconv

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; XSTATUS - ProDOS status call
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
xstatus   lda numbanks,y      ; Size = # 64K banks / 2
          lsr a
          sta yval
          lda #0
          sta xval
          rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PSTAT0 - Status call for device 0
; returns status block for call 0
; 1 0 0 0 0 0 0 0
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pstat0    lda pstat           ; Must be call 0
          bne stbad           ; Branch if bad
          sta yval            ; Set bytes read count
          ldy #8
          sty xval
          dey
st0lp     sta (pbuff),y       ; Save out the 0s
          dey
          bne st0lp
          lda #1
          sta (pbuff),y
          rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PCNTL - Control call
; call 0 (reset) is implemented for both devices
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pcntl     lda pstat           ; Call 0?
          beq pcntok
stbad     lda #badctl         ; Oops! bad status/control number
          sta error
pcntok    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PSTATUS - Status call for device 1
; call 0,3 supported
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pstatus   lda #4              ; Number bytes for call 0
          ldx pstat
          beq pst0
          cpx #3              ; Is it #3?
          bne stbad           ; Branch if bad call
          lda #25             ; # bytes for call 3
pst0      sta xval
          ldx #0
          stx yval
          tay
          dey
pstmov    lda stattbl,y       ; Move the status info
          sta (pbuff),y
          dey
          bpl pstmov
          ldy mslot           ; Get the size
          lda numbanks,y
          lsr a
          ldy #2
          sta (pbuff),y
          rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; XREAD - Read a block
; XWRITE - Write a block
;
; ProDDS read & write are changed into Protocol converter read block
; and write block which are then changed into read & write
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
xread     bit iorts           ; V = 1 for read
          dfb $50             ; BVC never taken
xwrite    clv                 ; V = 0 for write
          lda block+1         ; Move block & buffer pointer
          sta pblock+1
          lda block           ; Be careful not to step
          sta pblock          ;  on our own toes
          lda buffer+1
          sta pbuff+1
          lda buffer
          sta pbuff
          lda #0
          sta pblock+2
          beq xread2          ; Skip past other SEV & CLV

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PRDBLK - Protocol converter block read
; PWRBLK - Protocol converter block write
;
; changed into PPEAD & PWRITE
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
prdblk    bit iorts           ; V = 1 for read
          dfb $50             ; BVC never taken
pwrblk    clv
xread2    lda pblock          ; Convert block into 512 bytes
          asl a
          sta paddr+1
          lda pblock+1
          rol a
          sta paddr+2
          bcs prbad2          ; If C=1 bad address
          lda pblock+2        ; Third byte must be 0
          bne prbad2
          sta paddr           ; Low byte of address is 0
          sta pcount          ; Count = $200
          lda #2
          sta pcount+1
          lda rdramwrt        ; Fix aux bit in address
          bvs prdread
          lda rdramrd
prdread   and #$80            ; D7 = 1 if aux
          ora paddr+2
          sta paddr+2
          bvs pread           ; Go do read
          jmp pwrite
prbad2    jmp prbad

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PREAD - Reads bytes from card into the Apple
; D7 of the address = 1 if aux ram
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pread     lda paddr           ; Move the address
          sta addrl,x
          lda paddr+1
          sta addrm,x
          lda paddr+2
          and #$7F            ; Mask off high bit
          cmp numbanks,y      ; Valid address
          bge prbad
          sta addrh,x
          bit rdramwrt        ; Save current bank
          php
          sta wrmainram       ; Assume main
          bit paddr+2         ; If D7 = 1 then aux
          bpl prmain
          sta wrcardram       ; Its the card ram
prmain    ldy #0
          lda pcount+1        ; More than a page to move?
          sta yval
          beq prlast
prloop    lda data,x          ; Get a byte
          sta (pbuff),y
          iny
          lda data,x
          sta (pbuff),y
          iny
          bne prloop
          inc pbuff+1         ; Bump buffer pointer to next page
          dec pcount+1        ; Dec page count
          bne prloop
prlast    lda pcount          ; Any bytes left to do?
          beq prdone
          sta xval            ; Save bytes moved
          lsr a               ; C = 1 if odd # of bytes
          bcs prodd
prloop2   lda data,x
          sta (pbuff),y
          iny
prodd     lda data,x
          sta (pbuff),y
          iny
          cpy pcount
          bne prloop2
prdone    sta wrmainram       ; Fix main / aux ram
          plp
          bpl prmain2
          sta wrcardram
prmain2   rts
prbad     lda #badblk         ; Invalid address
          sta error
          rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PWRITE - writes bytes from Apple to card
; D7 of the address = 1 if aux ram
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pwrite    lda paddr           ; Move the address
          sta addrl,x
          lda paddr+1
          sta addrm,x
          lda paddr+2
          and #$7F            ; Mask off high bit
          cmp numbanks,y      ; Valid address
          bge prbad
          sta addrh,x
          bit rdramrd         ; Save current bank
          php
          sta rdmainram       ; Assume main
          bit paddr+2         ; If D7 = 1 then aux
          bpl pwmain
          sta rdcardram       ; Its the card ram
pwmain    ldy #0
          lda pcount+1        ; More than a page to move?
          sta yval
          beq pwlast
pwloop    lda (pbuff),y       ; Get a byte
          sta data,x
          iny
          lda (pbuff),y
          sta data,x
          iny
          bne pwloop
          inc pbuff+1         ; Bump buffer pointer to next page
          dec pcount+1        ; Dec page count
          bne pwloop
pwlast    lda pcount          ; Any bytes left to do?
          sta xval
          beq pwdone
          lsr a               ; C = 1 if odd # of bytes
          bcs pwodd
pwloop2   lda (pbuff),y
          sta data,x
          iny
pwodd     lda (pbuff),y
          sta data,x
          iny
          cpy pcount
          bne pwloop2
pwdone    sta rdmainram       ; Fix main / aux ram
          plp
          bpl pwmain2
          sta rdcardram
pwmain2   rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Here is the rest of the boot code
;  Inputs: A = kswh, X = devno, Y = mslot
;  Outputs: Z = 1 if boot fails
;
; Jumps to DDS patch if IN# from DOS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
boot      stx devno
          cmp mslot           ; Is it a IN#
          bne btnodos
          lda proflag         ; Are we in DOS?
          beq btnodos         ; 0 = Pascal
          cmp #$4C            ; JMP = ProDOS
          bne dospatch        ; Go patch DOS
btnodos   lda power2,y        ; If power up bytes not set, don't boot
          eor #$A5
          cmp powerup,y
          bne btfail
          ldy #3
btmv      lda btcmd,y
          sta buffer,y
          dey
          bpl btmv
          ldy mslot
          jsr xread           ; Go read the block and return
          lda bootbuf+1
          rts
btfail    lda #0              ; Z = 1 for fail
          rts
btcmd     dw $800
          dw 0                ; Read in block 0 @ $800

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; DOSPATCH - Patches rwts to jump to us
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
dospatch  lda #$4C            ; JMP opcode
          sta rwts
          lda #<boot1::dosent
          sta rwts+1          ; Make page 3 vector point to us
          sty rwts+2          ; Y = Cn
          lda #<dossyn        ; Patch out init command
          sta dosinit
          lda #>dossyn
          sta dosinit+1
          pla                 ; Pop off return address
          pla
          ldx #0
          lda #$98            ; Return a control-X
dcrts     rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; DOSCONV - Changes DOS command into ours
;  Output: command table in zp Y = command
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
dosconv   ldy #ibdrvn         ; Get drive 1 or 2
          lda (iobpl),y
          cmp #1              ; Dnly 1 valid
          beq dc1
dcerr     jmp pzcmd           ; Bad drive number
dc1       ldy #ibtrk          ; Get track & sector
          lda (iobpl),y       ; Addr = 00000TTT TTTSSSSS 00000000
          lsr a
          ror paddr+1
          lsr a
          ror paddr+1
          lsr a
          sta paddr+2
          lda paddr+1
          ror a
          and #$E0
          iny
          ora (iobpl),y       ; Dr in sector
          sta paddr+1
          ldy #ibbufp         ; Get pointer to user’s buffer
          lda (iobpl),y
          sta pbuff
          iny
          lda (iobpl),y
          sta pbuff+1
          ldy #ibcmd          ; Get command
          lda (iobpl),y
          beq dcrts           ; 0 = null = do nothing
          and #3
          beq dcerr           ; 4 = format is an error
          ora #17             ; 1 -> 17 , 2 -> 19
          tay                 ; Y = command
          ldx #0
          stx pcount          ; Count = $100 bytes
          stx paddr
          inx
          stx pcount+1
          jmp exec2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Various data tables
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Status info table
stattbl   dfb $F8             ; Status byte
          dfb $00,$00,$00     ; Size
          dfb 7               ; Name length
          asc 'RAMCARD'
          asc '         '
          dw 0                ; Type subtype
          dw revnum           ; Version

; Table of parameters
parmtbl   dfb 03,03,03
          dfb 03,03,03
          dfb 01,01,03
          dfb 03,01,01
          dfb 01,01,01
          dfb 01,04,04
          dfb 04,04,$FF
          dfb $FF,$FF,$FF
          dfb $FF

; Table of command addresses
cmdtbl    dfb <pstat0-1       ; status unit 0
          dfb <pstatus-1      ; Status
          dfb <pzcmd-1        ; Read block unit 0
          dfb <prdblk-1       ; Read block
          dfb <pzcmd-1        ; Wr1te block unit 0
          dfb <pwrblk-1       ; Write block
          dfb <pzcmd-1        ; Format unit 0
          dfb <iorts-1        ; Format
          dfb <pcntl-1        ; Control unit 0
          dfb <pcntl-1        ; Control
          dfb <iorts-1        ; Init unit 0
          dfb <iorts-1        ; Init
          dfb <pzcmd-1        ; Open unit 0
          dfb <pzcmd-1        ; Open
          dfb <pzcmd-1        ; Close unit 0
          dfb <pzcmd-1        ; Close
          dfb <pzcmd-1        ; Read unit 0
          dfb <pread-1        ; Read
          dfb <pzcmd-1        ; Write unit 0
          dfb <pwrite2-1      ; Write
          dfb <xstatus-1      ; ProDOS status call
          dfb <xread-1        ; ProDOS read call
          dfb <xwrite-1       ; ProDOS write call
          dfb <iorts-1        ; ProDOS format call
          dfb <dosconv2-1     ; Dos Command
