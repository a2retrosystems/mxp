;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Boot rom code
; this is the code in the $CN00 space
; This code is done as an include with 'slot' being a slot
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          cmp #$20            ; Boot entry point
          cmp #$00            ; Signature byte stuff
          cmp #$03
          cmp #$00
          ; Here is the boot code
          ; Reads in block 0 into $800 and executes at $801
          sei                 ; No interrupts if booting
          jsr setkbd          ; Reset the hooks
          jsr setvid
          ldx #slot*$10+$88
          ldy #slot+$C0
          sty mslot
          bit $CFFF
          jsr boot            ; Go get boot block
          bne btok
          ; Boot failed, continue scan or break
          ; First print out a nice message
          lda #23             ; Go to bottom of screen
          sta cv
          ldx #0
prmsg     lda msg,x
          beq mdone           ; Done if char 0
          jsr cout
          inx
          bne prmsg           ; Always taken
mdone     lda loc0            ; Does zp point to us
          bne noaut           ; Branch if not auto boot
          lda loc1
          cmp #slot+$C0
          bne noaut
          jmp autoscan        ; Continue auto boot
noaut     jmp basic           ; Go to basic
btok      ldx #slot*$10       ; X = n0
          jmp bootbuf+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Entry point for prodos driver
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
entry     jmp ent             ; Prodos entry point, jump to ProDOS
          jmp pconv           ; Jump to PC
ent       lda #nderr          ; Assume wrong drive
          ldx unit
          bmi rats            ; Error!!!
          lda #badcmd         ; Assume bad command
          ldy command         ; Get command
          ; The 'cpy #4' below used to be a 'cmp #4'
          ; Because of this change, revnum became 1.0.1
          cpy #4
          bge rats            ; Branch if bad command
          ldx #zused          ; Save zp
zsave     lda command-1,x
          pha
          dex
          bne zsave
          tya                 ; Add 20 to command for table look up
          clc
          adc #20

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This code is common to PC and ProDOS and DOS entry points
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
doit      ldx #slot*$10+$88
          ldy #slot+$C0       ; X = Cn
          sty mslot
          bit $CFFF           ; Deselect others in C800 space
          jsr execute         ; Go do command
done      ldx #0              ; Restore zp
rsloop    pla
          sta command,x
          inx
          cpx #zused
          blt rsloop
          ldx xval            ; Set X and Y to whatever
          ldy yval
          lda error           ; Was there an error?
rats      cmp #1              ; C = 1 if not 0
          ora #0              ; Set N,Z flags
          rts
pccmd     lda #badcmd         ; Bad command
          bne pcerr
pcbad     lda #badunit        ; Bad protocol unit
pcerr     sta error           ; Save the error code
          bne done

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Protocol converter entry point
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pconv     pla                 ; Pull the return address
          tay
          cmp #$FD            ; C = 1 if carry in +3
          pla                 ; High byte of return address
          tax
          adc #0
          pha
          tya
          adc #3              ; C = 0 from previous add
          pha                 ; Return address + 3 now pushed
          lda tempptr+1       ; Save zero page
          pha
          stx tempptr+1
          ldx #zused-1
pcsvzp    lda command-1,x     ; Save rest of zp
          pha
          dex
          bne pcsvzp
          sty tempptr
          ldy #3              ; Get (tempptr) + 3, +2, +1
pcgtp     sta command-1,y     ; First store don't care
pcskp     lda (tempptr),y
          dey
          bne pcgtp
          tax                 ; X = user command, Command = pointer to parm list, Y = 0
          ldy #8              ; Max # of param bytes A
pcparms   lda (command),y     ; Get the parms
          sta command+1,y     ; Note last store is on top of pointer
          dey
          bpl pcparms
          lsr punit           ; Unit 0 or 1?
          bne pcbad
          txa                 ; A = Command
          rol a               ; Get unit into low bit
          cmp #20             ; C = 1 if invalid command
          bcs pccmd           ; Branch if bad command
          bcc doit            ; Always taken
