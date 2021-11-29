;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Routines for making a directory on the disk
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Format - Tests for powerup and puts catalog on the disk if needed
;  Inputs: Y = mslot X = devno
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
format    lda power2,y        ; Is power2 eor powerup = A5
          eor #$A5
          cmp powerup,y
          beq fmtdone
          sta powerup,y
          ; If all screen holes spaces, someone might have cleared
          ; the screen wrong so we won't reformat
          cmp #$05            ; Space eor $A5?
          bne fmnosp
          lda #$A0            ; Check if rest are spaces
          cmp scrn1,y
          bne fmnosp
          cmp scrn3,y
          bne fmnosp
          cmp scrn4,y
          bne fmnosp
          cmp scrn5,y
          bne fmnosp
          cmp scrn6,y
          bne fmnosp
          cmp scrn7,y
          bne fmnosp
          cmp scrn8,y
fmnosp    php                 ; Z = 1 if all spaces
          lda #<megs*$10      ; A meg equals to $10 64K banks
          sta numbanks,y
          lsr a               ; Sizetemp = > block count
          sta sizetemp
          plp
          beq fmtdone
          lda proflag         ; What type of catalog?
          beq fmpas
          ldy #<procat        ; Do a ProDDS catalog
          jsr makecat         ; Put in all but bit map
          lda #01             ; Blocks 0-6 busy
fmpmap1   ldy #32             ; 32 FFs for each $100 blocks
fmpmap2   sta data,x
          ora #$FF            ; Rest are FFs
          dey
          bne fmpmap2
          dec sizetemp
          bne fmpmap1
fmtdone   rts

; Do a Pascal catalog
fmpas     ldy #<pascat
          jsr makecat
          rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MAKECAT - Creates a catalog
;  Input: X = index into catalog tables
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
makecat   lda #0              ; First bank is 0
          sta addrl,x         ; Start at 0
          sta addrm,x
          sta addrh,x
mcboot    sta data,x          ; Zero out first 8 blocks
          lda addrm,x         ; Loop until $400
          and #$F0            ; A = 0 if < $1000
          beq mcboot          ; Leaves us pointing at $1000 (block 2)
          lda #4              ; Point to $400 (block 2)
          sta addrm,x
mcbyte    iny
          lda cattbl,y        ; Get next byte from the table
          cmp #zers           ; Zeros flag?
          beq mc0
          cmp #skpfe          ; $FE zeros?
          beq mcfe
          cmp #sizeflg        ; Block size?
          bne mcntsz
          lda sizetemp        ; Get ># blocks
          bne mcntnm          ; Better not be 0
mcntsz    cmp #nameflg        ; Slot # for name?
          bne mcntnm
          lda mslot           ; Get $Cn
          eor #$F0            ; $Cn -> $3n
mcntnm    Sta data,x          ; Stick byte in catalog
          jmp mcbyte          ; Go to next byte
mc0       iny
          lda cattbl,y        ; Get # zeros
          beq mcadd           ; If 0, It's an address
mcfe      pha                 ; Save count
          lda #0
          sta data,x
          pla
          sec
          sbc #1
          bne mcfe
          beq mcbyte          ; Always taken
mcadd2    sta data,x          ; Store a 0
mcadd     cmp addrl,x         ; Finish off current page
          bne mcadd2
          iny
          lda cattbl,y        ; Get new address
          beq mcdone          ; If 0, all done
          sta addrm,x
          iny
          lda cattbl,y
          sta addrh,x
          jmp mcbyte
mcdone    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The catalog tables
; These tables contain the catalog info compacted
; in a scheme so clever it probably doesn't save
; me anything
; For each bank:
;  The first byte is the bank #, 0 = done
;  zers,n = skip n 0s
;  zers,0 = fill rest of page with 0s and get new address
;  zers,0,0 = all done
;  skpfe = fill $FE 0s
;  nameflg = replace with ascii slot #
;  sizeflg = replace with > block size
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cattbl
procat    equ *-cattbl-1
          dw 0                ; Prevoius pointer
          dw 3                ; Next block
          dfb $F4             ; Storage type
          asc 'RAM'
          dfb nameflg
          dfb zers,$19
          dfb $C3,$27,$0D
          dw 0
          dw 6                ; Bitmap pointer
          dfb 0,sizeflg       ; Number of blocks
          dfb zers,$D7
          dfb skpfe
          dw 2                ; Block 3 $600
          dw 4
          dfb skpfe,skpfe
          dw 3                ; Block 4 $800
          dw 5
          dfb skpfe,skpfe
          dw 4                ; Block 5 $A00
          dfb 0,skpfe         ; Get into second page
          dw zers,0,0         ; All done left at block 6

pascat equ *-cattbl-1
          dfb 0,0
          dfb 6
          dfb zers,3
          dfb 4
          asc 'RAM'
          dfb nameflg
          dfb zers,4
          dfb sizeflg
          dfb zers,0,0

msg       dfb $8D             ; Boot failed message
          scrcode 'UNABLE TO START UP FROM MEMORY CARD.'
          dfb $8D,0
