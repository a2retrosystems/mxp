;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Routines for making a directory on the disk
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; TESTSIZE - determines ramdisk size nondestructively
;  Inputs: Y = mslot X = devno
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
testsize  lda #0              ; Set address reg
          sta addrl,x
          sta addrm,x
          lda #$10            ; Start at 1 meg and go down
          sec
tsloop    sbc #1              ; Move down a bank
          sta addrh,x
          lda data,x          ; Save the data that is there
          pha
          dec addrl,x         ; Fix address
          lda #$A5            ; Store something there
          sta data,x
          dec addrl,x
          eor data,x          ; 0 if the data is there
          dec addrl,x
          cmp #1              ; C = 0 if data ok
          pla
          sta data,x          ; Restore what was there
          lda addrh,x
          and #$0F            ; 0nly lower 4 bits valid
          beq tsnoram         ; No ram somehow!!!
          bcs tsloop          ; Loop until we find a bank
          adc #1              ; C = 0 from compare
tsnoram   sta numbanks,y
          lsr a               ; Sizetemp = < block count
          sta sizetemp
          rts

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
          jsr testsize
          plp
          beq fmtdone
          lda proflag         ; What type of catalog?
          beq fmpas
          cmp #$4c            ; JMP if ProDDS
          bne fmdos
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
fmpas     ldy #pascat
          jsr makecat
          rts

; Do a DOS catalog
fmdos     ldy #doscat
          jsr makecat
          lda #$44            ; Point to track 3 bitmap
          sta addrl,x         ; Addrl = 0 from makecat
          lda sizetemp        ; Check if at least 512K
          ldy #114            ; Assume 256K
          cmp #4              ; At least $400 blocks
          blt fmdbmap
          ldy #186            ; Make 400K volume
fmdbmap   lda addrl,x         ; Don't free catalog
          cmp #$7C            ; Track $11?
          bne fmdok
          lda #$7E            ; Skip first 16 sectors
          sta addrl,x         ; 7C -> 7E so no false carry
fmdok     lda #$FF
          sta data,x
          dey
          bne fmdbmap
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
          lda sizetemp        ; Get <# blocks
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
;  sizeflg = replace with < block size
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

doscat    equ *-cattbl-1
          dfb zers,0,$20,$02  ; Trk $11 Sec 0 = $022000
          dfb 2               ; Sec 0 = VTOC
          dfb $11,$0F         ; Pointer to catalog
          dfb $4              ; Dos release
          dfb 0,0             ; Unused
          dfb $FB             ; Volume number
          dfb zers,$20
          dfb $7A             ; TS pairs in TS list
          dfb zers,8
          dfb $FF,$FF,$FF,$FF ; Allocation mask
          dfb $32             ; # Tracks
          dfb $20             ; # Sectors
          dw  $100            ; # Bytes per sector
          dfb zers,$CB
          dfb skpfe           ; Sector 1 all 0s
          dfb $11,$01,skpfe   ; Next cat sector pointer
          dfb $11,$02,skpfe
          dfb $11,$03,skpfe
          dfb $11,$04,skpfe
          dfb $11,$05,skpfe
          dfb $11,$06,skpfe
          dfb $11,$07,skpfe
          dfb $11,$08,skpfe
          dfb $11,$09,skpfe
          dfb $11,$0A,skpfe
          dfb $11,$0B,skpfe
          dfb $11,$0C,skpfe
          dfb $11,$0D,skpfe
          dfb $11,$0E,skpfe
          dfb zers,0,$20,$02  ; Leave pointing at VTOC
          dfb zers,0,0        ; All done

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
