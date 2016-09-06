.feature c_comments

/******************************************************************************

Copyright (c) 2015, Oliver Schmidt
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL OLIVER SCHMIDT BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

******************************************************************************/

.include "inc/common.i"
.include "inc/commonprint.i"
.include "inc/net.i"

.import a2_set_slot
.import dns_hostname_is_dotted_quad
.import dns_ip
.import dns_set_hostname
.import get_key

.import __DRIVER_LOAD__         ; linker generated
.import __DRIVER_SIZE__         ; linker generated
.import __DISKII_START__        ; linker generated
.import __DISKII_LAST__         ; linker generated

path   := $0280
basic  := $03D0
mli    := $BF00
devadr := $BF10
devcnt := $BF31
devlst := $BF32
memtbl := $BF58
lc_ro  := $C088
lc_wo  := $C089
lc_off := $C08A
lc_rw  := $C08B
bltu2  := $D39A
prbyte := $FDDA

;------------------------------------------------------------------------------

.segment "STARTUP"

; print title
  lda #<title_msg
  ldx #>title_msg
  jsr print_ascii_as_native

; check if Uther II Drive marker (JMP) is present
  bit lc_ro
  lda u2d_marker                ; either JMP or JSR
  cmp #$4C
  bit lc_off
  bcc install_u2d
  jmp remove_u2d


install_u2d:
; print 'uthernet'
  lda #<uthernet_msg
  ldx #>uthernet_msg
  jsr print_ascii_as_native
  lda slot
  clc
  adc #'0'
  jsr print_a

; check for Uthernet II
  lda slot
  jsr a2_set_slot
;  jsr ip65_init
clc
  bcc :+
  lda #<dev_not_found_msg
  ldx #>dev_not_found_msg
  jmp error_exit

; print 'obtain'
: lda #<obtain_msg
  ldx #>obtain_msg
  jsr print_ascii_as_native

; get IP addr
;  jsr dhcp_init
clc
  bcc :+
  jmp ip65_exit
: lda #<cfg_ip
  ldx #>cfg_ip
  jsr print_dotted_quad

; check if server name needs to be resolved
  lda #<dummy
  ldx #>dummy
  jsr dns_set_hostname
  bcc :+
  jmp ip65_exit
: lda dns_hostname_is_dotted_quad
  bne :+

; print 'resolve'
  lda #<resolve_msg
  ldx #>resolve_msg
  jsr print_ascii_as_native
  lda #<dummy
  ldx #>dummy
  jsr print_ascii_as_native

; resolve server name
;  jsr dns_resolve
clc
  bcc :+
  jmp ip65_exit

; print 'install'
: lda #<install_msg
  ldx #>install_msg
  jsr print_ascii_as_native

; switch LC bank 1 to write-only access
  bit lc_wo
  bit lc_wo

; replace (start of) ProDOS Disk II driver with DRIVER segment content
  lda #<__DRIVER_LOAD__
  ldx #>__DRIVER_LOAD__
  sta $9B                       ; source first lo
  stx $9C                       ; source first hi
  lda #<(__DRIVER_LOAD__ + __DRIVER_SIZE__)
  ldx #>(__DRIVER_LOAD__ + __DRIVER_SIZE__)
  sta $96                       ; source last lo
  stx $97                       ; source last hi
  lda #<__DISKII_LAST__
  ldx #>__DISKII_LAST__
  sta $94                       ; destination last lo
  stx $95                       ; destination last hi
  jsr bltu2                     ; Applesoft block transfer up

; copy IP configuration to UDP driver
  ldx #(6 + 4 + 4 + 4)          ; MAC addr + IP addr + netmask + gateway
: lda cfg_mac,x
  sta udp_mac,x
  dex
  bpl :-

; copy server IP address to UDP driver
  ldx #4
: lda dns_ip,x
  sta udp_server,x
  dex
  bpl :-

; backup device count and device list
  ldx #14
: lda devcnt,x
  sta devcnt_bak,x
  dex
  bpl :-

; remove all Disk II devices (if present) from driver address table and device list
  ldx #$00
  stx tmp2
  stx tmp3
: lda devadr,x
  cmp #<__DISKII_START__        ; Disk II driver lo ?
  bne :+
  lda devadr+1,x
  cmp #>__DISKII_START__        ; Disk II driver hi ?
  bne :+
  lda devadr                    ; "No Device Connected" lo
  sta devadr,x
  lda devadr+1                  ; "No Device Connected" hi
  sta devadr+1,x
  txa
  pha
  asl
  asl
  asl
  jsr remove_device             ; remove from device list
  pla
  tax
  inc tmp2                      ; mark removed Disk II driver slot/drive
: asl tmp2
  rol tmp3
  inx
  inx
  cpx #$20                      ; size of driver address table
  bcc :--
  lda tmp2
  sta devmap_disk
  lda tmp3
  sta devmap_disk+1

; backup and replace both slot 1 device driver addresses
  lda devadr   + 1 << 1         ; slot 1 drive 1 lo
  ldx devadr+1 + 1 << 1         ; slot 1 drive 1 hi
  sta devadr_slot1
  stx devadr_slot1+1
  lda devadr   + 1 << 1 + $10   ; slot 1 drive 2 lo
  ldx devadr+1 + 1 << 1 + $10   ; slot 1 drive 2 hi
  sta devadr_slot1+2
  stx devadr_slot1+3
  lda #<driver                  ; Uthernet II driver lo
  ldx #>driver                  ; Uthernet II driver hi
  sta devadr   + 1 << 1         ; slot 1 drive 1 lo
  stx devadr+1 + 1 << 1         ; slot 1 drive 1 hi
  sta devadr   + 1 << 1 + $10   ; slot 1 drive 2 lo
  stx devadr+1 + 1 << 1 + $10   ; slot 1 drive 2 hi

; remove slot 1 devices (if present) from device list
  lda #1 << 4                   ; slot 1 drive 1
  jsr remove_device
  lda #1 << 4 + $80             ; slot 1 drive 2
  jsr remove_device
  jsr print_cr

; add two slot 1 devices to device list just before /RAM (if present) because
; - programs wanting to disconnect /RAM might assume it a the end of the list
; - devices are searched for unknown volumes starting at the end of the list
;   so they are ordered best by speed (RAM > net > disk)
  ldx devcnt
  bmi :+                        ; list temporarily empty
  lda devlst,x
  tay
  and #$F3                      ; normalize $B3 or $B7 or $BB or $BF
  cmp #$B3                      ; AUXMEM RAM disk ?
  beq :++
: clc                           ; have carry only set for equality
  inx
: lda #1 << 4 + $80             ; slot 1 drive 2
  sta devlst,x
  lda #1 << 4                   ; slot 1 drive 1
  sta devlst+1,x
  bcc :+
  tya
  sta devlst+2,x
  inx
: lda #$00                      ; add trailing zero
  sta devlst+2,x
  inc devcnt
  inc devcnt

; print 'connect' messages
  lda #1 << 4                   ; slot 1 drive 1
  jsr print_connect
  lda #1 << 4 + $80             ; slot 1 drive 2
  jsr print_connect
  jsr print_cr
  jsr print_cr

; switch LC bank 1 to read/write access
  bit lc_rw
  bit lc_rw

; fixup UDP driver to access correct I/O addresses
  lda #<(fixup01+1)
  ldx #>(fixup01+1)
  sta ptr1
  stx ptr1+1
  ldx #$FF
  ldy #$00
: lda (ptr1),y
  ora eth_driver_io_base
  sta (ptr1),y
  inx
  cpx #fixup_size
  bcs :+
  lda ptr1
  clc
  adc fixup_data,x
  sta ptr1
  bcc :-
  inc ptr1+1
  bcs :-                        ; always

; Initialize UDP driver
: jsr udp_init

; switch LC bank 1 off and exit
  bit lc_off
  clc                           ; success
  jmp exit

;------------------------------------------------------------------------------

.code


; remove device (if present) from device list
remove_device:
  sta tmp1                      ; device to remove
  ldx devcnt
  bmi :++                       ; list temporarily empty
: lda devlst,x
  cmp tmp1                      ; found ?
  beq :++
  dex
  bpl :-
: rts                           ; device not present
: lda devlst+1,x
  sta devlst,x
  cpx devcnt                    ; cpx before inx to copy trailing zero
  inx
  bcc :-
  dec devcnt

; print 'disconnect'
  lda #<disconnect_msg
  ldx #>disconnect_msg
  bne print_device              ; always


; print 'connect'
print_connect:
  sta tmp1
  lda #<connect_msg
  ldx #>connect_msg

print_device:
  jsr print_ascii_as_native
  lda #<slot_msg
  ldx #>slot_msg
  jsr print_ascii_as_native
  lda tmp1
  lsr
  lsr
  lsr
  lsr
  and #$07                      ; mask drive number
  clc
  adc #'0'
  jsr print_a
  lda #<drive_msg
  ldx #>drive_msg
  jsr print_ascii_as_native
  lda tmp1
  asl                           ; drive number in carry
  lda #$01
  adc #'0'                      ; 1 + drive number + '0'
  jsr print_a
  rts

;------------------------------------------------------------------------------

.code


remove_u2d:
; load Disk II driver
  lda #<prodos_name
  ldx #>prodos_name
  ldy #$33                      ; file page to fast-forward to
  jsr load

; print 'remove'
  lda #<remove_msg
  ldx #>remove_msg
  jsr print_ascii_as_native

; switch LC bank 1 to read-only access
  bit lc_ro

; restore both slot 1 device driver addresses
  lda devadr_slot1
  ldx devadr_slot1+1
  sta devadr   + 1 << 1         ; slot 1 drive 1 lo
  stx devadr+1 + 1 << 1         ; slot 1 drive 1 hi
  lda devadr_slot1+2
  ldx devadr_slot1+3
  sta devadr   + 1 << 1 + $10   ; slot 1 drive 2 lo
  stx devadr+1 + 1 << 1 + $10   ; slot 1 drive 2 hi

; restore all Disk II device driver addresses
  ldx #(1 << 1)                 ; slot 1 drive 1
: asl devmap_disk
  rol devmap_disk+1
  bcc :+
  lda #<__DISKII_START__        ; Disk II driver lo
  sta devadr,x
  lda #>__DISKII_START__        ; Disk II driver hi
  sta devadr+1,x
: inx
  inx
  cpx #$20                      ; size of driver address table
  bcc :--

; restore device count and device list
  ldx #14
: lda devcnt_bak,x
  sta devcnt,x
  dex
  bpl :-

; switch LC bank 1 to write-only access
  bit lc_wo
  bit lc_wo

; restore (start of) ProDOS Disk II driver
  lda #<driver_buffer
  ldx #>driver_buffer
  sta $9B                       ; source first lo
  stx $9C                       ; source first hi
  lda #<(driver_buffer + __DRIVER_SIZE__)
  ldx #>(driver_buffer + __DRIVER_SIZE__)
  sta $96                       ; source last lo
  stx $97                       ; source last hi
  lda #<__DISKII_LAST__
  ldx #>__DISKII_LAST__
  sta $94                       ; destination last lo
  stx $95                       ; destination last hi
  jsr bltu2                     ; Applesoft block transfer up

; switch LC bank 1 off and exit
  bit lc_off
  clc                           ; success
  jmp exit

;------------------------------------------------------------------------------

.code


load:
; set file page to fast-forward to
  sty set_mark_position+1

; build path
  sta ptr1
  stx ptr1+1
  ldy #$00
  ldx path
  dex                           ; 'D'
  dex                           ; '2'
  dex                           ; 'U'
: lda (ptr1),y
  sta path+1,x
  beq :+
  iny
  inx
  bne :-                        ; always
: stx path

; print 'open'
  lda #<open_msg
  ldx #>open_msg
  jsr print_ascii_as_native
  lda #<(path+1)
  ldx #>(path+1)
  jsr print_ascii_as_native

; open file
  jsr mli
  .byte $C8                     ; open call
  .word open_param
  bcc :+
  cmp #$46                      ; file not found
  bne code_exit
  lda #<file_not_found_msg
  ldx #>file_not_found_msg
  bne error_exit                ; always
: lda open_ref_num
  sta set_mark_ref_num
  sta read_ref_num
  sta close_ref_num

; fast-forward file
  jsr mli
  .byte $CE                     ; set_mark call
  .word set_mark_param
  bcs code_exit

; read file
  jsr mli
  .byte $CA                     ; read call
  .word read_param
  bcs code_exit

; close file
  jsr mli
  .byte $CC                     ; close call
  .word close_param
  bcs code_exit
  rts

;------------------------------------------------------------------------------

.code


; print ip65 'code' and exit
ip65_exit:
  lda ip65_error
  cmp #KPR_ERROR_ABORTED_BY_USER
  bne :+
  lda #<abort_msg
  ldx #>abort_msg
  bne error_exit                ; always
: cmp #KPR_ERROR_TIMEOUT_ON_RECEIVE
  bne code_exit
  lda #<timeout_msg
  ldx #>timeout_msg
  bne error_exit                ; always


; print 'code' and exit
code_exit:
  pha
  lda #<error_msg
  ldx #>error_msg
  jsr print_ascii_as_native
  pla
  jsr prbyte
  jsr print_cr
  jsr print_cr
  sec                           ; error
  bcs exit                      ; always


error_exit:
  jsr print_ascii_as_native
  sec                           ; error


exit:
; check ProDOS system bit map
  rol                           ; save carry
  ldx memtbl + >$B800 >> 3      ; protection for pages $B8 - $BF
  cpx #%00000001                ; exactly system global page is protected
  beq quit

; exit to BASIC.SYSTEM
  jmp basic

; exit to ProDOS quit code
quit:
  lsr                           ; restore carry
  bcc :+
  lda #<press_a_key_to_continue
  ldx #>press_a_key_to_continue
  jsr print_ascii_as_native
  jsr get_key
: jsr mli
  .byte $65                     ; quit call
  .word *+2                     ; quit param list
  .byte $04                     ; quit param count
  .byte $00                     ; quit type
  .word $0000                   ; reserved
  .byte $00                     ; reserved
  .word $0000                   ; reserved

;------------------------------------------------------------------------------

.segment "DRIVER"

command = $42
unitnum = $43
buffer  = $44
blknum  = $46

status = $00
read   = $01
write  = $02
format = $03

  cld                           ; necessary for LC setting detection
u2d_marker:
  jmp udp_init

driver:
  lda command
  cmp #read
  beq do_read
  cmp #write
  beq do_write

; the ADTPro Virtual Drive Server Functions support neither the command
; 'status' nor 'format' so simply always pretend everything is just fine
  ldx #<$FFFF
  ldy #>$FFFF
  lda #$00
  clc
  rts


do_read:
  lda buffer
  ldx buffer+1
  inx
  sta $3A
  stx $3A+1
  ldy #$00
  lda $46
: sta (buffer),y
  iny
  bne :-
  lda $47
: sta ($3A),y
  iny
  bne :-
  clc
  rts


do_write:
  lda #$2B
  sec
  rts

;------------------------------------------------------------------------------

.segment "DRIVER"

ptr  := $06         ; 2 byte pointer value
sha  := $08         ; 2 byte physical addr shadow ($F000-$FFFF)
adv  := $EB         ; 2 byte pointer register advancement
len  := $ED         ; 2 byte frame length
tmp  := $FA         ; 1 byte temporary value
bas  := $FB         ; 1 byte socket 3 Base Address (hibyte)

; fixed up at runtime
mode := $C000
addr := $C001
data := $C003


udp_init:
; S/W Reset
  lda #$80
fixup01:
  sta mode
fixup02:
: lda mode
  bmi :-

; Indirect Bus I/F mode, Address Auto-Increment
  lda #$03
fixup03:
  sta mode

; Gateway IP Address Register: IP address of router on local network
  ldx #$00                      ; hibyte
  ldy #$01                      ; lobyte
  jsr set_addr
  ldy #(udp_gateway - udp_mac)
  jsr set_ipv4value

; Subnet Mask Register: Netmask of local network
; -> addr is already set
  ldy #(udp_netmask - udp_mac)
  jsr set_ipv4value

; Source Hardware Address Register: MAC Address
; -> addr is already set
  ldx #$00
: lda udp_mac,x
fixup04:
  sta data
  inx
  cpx #$06
  bcc :-

; Source IP Address Register: IP address of local machine
; -> addr is already set
  ldy #(udp_ip - udp_mac)
  jsr set_ipv4value

; RX Memory Size Register: Assign 4+2+1+1KB to socket 0 to 3
  ldx #$00                      ; hibyte
  ldy #$1A                      ; lobyte
  jsr set_addr
  lda #$06
fixup05:
  sta data

; TX Memory Size Register: Assign 4+2+1+1KB to socket 0 to 3
; -> addr is already set
; -> A is still $06
fixup06:
  sta data

; Socket 3 Source Port Register: 6502
  ldy #$04
  jsr set_addrsocket3
  jsr set_data6502

; Socket 3 Destination IP Address Register: Destination IP address
; this has to be the last call to set_ipv4value because it writes
; as a side effect to 'recv_hdr' and it is the destination IP address
; that has to be present in 'recv_hdr' after initialization
  ldy #$0C
  jsr set_addrsocket3
  ldy #(udp_server - udp_mac)
  jsr set_ipv4value

; Socket 3 Destination Port Register: 6502
; -> addr is already set
  jsr set_data6502

; Socket 3 Mode Register: UDP
  ldy #$00
  jsr set_addrsocket3
  lda #$02
fixup07:
  sta data

; Socket 3 Command Register: OPEN
; -> addr is already set
  lda #$01
fixup08:
  sta data
  rts

set_ipv4value:
  ldx #$03
: lda udp_mac,y
  iny
fixup09:
  sta data
  sta recv_hdr+2,x
  dex
  bpl :-
  rts

set_data6502:
  lda #<6502
  ldx #>6502
fixup10:
  stx data                      ; hibyte
fixup11:
  sta data                      ; lobyte
  rts


udp_recv_init:
; Socket 3 RX Received Size Register: 0 or volatile ?
  lda #$26                      ; Socket RX Received Size Register
  jsr prolog
  bne error

; Socket 3 RX Read Pointer Register
; -> addr already set

; calculate and set pyhsical address
  ldx #>$7C00                   ; Socket 3 RX Base Address
  jsr set_addrphysical

; compare peer IP addr and peer port with expected values
; in 'recv_hdr' and set C(arry flag) if there's a mismatch
  clc
  ldx #$05
  stx tmp
: jsr udp_recv_byte             ; doesn't trash C
  ldx tmp
  eor recv_hdr,x                ; doesn't trash C
  beq :+
  sec
: dec tmp
  bpl :--
  php                           ; save C

; read data length
  jsr udp_recv_byte             ; hibyte
  sta len+1
  jsr udp_recv_byte             ; lobyte
  sta len

; add 8 byte header to set pointer advancement
  clc
  adc #<$0008
  sta adv
  lda len+1
  adc #>$0008
  sta adv+1

; skip frame if it doesn't originate from our expected communicaion peer
  plp                           ; restore C
  bcs udp_recv_done

; return data length
  lda len
  ldx len+1
  rts


udp_send_init:
; set pointer advancement
  sta adv
  stx adv+1

; Socket 3 TX Free Size Register: 0 or volatile ?
  lda #$20                      ; Socket TX Free Size Register
  jsr prolog
  bne error

; Socket 3 TX Free Size Register: < advancement ?
  cpx adv                       ; lobyte
  sbc adv+1                     ; hibyte
  bcc error                     ; not enough free size

; Socket 3 TX Write Pointer Register
  ldy #$24
  jsr set_addrsocket3

; calculate and set pyhsical address
  ldx #>$5C00                   ; Socket 3 TX Base Address
  jsr set_addrphysical

; return success
  clc
  rts

error:
  sec
  rts

prolog:
; check for completion of previous command
; Socket 3 Command Register: 0 ?
  jsr set_addrcmdreg3
fixup12:
  ldx data
  bne :++                       ; not completed -> Z = 0

; Socket Size Register: not 0 ?
  tay                           ; select Size Register
  jsr get_wordsocket3
  stx ptr                       ; lobyte
  sta ptr+1                     ; hibyte
  ora ptr
  bne :+
  inx                           ; -> Z = 0
  rts

; Socket Size Register: volatile ?
: jsr get_wordsocket3
  cpx ptr                       ; lobyte
  bne :+                        ; volatile size -> Z = 0
  cmp ptr+1                     ; hibyte
; bne :+                        ; volatile size -> Z = 0
: rts


udp_recv_byte:
; Read byte
fixup13:
  lda data

; increment physical addr shadow lobyte
  inc sha
  beq incsha
  rts


udp_send_byte:
; Write byte
fixup14:
  sta data

; increment physical addr shadow lobyte
  inc sha
  beq incsha
  rts

incsha:
; increment physical addr shadow hibyte
  inc sha+1
  beq set_addrbase
  rts


udp_recv_done:
; set parameters for commit code
  lda #$40                      ; RECV
  ldy #$28                      ; Socket RX Read Pointer Register
  bne epilog                    ; always


udp_send_done:
; set parameters for commit code
  lda #$20                      ; SEND
  ldy #$24                      ; Socket TX Write Pointer Register

epilog:
; advance pointer register
  jsr set_addrsocket3
  tay                           ; save command
  clc
  lda ptr
  adc adv
  tax
  lda ptr+1
  adc adv+1
fixup15:
  sta data                      ; hibyte
fixup16:
  stx data                      ; lobyte

; Set command register
  tya                           ; restore command
  jsr set_addrcmdreg3
fixup17:
  sta data

; return error (for udp_recv_init)
  bne error                     ; always

set_addrphysical:
fixup18:
  lda data                      ; hibyte
fixup19:
  ldy data                      ; lobyte
  sty ptr
  sta ptr+1
  and #>$03FF                   ; Socket Mask Address (hibyte)
  stx bas                       ; Socket Base Address (hibyte)
  ora bas
  tax
  ora #>$FC00                   ; move sha/sha+1 to $FC00-$FFFF
  sty sha
  sta sha+1

set_addr:
fixup20:
  stx addr                      ; hibyte
fixup21:
  sty addr+1                    ; lobyte
  rts

set_addrcmdreg3:
  ldy #$01                      ; Socket Command Register

set_addrsocket3:
  ldx #>$0700                   ; Socket 3 register base address
  bne set_addr                  ; always

set_addrbase:
  ldx bas                       ; Socket Base Address (hibyte)
  ldy #<$0000                   ; Socket Base Address (lobyte)
  beq set_addr                  ; always

get_wordsocket3:
  jsr set_addrsocket3
fixup22:
  lda data                      ; hibyte
fixup23:
  ldx data                      ; lobyte
  rts

;------------------------------------------------------------------------------

.rodata

prodos_name:
  .byte "PRODOS",0

slot:
  .byte 3

dummy:
  .byte "www.google.com",0

title_msg:
  .byte $0A
  .byte "***************************",$0A
  .byte "**                       **",$0A
  .byte "**  Uther II Drive V1.0  **",$0A
  .byte "**                       **",$0A
  .byte "***************************",$0A
  .byte $0A,0

uthernet_msg:
  .byte "Init Uthernet II in slot ",0

obtain_msg:
  .byte $0A
  .byte $0A
  .byte "Obtain IP address ",0

resolve_msg:
  .byte $0A
  .byte "Resolve ",0

install_msg:
  .byte $0A
  .byte $0A
  .byte "Replace Disk II driver",$0A,0

disconnect_msg:
  .byte $0A
  .byte "Disconnect device",0

connect_msg:
  .byte $0A
  .byte "Add Uther II Drive",0

slot_msg:
  .byte " in slot ",0

drive_msg:
  .byte " drive ",0

remove_msg:
  .byte $0A
  .byte $0A
  .byte "Restore Disk II driver",$0A
  .byte $0A
  .byte "Reconnect devices",$0A
  .byte $0A,0

open_msg:
  .byte "Open ",0

error_msg:
  .byte $0A
  .byte "Error $",0

dev_not_found_msg:
  .byte $0A
  .byte "Device not found",$0A
  .byte $0A,0

abort_msg:
  .byte $0A
  .byte "User abort",$0A
  .byte $0A,0

timeout_msg:
  .byte $0A
  .byte "Timeout",$0A
  .byte $0A,0

file_not_found_msg:
  .byte $0A
  .byte "File not found",$0A
  .byte $0A,0

fixup_data:
  .byte fixup02-fixup01, fixup03-fixup02, fixup04-fixup03, fixup05-fixup04
  .byte fixup06-fixup05, fixup07-fixup06, fixup08-fixup07, fixup09-fixup08
  .byte fixup10-fixup09, fixup11-fixup10, fixup12-fixup11, fixup13-fixup12
  .byte fixup14-fixup13, fixup15-fixup14, fixup16-fixup15, fixup17-fixup16
  .byte fixup18-fixup17, fixup19-fixup18, fixup20-fixup19, fixup21-fixup20
  .byte fixup22-fixup21, fixup23-fixup22

fixup_size = * - fixup_data

;------------------------------------------------------------------------------

.data

open_param:
  .byte $03                     ; open param count
  .word path
  .byte $00                     ; page aligned io_buffer lo
  .byte >io_buffer              ; page aligned io_buffer hi
open_ref_num:
  .byte $00

set_mark_param:
  .byte $02                     ; set_mark param count
set_mark_ref_num:
  .byte $00
set_mark_position:
  .byte $00, $00, $00

read_param:
  .byte $04                     ; read param count
read_ref_num:
  .byte $00
  .word driver_buffer
  .word __DRIVER_SIZE__
  .word $0000

close_param:
  .byte $01                     ; close param count
close_ref_num:
  .byte $00

;------------------------------------------------------------------------------

.bss

  .res $0100                    ; for page aligned io_buffer
io_buffer:
  .res $0400

driver_buffer:
  .res $0700

;------------------------------------------------------------------------------

.segment "DRIVER"

devcnt_bak:
  .byte $00
  .word $0000, $0000, $0000, $00000, $0000, $0000, $0000
devmap_disk:
  .word $0000
devadr_slot1:
  .word $0000, $0000

udp_mac:
  .word $0000, $0000, $0000
udp_ip:
  .word $0000, $0000
udp_netmask:
  .word $0000, $0000
udp_gateway:
  .word $0000, $0000
udp_server:
  .word $0000, $0000

recv_hdr:
  .word 6502                    ; server port       (little endian !)
  .word $0000, $0000            ; server IP address (little endian !)

;------------------------------------------------------------------------------
