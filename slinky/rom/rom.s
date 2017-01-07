;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Driver for the ramdisk
; for the Apple II computer
;
; by Rich Williams
; Version 1.0 September 1985
;
; Copyright Apple Computer, Inc. 1985
; All rights reserved
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macpack  generic
.macpack  apple2
.feature  labels_without_colons
.feature  loose_string_term
.define   equ =
.define   asc .byte
.define   dfb .byte
.define   dw  .word

.include  'equates.inc'

.segment  'msg'
          asc 'This wonderful, wonderful rom'
          asc 'is brought to you by Richard Williams'
          asc 'and should only be used for'
          asc 'the benefit of mankind'

.macro    bootromdefs
          dfb pcrevnum        ; Smartport revision number
          dfb $01             ; Mark ram card
          dw 0                ; Number of blocks = 0 for status call
          ; Status bits
          ;  7 = medium is removable
          ;  6 = device is interruptable
          ;  5-4 = number of volumes (0..3 means 1..4)
          ;  3 = device supports Format call
          ;  2 = device can be written to
          ;  1 = device can be read from (must be 1)
          ;  0 = device status can be read (must be 1)
          dfb %01001111
          dfb <entry
.endmacro

.scope    boot1
slot      equ 1
.segment  'boot1'
.include  'bootrom.inc'
.segment  'defs1'
          bootromdefs
.endscope

.scope    boot2
slot      equ 2
.segment  'boot2'
.include  'bootrom.inc'
.segment  'defs2'
          bootromdefs
.endscope

.scope    boot3
slot      equ 3
.segment  'boot3'
.include  'bootrom.inc'
.segment  'defs3'
          bootromdefs
.endscope

.scope    boot4
slot      equ 4
.segment  'boot4'
.include  'bootrom.inc'
.segment  'defs4'
          bootromdefs
.endscope

.scope    boot5
slot      equ 5
.segment  'boot5'
.include  'bootrom.inc'
.segment  'defs5'
          bootromdefs
.endscope

.scope    boot6
slot      equ 6
.segment  'boot6'
.include  'bootrom.inc'
.segment  'defs6'
          bootromdefs
.endscope

.scope    boot7
slot      equ 7
.segment  'boot7'
.include  'bootrom.inc'
.segment  'defs7'
          bootromdefs
.endscope

.segment  'exec'
.include  'execute.inc'
.include  'makecat.inc'

.segment  'diags'
.include  'diags.inc'
