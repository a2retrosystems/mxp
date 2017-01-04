;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Equates
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
revnum    equ $1000           ; Revision 1.0
pcrevnum  equ $11             ; SmartPort rev 1.1

; Prodos equates
command   equ $42             ; Command to be executed
unit      equ $43             ; D<6-4> = slot
buffer    equ $44             ; Pointer to 512 byte data buffer
block     equ $46             ; Block number

; Protocol converter equates
pparm     equ $43             ; Parameter count
punit     equ $44             ; Unit number
pbuff     equ $45             ; Two byte buffer pointer
pstat     equ $47             ; Status / control code
pblock    equ $47             ; B1ock number
pcount    equ $47             ; Byte count
paddr     equ $49             ; Address for read
tempptr   equ $4A             ; Pointer to params, must be last 2 zp bytes
zused     equ tempptr-command+2 ; Zero page bytes used

; Prodos commands
prostat   equ 0               ; Status command
proread   equ 1               ; Read command
prowr1t   equ 2               ; Write command
proform   equ 3               ; Format command

; DOS equates
iobpl     equ $48             ; Pointer to I0B
iobph     equ $49
ibslot    equ 1               ; Slot * 16
ibdrvn    equ 2               ; Drive 1 or 2
ibtrk     equ 4               ; Track number
ibsect    equ 5               ; Sector number
ibbufp    equ 8               ; 8uffer pointer
ibcmd     equ 12              ; Command
ibstat    equ 13              ; Status
doserr    equ $80             ; D0S io error
dosinit   equ $9D1E           ; DOS init vector use addr-1
dossyn    equ $A6C4-1         ; DOS syntax error
rwts      equ $BD00           ; RWTS entry point

; Error codes
badcmd    equ $01             ; Bad command
badpcnt   equ $04             ; Bad parameter count
badunit   equ $11             ; Bad unit number
badctl    equ $21             ; Bad control / status code
ioerr     equ $27             ; Other I/0 error
nderr     equ $28             ; No device error
badblk    equ $2D             ; Bad block or address

; Prodos boot equates
loc0      equ 0               ; Used by auto boot
loc1      equ 1
bootjmp   equ 2               ; Make jmp to entry
cv        equ $25             ; Cursor vertical position
kswh      equ $39             ; Input switch high byte
bootblk   equ 0               ; Reads block 0
bootbuf   equ $800            ; Into $800
basic     equ $E000           ; Entry point for basic
autoscan  equ $FABA           ; Re-entry point to auto boot
cout      equ $FDED           ; Print a character
setkbd    equ $FE89           ; Reset input hook
setvid    equ $FE93           ; Reset output hook
monitor   equ $FF59           ; Go to monitor if boot fails

; Scratch area equates
sizetemp  equ $478            ; Holds # blocks
error     equ $4F8            ; Error flag
xval      equ $578            ; Value to be returned in X
yval      equ $5F8            ; Value to be returned in Y
devno     equ $778            ; Slot * 16 ($N0) + $88
mslot     equ $7F8            ; $C0 + slot ($CN)

; Slot ram equates
scrn1     equ $478-$C0
scrn2     equ $4F8-$C0
scrn3     equ $578-$C0
scrn4     equ $5F8-$C0
scrn5     equ $678-$C0
scrn6     equ $6F8-$C0
scrn7     equ $778-$C0
scrn8     equ $7F8-$C0
numbanks  equ scrn1           ; Number of 64K banks on card
powerup   equ scrn2           ; Powerup byte
power2    equ scrn3

; Hardware equates, must be in BF00 to avoid double access
addrl     equ $BFF8           ; Address pointer
addrm     equ $BFF9           ; Automat1cally incs every data access
addrh     equ $BFFA
data      equ $BFFB           ; Data pointed to
rdmainram equ $C002           ; Read from main ram
rdcardram equ $C003
wrmainram equ $C004           ; Write to main ram
wrcardram equ $C005
rdramrd   equ $C013           ; D7 = 1 if main ram
rdramwrt  equ $C014

; Other interface equates
proflag   equ $BF00           ; 0 = Pascal, $4C = ProDOS, other = DOS
nameflg   equ $AA             ; Value unused in any catalog
sizeflg   equ $FC             ; Block size flag
zers      equ $FD             ; Catalog skip flag
skpfe     equ $FE             ; Skip FE bytes in catalog
diagcode  equ $CCF0           ; Location of diagnostic code
diagdest  equ $2000           ; Location of diagnostics in ram