#define A 50
#define B 954

.section bss
.align 16
stack_bottom:
.skip 16384
stack_top:

start:

ori r1, zero, A
ori r2, zero, B
add r3, r1, r2

subi r4, r3, 23

li r5, 0xFFD

bne r4, r1, 7
call 40(zero)

mvsrr a0, status

add r1, r2, r3

addi r1, r2, 78

addiu r1, r2, 894

addu r1, r2, r6

li r8, 0xFFFF1111

la r7, start
mov r5, r7

and r1, r2, r3

andi r1, r2, 86

b start

b 0x56

beq r1, r2, start

ret

swap r5, r1
