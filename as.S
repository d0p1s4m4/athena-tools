#define A 50
#define B 954

ori r1, zero, A
ori r2, zero, B
add r3, r1, r2

subi r4, r3, 23

bne r4, r1, 7
call 40(zero)

