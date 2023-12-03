	.section .data
printf_int_str:	.string "%d\n"

	.section .text
	.global main
main:
	li s0, 0 		# s0 - Sum, solution
loop:	
	jal read_line
	add s0, s0, a0	
	jal print_int
	j loop
exit:
	mv a0, s0
	jal print_int
	li a7, 93
	li a0, 0
	ecall

print_int:
	mv t0, ra
	mv t1, a0
	la a0, printf_int_str
	mv a1, t1
	call printf
	jr t0

read_line:
	mv s3, ra		# s3 - Return address
	li s1, 0		# s1 - First digit
	li s2, 0		# s2 - Last digit
read_char_loop:	
	call getchar
	mv t0, a0		# t0 - New character
	li t1, 0x0A		# t1 - Character LF
	beq t0, t1, exit_read_line
	blt t0, zero, exit      # EOF
	li t1, 0x30		# t1 - 0 Character
	blt t0, t1, read_char_loop
	li t1, 0x39		# t1 - 9 Character
	bgt t0, t1, read_char_loop
is_a_digit:
	mv s2, t0
	beq s1, zero, is_first_digit
	j read_char_loop
is_first_digit:
	mv s1, s2
	j read_char_loop
exit_read_line:
	li t0, 48
	sub s1, s1, t0
	sub s2, s2, t0
	li t0, 10
	mul s1, s1, t0
	add a0, s1, s2
	jr s3
	
