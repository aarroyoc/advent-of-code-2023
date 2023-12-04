	# Day 4 - Advent of Code

	.section .data
printf_int_str:	.string "%d\n"
winners: .double -1,-1,-1,-1,-1,-1,-1,-1,-1,-1
numbers: .double 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
tickets: .space 1616
	.section .text
	.global main
main:
	jal init_tickets
	li s11, 0
	la s10, tickets
read_header:	
	call getchar
	blt a0, zero, exit
	li t0, 0x3A      # :
	bne a0, t0, read_header
read_winners:
	la s1, winners
loop_space_winners:
	li s0, 0
	call getchar
	li t0, 0x20      # SPACE
	beq a0, t0, loop_space_winners
	li t0, 0x7C      # |
	beq a0, t0, read_my_numbers
	add s0, s0, a0
	addi s0, s0, -48
loop_winners:
	call getchar
	li t0, 0x20     # SPACE
	beq a0, t0, save_winner
	li t0, 10
	mul s0, s0, t0
	addi a0, a0, -48
	add s0, s0, a0
	j loop_winners
save_winner:
	sd s0, 0(s1)
	addi s1, s1, 8
	j loop_space_winners

read_my_numbers:
	la s1, numbers
loop_space_numbers:
	li s0, 0
	call getchar
	li t0, 0x20      # SPACE
	beq a0, t0, loop_space_numbers
	add s0, s0, a0
	addi s0, s0, -48
loop_numbers:
	call getchar
	li t0, 0x20     # SPACE
	beq a0, t0, save_number	
	li t0, 0x0A      # LF
	beq a0, t0, save_final_number
	li t0, 10
	mul s0, s0, t0
	addi a0, a0, -48
	add s0, s0, a0
	j loop_numbers
save_number:
	sd s0, 0(s1)
	addi s1, s1, 8
	j loop_space_numbers
save_final_number:
	sd s0, 0(s1)

find_magic_num:
	la s0, numbers
	li s1, 0
	addi s2, s0, 192
loop_find_magic_sum:
	ld a0, 0(s0)
	jal number_is_winner
	beq a0, zero, continue_find_magic_sum_loop
	addi s1, s1, 1
continue_find_magic_sum_loop:	
	addi s0, s0, 8
	bgt s0, s2, exit_magic_loop
	j loop_find_magic_sum
exit_magic_loop:
	beq s1, zero, exit_magic_loop_zero
	mv a0, s1
	jal add_tickets
	li t0, 1
	addi s1, s1, -1
	sll a0, t0, s1
	add s11, s11, a0
	j read_header
exit_magic_loop_zero:
	li a0, 0
	jal add_tickets
	li a0, 0
	j read_header

	
number_is_winner:
	la t0, winners
	addi t1, t0, 72
	mv t2, a0
loop_number_is_winner:	
	ld t3, 0(t0)
	beq t2, t3, is_winner
	addi t0, t0, 8
	bgt t0, t1, exit_no_winner
	j loop_number_is_winner
is_winner:
	li a0, 1
	jr ra
exit_no_winner:
	li a0, 0
	jr ra

add_tickets:
	mv t3, a0
	ld t4, 0(s10)
	mv t5, s10
	beq t4, zero, exit_add_tickets
loop_add_tickets:
	beq t3, zero, exit_add_tickets
	addi t5, t5, 8
	ld t6, 0(t5)
	add t6, t6, t4
	sd t6, 0(t5)
	addi t3, t3, -1
	j loop_add_tickets
exit_add_tickets:
	addi s10, s10, 8	
	jr ra

sum_tickets:
	mv s3, ra
	la s10, tickets
	addi s0, s10, 1600
	li s1, 0
sum_tickets_loop:
	bgt s10, s0, exit_sum_tickets
	ld s2, 0(s10)
	mv a0, s2
	add s1, s1, s2
	addi s10, s10, 8
	j sum_tickets_loop
exit_sum_tickets:
	mv a0, s1
	jr s3

init_tickets:
	la s10, tickets
	addi t4, s10, 1600
	li t5, 1
init_tickets_loop:
	bgt s10, t4, exit_init_tickets
	sd t5, 0(s10)
	addi s10, s10, 8
	j init_tickets_loop
exit_init_tickets:
	jr ra
	
exit:
	mv a0, s11
	jal print_int
	jal sum_tickets
	jal print_int
	li a7, 93
	li a0, 0
	ecall

print_int:
	addi sp, sp, -8
	sd ra, 0(sp)
	mv t1, a0
	la a0, printf_int_str
	mv a1, t1
	call printf
	ld ra, 0(sp)
	addi sp, sp, 8
	jr ra
