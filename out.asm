.text
.globl main
L24:
sub $sp, $sp, ~60
L27:
lw $a0, 0($fp)
addi $t0, $fp, ~4
sw $v0, 0($t0)
addi $t0, $fp, ~8
sw $s0, 0($t0)
addi $t0, $fp, ~12
sw $s1, 0($t0)
addi $t0, $fp, ~16
sw $s2, 0($t0)
addi $t0, $fp, ~20
sw $s3, 0($t0)
addi $t0, $fp, ~24
sw $s4, 0($t0)
addi $t0, $fp, ~28
sw $s5, 0($t0)
addi $t0, $fp, ~32
sw $s6, 0($t0)
addi $t0, $fp, ~36
sw $s7, 0($t0)
L25move $v0, $t0
lw $v0, ~4($fp)
lw $s0, ~8($fp)
lw $s1, ~12($fp)
lw $s2, ~16($fp)
lw $s3, ~20($fp)
lw $s4, ~24($fp)
lw $s5, ~28($fp)
lw $s6, ~32($fp)
lw $s7, ~36($fp)
j L26 
L26:
addi $sp, $sp, ~60
jr $ra
.data
L25: .ascii "string"
