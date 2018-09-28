#####################################################################
# Alunos: Guilherme Sergei Schäfer e Gustavo Koefender
# E-mails: guilherme.schafer@acad.pucrs.br e gustavo.koefender@acad.pucrs.br
# Professor: César Marcon
# Programa para testar todas instruções suportadas pelo MIPS_MC
#####################################################################


	.text			
		.globl	main		

main:			
		la		$t0,var
		lw 		$t1,($t0)
		addiu	$t1,$t1,100
		sw		$t1,($t0)	
		lui		$t0,0xf3	
		ori		$t0,$t0,0x23		
		beq		$t1,$zero,loop	
		bne		$t3,$t4,loop	
		addu	$t3,$t0,$t1	
		subu	$t4,$t0,$t1	
		and		$t6,$t0,$t1	
		or		$t7,$t0,$t1	
		xor		$t8,$t0,$t1	
		nor		$t9,$t0,$t1	
		andi	$t0,$t0,0x00ab	
		xori	$t0,$t0,0xffab	
		sll		$t0,$t0,2	
		srl		$t0,$t0,2	
		sllv	$t0,$t9,$s2	
		sra		$t0,$t0,4	
		srav	$t0,$t0,$s2
		srlv	$t0,$t0,$s2	
		la		$t0,var	
		lbu		$t1,($t0)		
		sb		$t1,($t0)
		li		$t0,-1			
		bgez	$t0,loop	
		slt		$t3,$t0,$t1	
		sltu	$t3,$t0,$t1	
		slti	$t3,$t0,0x1	
		sltiu	$t3,$t0,0x1	
		li		$t0,5
		blez	$t0,loop		
		jal		loop	
		jr		$s0				 		
loop: 
		jalr	$s0,$ra
		nop
		end:	j end					
		
	.data

var:	.word	100	