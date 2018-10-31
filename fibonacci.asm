#####################################################################
# Alunos: Guilherme Sergei Schäfer e Gustavo Koefender
# E-mails: guilherme.schafer@acad.pucrs.br e gustavo.koefender@acad.pucrs.br
# Professor: César Marcon
# Programa para testar a sequencia de Fibonacci
#####################################################################


	.text
	.globl main
main:	
	la	$t0,val1	#Carregando os 
	lw	$t0,($t0)	#2 valores do 
	la	$t1,val2	#inicio do 
	lw	$t1,($t1)	#fibonacci
	la	$t2,vet		#Endereço do vetor
	la	$t3,tam		#Tamanho do vetor
	lw	$t3,($t3)	
	
	li	$t7,0		#Zerando contador
loop:	beq	$t7,$t3,end	#Condição para sair do loop
	addu	$t4,$t0,$t1	#somando os valores	
	move	$t0,$t1		#atualizando valores
	move	$t1,$t4		#para a proxima soma
	sw	$t4,($t2)	#guardando o valor somado
	addiu	$t2,$t2,4	#Próximo endereço
	addiu	$t7,$t7,1	#Acrescendo somador
	j	loop
	
end:	j 	end
	
	

	.data		
tam:	.word	20	
val1:	.word	0
val2:	.word	1
vet:	.word	0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 

