	loadi %last 24
	loadi %current 0
	loadi %calls 0
	loadi %one 1
	loadi %zero 0
	loadi %sp 0
loop:
	addi %val %current 0
	jumpi %ra $fib
	jumpi %ra $print
	addi %current %current 1
	gt %test %current %last
	jezi %null %test $loop
	jumpi %null $end
fib:
	addi %calls %calls 1
	gt %test %val %one
	jez %null %test %ra
	subi %sp %sp 3		; push %val, %ra, %tmp
	store %ra %sp 0
	store %val %sp 1
	;; fib (n-1)
	subi %val %val 1
	jumpi %ra $fib
	store %val %sp 2	; tmp
	load %val %sp 1
	;; fib (n-2)
	subi %val %val 2
	jumpi %ra $fib
	load %tmp %sp 2		; tmp
	add %val %val %tmp
	;; return
	load %ra %sp 0
	addi %sp %sp 3		; pop
	jump %null %ra
print:
	cout %val
	couti '\n'
	jump %null %ra
end:
	couti "function calls: "
	cout %calls
	couti '\n'
	halt
