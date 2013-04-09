	loadi %n 7
	loadi %one 1
	loadi %val 1
loop:
	gt %test %n %one
	jezi %ra %test $end
	mul %val %val %n
	subi %n %n 1
	jumpi %ra $loop
end:
	cout %val
	couti '\n'
	halt
