	lch %4 0
	lcl %4 35 		; n
	lch %3 0
	lcl %3 0		; current
	lch %2 0
	lcl %2 0		; numcalls
	lch %0 0
	lcl %0 1024		; nbytes to alloc for stack
	alloc %1 %0		; sp
	add %1 %1 %0		; move sp to end of mem region
	;; set up some common jumps (kind of annoying)
	lch %11 0
	lcl %11 $fib
	lch %12 0
	lcl %12 $print
	lch %13 0
	lcl %13 $loop
	lch %14 0
	lcl %14 $end
loop:
	add_i %5 %3 0		; n = current
	j %6 %11		; $fib
	j %6 %12		; $print
	add_i %3 %3 1 		; ++current
	gt %7 %3 %4		; current > input
	jez %0 %7 %13		; $loop
	j %0 %14		; $end
fib:
	add_i %2 %2 1		; ++numcalls
	gt_i %7 %5 1		; n > 1
	jez %0 %7 %6
	sub_i %1 %1 12		; push result, ra, temp
	store %6 %1 0
	store %5 %1 1
	;; fib (n-1)
	sub_i %5 %5 1		; --n
	j %6 %11		; $fib
	store %5 %1 2		; tmp
	load %5 %1 1
	;; fib (n-2)
	sub_i %5 %5 2
	j %6 %11		; $fib
	load %8 %1 2		; tmp
	add %5 %5 %8		; n += tmp
	;; return
	load %6 %1 0
	add_i %1 %1 12		; pop
	j %0 %6
print:
	print %10 %5		; print result
	putch_i %10 '\n'
	j %0 %6
end:
;; 	couti "function calls: "
	putch_i %10 '\n'	
	print %10 %2		; print numcalls
	putch_i %10 '\n'
	halt
