	lch %1 0
	lcl %1 7		; calculate factorial of this number
	lch %2 0
	lcl %2 1
loop:
	gt_i %3 %1 1
	lch %4 0
	lcl %4 $end
	jez %0 %3 %4 		; inconvenient to not have immediate jump, huh?
	mul %2 %2 %1
	sub_i %1 %1 1
	lch %4 0
	lcl %4 $loop
	j %0 %4			; damn right it is
end:
	print %0 %2		; %0 placeholder for a future channel
	putch_i %0 '\n'
	halt
