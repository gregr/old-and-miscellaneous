	lch %11 0
	lcl %11 $print
	;; alloc some memory
	lch %0 2
	lcl %0 0
	alloc %1 %0

	;; write null-terminated string data to memory
	lch %2 0
	lcl %2 0

	;; endient-independent
	add_i %3 %1 0
	lcl %2 'e'
	storeb %2 %3 0
	add_i %3 %3 1
	lcl %2 'f'
	storeb %2 %3 0
	add_i %3 %3 1
	lcl %2 'g'
	storeb %2 %3 0
	add_i %3 %3 1
	lcl %2 0
	storeb %2 %3 0
	add_i %3 %3 1
	
;; 	;; endian-dependent
;; 	add_i %2 %2 'd'
;; 	shl_i %2 %2 8		
;; 	add_i %2 %2 'c'
;; 	shl_i %2 %2 8
;; 	add_i %2 %2 'b'
;; 	shl_i %2 %2 8
;; 	add_i %2 %2 'a'
;; 	store %2 %1 0

	;; print it
	j %6 %11		; $print
	putch_i %10 '\n'
	halt
print:
	loadb %9 %1 0
	jez %0 %9 %6		; if (*s == 0) return;
	putch %10 %1		; putch(*s);
	add_i %1 %1 1		; ++s;
	j %0 %11		; loop
