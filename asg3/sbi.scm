#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; Author: Adam Henry, adlhenry@ucsc.edu"
;; $Id: sbi.scm,v 1.1 2014-11-03 11:16:32-08 - - $

;; Stderr atom
(define *stderr* (current-error-port))

;; Program name atom
(define *run-file*
	(let-values
		(((dirpath basepath root?)
			(split-path (find-system-path 'run-file))))
		(path->string basepath)
	)
)

;; Warning display function
(define (die list)
	(for-each (lambda (item) (display item *stderr*)) list)
	(newline *stderr*)
	(exit 1)
)

;; Usage warning function
(define (usage-exit)
	(die `("Usage: " ,*run-file* " filename"))
)

;; Define the program counter
(define PC 1)

;; Define statement list size
(define size 0)

;; Define statement list hash
(define stmt-list (make-hash))

;; Define label line-number hash
(define label-linenr (make-hash))

;; Define label table hash
(define label-table (make-hash))

;; Define symbol table hash
(define symbol-table (make-hash))

;; Initialize built-in symbols
(hash-set! symbol-table 'abs (lambda (num) (abs num)))
(hash-set! symbol-table 'acos (lambda (num) (acos num)))
(hash-set! symbol-table 'asin (lambda (num) (asin num)))
(hash-set! symbol-table 'atan (lambda (num) (atan num)))
(hash-set! symbol-table 'cos (lambda (num) (cos num)))
(hash-set! symbol-table 'sin (lambda (num) (sin num)))
(hash-set! symbol-table 'tan (lambda (num) (tan num)))
(hash-set! symbol-table 'exp (lambda (num) (exp num)))
(hash-set! symbol-table 'floor (lambda (num) (floor num)))
(hash-set! symbol-table 'log (lambda (num) (log num)))
(hash-set! symbol-table 'round (lambda (num) (round num)))
(hash-set! symbol-table 'sqrt (lambda (num) (sqrt num)))
(hash-set! symbol-table 'ceil (lambda (num) (ceiling num)))
(hash-set! symbol-table 'trunc (lambda (num) (truncate num)))
(hash-set! symbol-table 'log10 (lambda (num) (/ (log num) (log 10))))
(hash-set! symbol-table 'log2 (lambda (num) (/ (log num) (log 2))))
(hash-set! symbol-table 'pi pi)
(hash-set! symbol-table 'e (exp 1))

;; Program file read function
(define (readlist-from-inputfile filename)
	(let ((inputfile (open-input-file filename)))
		(if (not (input-port? inputfile))
			(die `(,*run-file* ": " ,filename ": open failed"))
			(let ((program (read inputfile)))
				(close-input-port inputfile)
				program
			)
		)
	)
)

;; Statement label hash function
(define (label-hash label stmt)
	(if (null? stmt)
		(hash-set! label-table label stmt)
		(hash-set! label-table label (car stmt))
	)
)

;; Program load function
(define (load-program program)
	(let ((index 0))
		(map (lambda (line)
			(let ((stmt (cdr line)))
				(if (null? stmt)
					stmt
					(let ((stmt-val (car stmt)))
						(set! index (+ index 1))
						(hash-set! stmt-list index stmt-val)
						(if (symbol? stmt-val)
							(let ((label (car stmt)))
								(hash-set! label-linenr label index)
								(label-hash label (cdr stmt))
							)
							stmt-val
						)
					)
				)
			))
			program
		)
		(set! size index)
	)
)

;; Expression evaluate function
(define (eval-expr expr)
	(if (or (null? expr) (number? expr) (symbol? expr))
		(cond
			[(symbol? expr) (hash-ref symbol-table expr)]
			[(equal? expr 0) 0.0]
			[else expr]
		)
		(if (null? (cdr (cdr expr)))
			(let ((op (car expr))
				(expr1 (car (cdr expr))))
				(cond
					[(equal? op '+) (+ (eval-expr expr1))]
					[(equal? op '-) (- (eval-expr expr1))]
					[else ((hash-ref symbol-table op) (eval-expr expr1))]
				)
			)
			(let ((op (car expr))
				(expr1 (car (cdr expr)))
				(expr2 (car (cdr (cdr expr)))))
				(cond
					[(equal? op '+) (+ (eval-expr expr1) (eval-expr expr2))]
					[(equal? op '-) (- (eval-expr expr1) (eval-expr expr2))]
					[(equal? op '*) (* (eval-expr expr1) (eval-expr expr2))]
					[(equal? op '/) (/ (eval-expr expr1) (eval-expr expr2))]
					[(equal? op '%) (modulo (eval-expr expr1) (eval-expr expr2))]
					[(equal? op '^) (expt (eval-expr expr1) (eval-expr expr2))]
				)
			)
		)
	)
)

;; Dim subroutine
(define (dim-stmt array)
	(printf "dim: ~s~n" array)
)

;; Let subroutine
(define (let-stmt mem-expr)
	(let ((symbol (car mem-expr)) (expr (car (cdr mem-expr))))
		(hash-set! symbol-table symbol (eval-expr expr))
	)
)

;; Goto subroutine
(define (goto-stmt label)
	(if (hash-has-key? label-table (car label))
		(set! PC (hash-ref label-linenr (car label)))
		(die `(,*run-file* ": goto: " ,label " undefined"))
	)
)

;; If subroutine
(define (if-stmt op-label)
	(printf "if: ~s~n" op-label)
)

;; Print subroutine
(define (print-stmt printable)
	(if (null? printable)
		(printf "~n")
		(let ((expr (car printable)) (rest (cdr printable)))
			(if (string? expr)
				(printf "~a" expr)
				(printf "~a" (eval-expr expr))
			)
			(print-stmt rest)
		)
	)
)

;; Input subroutine
(define (input-stmt memory)
	(printf "input: ~s~n" memory)
)

;; Statement execute function
(define (exe-statement stmt)
	(if (null? stmt)
		stmt
		(let ((stmt-type (car stmt))
			(args (cdr stmt)))
			(cond
				[(equal? stmt-type 'dim) (dim-stmt args)]
				[(equal? stmt-type 'let) (let-stmt args)]
				[(equal? stmt-type 'goto) (goto-stmt args)]
				[(equal? stmt-type 'if) (if-stmt args)]
				[(equal? stmt-type 'print) (print-stmt args)]
				[(equal? stmt-type 'input) (input-stmt args)]
			)
		)
	)
)

;; Program execute function
(define (run-program)
	(let ((prgm-counter PC))
		(if (> PC size)
			PC
			(let ((stmt-val (hash-ref stmt-list PC)))
				(if (symbol? stmt-val)
					(let* ((label stmt-val) (stmt (hash-ref label-table label)))
						(if (null? stmt)
							stmt
							(exe-statement stmt)
						)
					)
					(exe-statement stmt-val)
				)
				(if (= PC prgm-counter)
					(set! PC (+ PC 1))
					PC
				)
				(run-program)
			)
		)
	)
)

;; Symbol table print function
(define (print-label-table program)
	(map (lambda (line)
		(let ((stmt (cdr line)))
			(if (null? stmt)
				stmt
				(if (hash-has-key? label-table (car stmt))
					(printf "~s: [~s] --> ~s~n" 
					(hash-ref label-linenr (car stmt))
					(car stmt)
					(hash-ref label-table (car stmt)))
					(car stmt)
				)
			)
		))
		program
	)
)

;; Program main function
(define (main arglist)
	(if (or (null? arglist) (not (null? (cdr arglist))))
		(usage-exit)
		(let* ((sbprogfile (car arglist))
			(program (readlist-from-inputfile sbprogfile)))
			(load-program program)
			(run-program)
		)
	)
)

;; Call main function
(main (vector->list (current-command-line-arguments)))
