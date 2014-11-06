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
		(path->string basepath))
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

;; Define symbol table hash
(define symbol-table (make-hash))

;; Define variable table hash
(define var-table (make-hash))

;; Program file read function
(define (readlist-from-inputfile filename)
	(let ((inputfile (open-input-file filename)))
		 (if (not (input-port? inputfile))
			 (die `(,*run-file* ": " ,filename ": open failed"))
			 (let ((program (read inputfile)))
				  (close-input-port inputfile)
						 program))))

;; Statement label hash function
(define (label-hash label stmt)
	(if (null? stmt)
		(hash-set! symbol-table label stmt)
		(hash-set! symbol-table label (car stmt))
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

;; Print subroutine
(define (print-stmt printable)
	(printf "~s~n" (car printable))
)

;; Goto subroutine
(define (goto-stmt label)
	(printf "goto: ~s~n" (car label))
)

;; Statement execute function
(define (exe-statement stmt)
	(if (null? stmt)
		stmt
		(let ((stmt-type (car stmt))
			(stmt-args (cdr stmt)))
			(cond
			[(equal? stmt-type 'dim) ('dim)]
			[(equal? stmt-type 'let) ('let)]
			[(equal? stmt-type 'goto) (goto-stmt stmt-args)]
			[(equal? stmt-type 'if) ('if)]
			[(equal? stmt-type 'print) (print-stmt stmt-args)]
			[(equal? stmt-type 'input) ('input)]
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
					(let* ((label stmt-val) (stmt (hash-ref symbol-table label)))
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
(define (print-symbol-table program)
	(map (lambda (line)
		(let ((stmt (cdr line)))
			(if (null? stmt)
				stmt
				(if (hash-has-key? symbol-table (car stmt))
					(printf "~s: [~s] --> ~s~n" 
					(hash-ref label-linenr (car stmt))
					(car stmt)
					(hash-ref symbol-table (car stmt)))
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
			(print-symbol-table program)
		)
	)
)

;; Call main function
(main (vector->list (current-command-line-arguments)))
