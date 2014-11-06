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

;; Define symbol table hash
(define symbol-table (make-hash))

;; Statement label hash function
(define (label-hash label stmt)
	(if (null? stmt)
		(hash-set! symbol-table label stmt)
		(hash-set! symbol-table label (car stmt))
	)
)

;; Symbol table load function
(define (load-symbol-table program)
	(map (lambda (line)
		(let ((stmt (cdr line)))
			(if (null? stmt)
				stmt
				(if (symbol? (car stmt)) 
					(label-hash (car stmt) (cdr stmt))
					stmt
				)
			)
		))
		program
	)
)

;; Program statement read function
(define (readlist-from-inputfile filename)
	(let ((inputfile (open-input-file filename)))
		 (if (not (input-port? inputfile))
			 (die `(,*run-file* ": " ,filename ": open failed"))
			 (let ((program (read inputfile)))
				  (close-input-port inputfile)
						 program))))

;; Print subroutine
(define (print-stmt printable)
	(printf "~s~n" (car printable))
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
			[(equal? stmt-type 'goto) (print-stmt stmt-args)]
			[(equal? stmt-type 'if) ('if)]
			[(equal? stmt-type 'print) (print-stmt stmt-args)]
			[(equal? stmt-type 'input) ('input)]
			)
		)
	)
)

;; Program loop function
(define (run-program program)
	(map (lambda (line)
		(let ((stmt (cdr line)))
			(if (null? stmt)
				stmt
				(if (symbol? (car stmt))
					;; Need check for label --> null
					(exe-statement (car (cdr stmt)))
					(exe-statement (car stmt))
				)
			)
		))
		program
	)
)

;; Symbol table print function
(define (print-symbol-table program)
	(map (lambda (line)
		(let ((stmt (cdr line)))
			(if (null? stmt)
				stmt
				(if (hash-has-key? symbol-table (car stmt))
					(printf "[~s] --> ~s~n" (car stmt) 
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
			  (load-symbol-table program)
			  (run-program program)
			  (print-symbol-table program))))

;; Call main function
(main (vector->list (current-command-line-arguments)))
