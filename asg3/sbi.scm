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

;; Symbol table load function
(define (load-symbol-table program)
	(map (lambda (line) (printf "~s --> ~s~n"
	(car line) (cdr line))) program))

;; Program statement read function
(define (readlist-from-inputfile filename)
	(let ((inputfile (open-input-file filename)))
		 (if (not (input-port? inputfile))
			 (die `(,*run-file* ": " ,filename ": open failed"))
			 (let ((program (read inputfile)))
				  (close-input-port inputfile)
						 program))))

;; Debug print function
(define (write-program-by-line filename program)
	(printf "==================================================~n")
	(printf "~a: ~s~n" *run-file* filename)
	(printf "==================================================~n")
	(printf "(~n")
	(map (lambda (line) (printf "~s --> ~s~n"
	(car line) (cdr line))) program)
	(printf ")~n"))

;; Program main function
(define (main arglist)
	(if (or (null? arglist) (not (null? (cdr arglist))))
		(usage-exit)
		(let* ((sbprogfile (car arglist))
			   (program (readlist-from-inputfile sbprogfile)))
			  (write-program-by-line sbprogfile program))))

;; Call main function
(main (vector->list (current-command-line-arguments)))
