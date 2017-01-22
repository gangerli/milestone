#!/usr/bin/gosh

(use file.util)
(load (string-append (current-directory) "/basic-lexer.scm"))

(define (main args)
  (let ((input (cadr args)))
  ;; If file, map to each line or try string, else throw error.
    (cond ((file-exists? input)
           (map (lambda (input-line)
                  (display (parse-string input-line)))
                (file->string-list (cadr args))))
          ((string? input) (display (parse-string input)))
          (else (error "Unrecognized input! Is it a quoted string or a file?")))))
                  
