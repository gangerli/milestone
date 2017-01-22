;;;;;;;;;;;;;;;;;;;;;;;
;; TOKEN DEFINITIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define-macro (define-token name delimiter . supers)
  ;; Create a class named "name" characterized by its delimiter and
  ;; a list of its supers that it will inherit slots from.
  ;; TODO: extend for more detailed (define-token) declarations. 
  `(define-class ,name ,supers
     ((delim :init-value ,delimiter)
      (name  :init-value ',name)
      (value :init-keyword :value :init-value #f))))

;; Token definitions
(define-token <string-tree> #\space)
(define-token <lex-token>   #\space <string-tree>)
(define-token <lex-word>    #\space <lex-token>)
(define-token <lex-quote>   #\"     <lex-token>)

(define-method write-object (obj <string-tree>) (current-output-port)
  ;; Use with (display <object>) to output object structure readily.
  ;; Since all tokens are subclasses of <syntax-tree>, the printing
  ;; behavior is inherited, so there is no need to specially format
  ;; printing behavior for each.
  (format #t "~a: ~a~%" (slot-ref obj 'name) (slot-ref obj 'value)))

;;;;;;;;;;;
;; PARSE ;;
;;;;;;;;;;;
(define *ignored-list* '(#\space #\,))

(define *escape-character* #\\)

(define (parse-token port delimiter)
  ;; Take port and delimiter and collect all non-delimiter characters
  ;; and return them as a string.
  (let ((buf '()))
    (do ((char (read-char port) (read-char port)))
        ((or (eof-object? (peek-char port))
             (if (not (char=? char *escape-character*))
                 (char=? (peek-char port) delimiter) #f))
         (push! buf char)
         (cond ((char=? delimiter #\") (push! buf (read-char port))))
         (list->string (reverse buf)))
      (cond ((char=? char *escape-character*)
             (push! buf char)
             (push! buf (read-char port)))
            (else (push! buf char))))))

(define (parse-string string)
  ;; Open an input port for the string passed as argument
  ;; For each token delimiter encountered, create an object
  ;; and put it in "result". After reaching EOF, create
  ;; <syntax-tree> object and return it.
  (let ((port (open-input-string string))
        (result '()))
    (while (not (eof-object? (peek-char port)))
           (let ((token (make (get-next-token port))))
             (set! (slot-ref token 'value)
                   (parse-token port (slot-ref token 'delim)))
             (push! result token)))
    (make <string-tree> :value (reverse result))))

;; UTILITIES
(define (get-next-token port)
  ;; Open port and discard non-consequential characters until
  ;; a delimiter is encountered. Current behavior: if the char
  ;; is a double quote, return <lex-quote> as next token type,
  ;; else default to <lex-word>.
  (let ((next-type <lex-word>))
    (cond ((member (peek-char port) *ignored-list*)
           (while (member (peek-char port) *ignored-list*)
                  (read-char port))
           (if (char=? (peek-char port) #\")
               (set! next-type <lex-quote>))))
    next-type))
