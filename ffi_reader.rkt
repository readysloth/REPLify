#lang racket

(require string-util)
(require 2htdp/batch-io)
(require ffi/unsafe ffi/unsafe/define)


(provide (rename-out [ffi-read read]
                     [ffi-read-syntax read-syntax]))


(define (ffi-read in)
  (ffi-read-syntax #f in))


(define (ffi-read-syntax src in) 
  (define lines (port->lines in))
  (datum->syntax #f
    `(module ffi-script racket
    ,@(map (lambda (line)
              (define parsed-line (parse-line line))
              (cond 
                [(list? parsed-line) (list (string->symbol (first parsed-line))
                                           (first (rest parsed-line)))]
                [(string? parsed-line) `(eval ,(string->symbol parsed-line))]))
            lines))))


(define (to-syntax v src ln)
  (datum->syntax #f v #f));(make-srcloc src ln)))


(define (parse-line line)
  (if (starts-with-char? line #\#)
    (parse-define line)
    line))


(define (parse-define line)
  (rest (regexp-match #px"#(use|header|fregex)\\s+(\\S+)" line)))

