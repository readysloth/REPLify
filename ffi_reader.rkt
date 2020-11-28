#lang racket

(require string-util)
(require 2htdp/batch-io)
(require ffi/unsafe ffi/unsafe/define)


(provide read read-syntax)


(define (read in)
  (read-syntax #f in))


(define (read-syntax src in) 
  (define parsed-line (parse-line (read-line in)))
  (cond 
    [(list? parsed-line) (to-syntax (list (string->symbol (first parsed-line))
                                                          (first (rest parsed-line)))
                                    #f #f)]
    [(string? parsed-line) (to-syntax `(eval ,(string->symbol parsed-line)) #f #f)]))


(define (to-syntax v src ln)
  (datum->syntax #f v #f));(make-srcloc src ln)))


(define (parse-line line)
  (if (starts-with-char? line #\#)
    (parse-define line)
    line))


(define (parse-define line)
  (rest (regexp-match #px"#(use|header|fregex)\\s+(\\S+)" line)))
