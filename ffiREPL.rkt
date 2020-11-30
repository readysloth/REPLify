#lang racket

(require string-util)
(require 2htdp/batch-io)
(require ffi/unsafe ffi/unsafe/define)


; reader
(provide (rename-out [ffi-read read]
                     [ffi-read-syntax read-syntax]))


(define (ffi-read in)
  (ffi-read-syntax #f in))


(define (ffi-read-syntax src in) 
  (define lines (port->lines in))
  (datum->syntax #f
    (filter identity
      `(module ffi-script "ffiREPL.rkt"
        (require ffi/unsafe ffi/unsafe/define)
        ,@(map (lambda (line)
                  (define parsed-line (parse-line line))
                  (cond 
                    [(list? parsed-line) (append
                                           (list (string->symbol (first parsed-line)))
                                           (filter identity (rest parsed-line)))]
                    [(string? parsed-line) (if (regexp-match #px"^\\s*$" parsed-line)
                                               #f
                                               (string->symbol parsed-line))]))
                lines)))))


(define (to-syntax v src ln)
  (datum->syntax #f v #f));(make-srcloc src ln)))


(define (parse-line line)
  (if (starts-with-char? line #\#)
    (parse-define line)
    line))


(define (parse-define line)
  (rest (regexp-match #px"#(use|header)\\s+(\\S+)(?:\\s+(\\S+))?" line)))


; expander
(require syntax/wrap-modbeg)

(provide (rename-out [ffi-module-begin #%module-begin])
         require #%top-interaction #%datum
         header use)

(define-syntax-rule (ffi-module-begin EXPR ...)
  (#%module-begin
   EXPR ...))

(define-syntax-rule (use lib)          (define-ffi-definer define-lib-func (ffi-lib lib)))
(define-syntax-rule (header hdr regex) (get-functions-from-header hdr (pregexp regex)))

(define (get-functions-from-header header-path regex)
  (filter (lambda (l) (regexp-match regex l)) 
          (read-lines header-path)))
