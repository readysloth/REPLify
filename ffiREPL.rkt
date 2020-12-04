#lang racket

(require string-util
         2htdp/batch-io
         ffi/unsafe ffi/unsafe/define)


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
                                               (read (open-input-string parsed-line)))]))
                lines)))))


(define (to-syntax v src ln)
  (datum->syntax #f v #f));(make-srcloc src ln)))


(define (parse-line line)
  (if (starts-with-char? line #\#)
    (parse-define line)
    (regexp-replace #px";.*" line "")))


(define (parse-define line)
  (rest (regexp-match #px"#(use|header)\\s+(\\S+)(?:\\s+\"(.*)\")?" line)))


; expander
(require syntax/wrap-modbeg
         racket/syntax
         ffi/unsafe ffi/unsafe/define)


(provide (rename-out [ffi-module-begin #%module-begin])
         eval require #%top-interaction #%datum #%top #%app
         get-functions-from-header define-func
         header use)


(define-syntax-rule (ffi-module-begin EXPR ...)
  (#%module-begin
   EXPR ...))


(define-syntax-rule (use lib)
    (define-ffi-definer define-lib-func (ffi-lib lib)
                        #:default-make-fail (lambda (n)
                                              (displayln (format "Cannot load ~a" n)))))


(define-syntax-rule (header hdr regex)
    (map define-func
         (get-functions-from-header hdr (pregexp regex))))


(define (make-ffi-type any) (string->symbol (format "_~a" any)))


(define (define-func str)
  (match (parse-proto str)
    ([list name sign]
      (let
        ([rsign (map (lambda (t) (cond
                                   [(regexp-match #px"char\\s+\\*" t) "string"]
                                   [(regexp-match #px".+\\*" t) "pointer"]
                                   [else t]))
                     sign)]
         [rname (string->symbol name)])
         #`(define-lib-func #,rname
                           #,(append '(_fun)
                                    (filter (lambda(t) (not (eq? t '_void)))
                                              (map make-ffi-type (rest rsign))) '(->)
                                    (list (make-ffi-type (first rsign)))))))))



(define (get-functions-from-header header-path regex)
  (filter (lambda (l) (regexp-match regex l))
          (read-lines header-path)))


(define (parse-proto str)
  (define proto-regex (pregexp
                        (string-join (append (map
                              (lambda (t) (format "~s\\s*\\*|~s|" t t))
                              '(char short int long
                                float double bool size_t
                                FILE))
                            `(,(format "~s\\s*\\*|~s" 'void 'void)))
                          "")))
  (append (list (second (regexp-match #px"([a-zA-Z_][a-zA-Z_0-9]*)\\s*\\(" str))
                (map (lambda (t) (string-replace t "size_t" "size"))
                     (regexp-match* proto-regex str)))))
