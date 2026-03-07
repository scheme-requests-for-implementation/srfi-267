; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define (read-raw-string port)
  ;; This parser starts reading after `"`.
  ;; In the given examples, the parser starts at the dot:
  ;;
  ;; #"."asdf""
  ;; #".--"#"()""--"
  (define (read-char* location)
    (let ((ch (read-char port)))
      (if (eof-object? ch)
          (error (list "eof in raw string literal" location))
          ch)))
  (define delimiter
    (do ((ch (read-char* 'delim) (read-char* 'delim))
         (acc '(#\") (cons ch acc)))
        ((char=? ch #\")
         (reverse (cons #\" acc)))))
  (call-with-port (open-output-string)
    (lambda (out)
      (define (read-delimiter n rest-of-delimiter)
        (if (null? rest-of-delimiter)
            (get-output-string out)
            (let ((ch (read-char* 'check)))
              (if (char=? ch (car rest-of-delimiter))
                  (read-delimiter (+ n 1) (cdr rest-of-delimiter))
                  (do ((n n (- n 1))
                       (delimiter delimiter (cdr delimiter)))
                      ((zero? n) (read-raw ch))
                    (write-char (car delimiter) out))))))
      (define (read-raw ch)
        (if (char=? ch (car delimiter))
            (read-delimiter 1 (cdr delimiter))
            (begin (write-char ch out)
                   (read-raw (read-char* 'read)))))
      (read-raw (read-char* 'read)))))

(cond-expand
  (chicken (import (chicken read-syntax))
           (set-sharp-read-syntax! #\" read-raw-string))
  (guile (read-hash-extend #\"
                           (lambda (_ port)
                             (read-raw-string port))))
  (else (error "your implementation is not supported")))

(define (test x y)
  (display (string=? x y)) (newline))

(test "" #"""")
(test "a" #""a"")
(test "\\" #""\"")
(test "\"" #"-"""-")
(test "\\\"" #"-"\""-")
(test "#\"()\"" #"-"#"()""-")
(test "#\"\"a\"\"" #"-"#""a"""-")
(test "ends with \\\"" #"-"ends with \""-")
(test "multiline\nstring" #""multiline
string"")
(test "\n    no whitespace stripping" #""
    no whitespace stripping"")
