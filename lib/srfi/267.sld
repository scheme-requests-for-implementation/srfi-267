;;; ; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; ;
;;; ; SPDX-License-Identifier: MIT
(define-library (srfi 267)
  (import (scheme base) (scheme case-lambda))
  (export raw-string-read-error? raw-string-write-error?
          read-raw-string
          read-raw-string-after-prefix
          write-raw-string can-delimit?)
  (begin
    (define raw-string-read-error (vector #f))
    (define (raw-string-read-error? x)
      (eq? raw-string-read-error x))
    (define raw-string-write-error (vector #f))
    (define (raw-string-write-error? x)
      (eq? raw-string-write-error x))
    (define read-raw-string-after-prefix
      (case-lambda
        (() (read-raw-string-after-prefix (current-input-port)))
        ((port)
         (define (read-char* location)
           (let ((ch (read-char port)))
             (if (eof-object? ch)
                 (raise raw-string-read-error)
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
             (read-raw (read-char* 'read)))))))
    (define read-raw-string
      (case-lambda
        (() (read-raw-string (current-input-port)))
        ((port)
         (unless (eqv? (peek-char port) #\#)
           (raise raw-string-read-error))
         (read-char port)
         (unless (eqv? (peek-char port) #\")
           (raise raw-string-read-error))
         (read-char port)
         (read-raw-string-after-prefix port))))
    (define (can-delimit? string delimiter)
      ;; This could be made significantly better.
      (call-with-port (open-input-string (string-append "#\""
                                                        delimiter
                                                        "\""
                                                        string
                                                        "\""
                                                        delimiter
                                                        "\""))
        (lambda (port)
          (equal? string (read-raw-string port)))))
    (define write-raw-string
      (case-lambda
        ((string delimiter) (write-raw-string string
                                              delimiter
                                              (current-output-port)))
        ((string delimiter port)
         (unless (can-delimit? string delimiter)
           (raise raw-string-write-error))
         (write-string "#\"" port)
         (write-string delimiter port)
         (write-string "\"" port)
         (write-string string port)
         (write-string "\"" port)
         (write-string delimiter port)
         (write-string "\"" port))))))