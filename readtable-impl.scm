;;;  SPDX-FileCopyrightText: 2026 Peter McGoron
;;; ;
;;; ; SPDX-License-Identifier: MIT

(cond-expand
 (chicken-5 (import r7rs))
 (else))

(cond-expand
 (chicken (load "lib/srfi/267.sld"))
 (else))

(import (srfi 267))

(cond-expand
  (chicken (import (chicken read-syntax))
           (set-sharp-read-syntax! #\" read-raw-string-after-prefix))
  (guile (read-hash-extend #\"
                           (lambda (_ port)
                             (read-raw-string-after-prefix port))))
  (else (error "your implementation is not supported")))

