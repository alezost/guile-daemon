;;; utils.scm --- General utilities for Guile-Daemon

;; Copyright © 2016 Alex Kost <alezost@gmail.com>

;; This file is part of Guile-Daemon.

;; Guile-Daemon is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Guile-Daemon is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Guile-Daemon.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides miscellaneous utilities for guile-daemon.

;;; Code:

(define-module (daemon utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (alist-replace
            delete-file-maybe
            mkdir-with-parents
            ensure-directory
            print-output
            print-error))

(define-syntax-rule (print-output format-string args ...)
  "Write some text and a newline to stdout using 'format'."
  (begin
    (format #t format-string args ...)
    (newline)))

(define-syntax-rule (print-error format-string args ...)
  "Write some text and a newline to stderr using 'format'."
  (begin
    (format (current-error-port) format-string args ...)
    (newline)))

(define* (alist-replace key value alist #:optional (key-eq? eq?))
  "Remove KEY elements from alist and add KEY/VALUE pair instead."
  (alist-cons key value
              (alist-delete key alist key-eq?)))

;; Originates from Guix: 'mkdir-p' from (guix build utils) module.
(define (mkdir-with-parents directory)
  "Create DIRECTORY and all its ancestors."
  (let ((not-slash (char-set-complement (char-set #\/))))
    (let loop ((components (string-tokenize directory not-slash))
               (root (if (string-prefix? "/" directory) "" ".")))
      (match components
        ((head tail ...)
         (let ((file (string-append root "/" head)))
           (unless (file-exists? file)
             (mkdir file))
           (loop tail file)))
        (_ #t)))))

(define (ensure-directory directory)
  "Create DIRECTORY if it does not exist."
  (unless (file-exists? directory)
    (mkdir-with-parents directory)))

(define (delete-file-maybe file-name)
  "Delete file with FILE-NAME if it exists."
  (when (file-exists? file-name)
    (delete-file file-name)))

;;; utils.scm ends here
