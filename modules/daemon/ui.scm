;;; ui.scm --- User interface module for Guile-Daemon

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

;; This module provides subroutines to parse command-line arguments for
;; guile-daemon.

;;; Code:

(define-module (daemon ui)
  #:use-module (srfi srfi-37)
  #:use-module (daemon config)
  #:use-module (daemon defaults)
  #:use-module (daemon utils)
  #:export (show-help
            show-version
            parse-args))

(define (show-help)
  (display "Usage: guile-daemon [OPTIONS...]
Run Guile Daemon.")
  (display "\n
Options:
  -h, --help            display this help and exit
  -V, --version         display version information and exit
  -c, --config=FILE     load startup configuration from FILE
  -f, --fifo=FILE       use fifo FILE
  -s, --socket=FILE     use socket FILE
"))

(define (show-version)
  (print-output "~a ~a" %package-name %version)
  (print-output "Copyright (C) ~a Alex Kost
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law."
                %copyright-year))

(define %default-options
  `((config-file . ,%default-config-file)
    (fifo-file   . ,%default-fifo-file)
    (socket-file . ,%default-socket-file)))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda _
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f  ; 'v' is for 'verbose'
                (lambda _
                  (show-version)
                  (exit 0)))
        (option '(#\c "config") #t #f
                (lambda (opt name arg seed)
                  (alist-replace 'config-file arg seed)))
        (option '(#\f "fifo") #t #f
                (lambda (opt name arg seed)
                  (alist-replace 'fifo-file arg seed)))
        (option '(#\s "socket") #t #f
                (lambda (opt name arg seed)
                  (alist-replace 'socket-file arg seed)))))

(define (parse-args args)
  "Return alist of options from command-line ARGS."
  (args-fold args %options
             (lambda (opt name arg seed)
               (print-error "Unrecognized option: ~a" name)
               seed)
             (lambda (arg seed)
               (print-error "Unneeded argument: ~a" arg)
               seed)
             %default-options))

;;; ui.scm ends here
