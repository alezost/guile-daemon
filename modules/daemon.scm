;;; daemon.scm --- Main module for Guile-Daemon

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

;; This module provides main subroutines to start guile-daemon.

;;; Code:

(define-module (daemon)
  #:use-module (ice-9 match)
  #:use-module (system repl server)
  #:use-module (daemon ui)
  #:use-module (daemon utils)
  #:export (main))

;; FIFO file must be writable only by a user: the daemon evaluates an
;; arbitrary Guile code passed to the FIFO file, so it is extremely
;; dangerous to allow other users to write to this file.
(define %fifo-permissions #o600)

(define (load-config-file file-name)
  (if (file-exists? file-name)
      (catch #t
        (lambda ()
          (primitive-load file-name))
        (lambda (error . args)
          (print-error "Configuration file has not been loaded:")
          (apply display-error #f (current-error-port) args)))
      (print-error "Configuration file does not exist: ~a" file-name)))

(define (load-fifo-file file-name)
  (catch #t
    (lambda ()
      (primitive-load file-name))
    (lambda args
      (match args
        (('system-error args ...)
         (print-error "Something wrong with the FIFO file:")
         (apply display-error #f (current-error-port) args)
         (exit 1))
        ((error args ...)
         (print-error "The code from the FIFO file has not been loaded:")
         (apply display-error #f (current-error-port) args))))))

(define (read-eval-loop file-name)
  "Read and evaluate Guile code from FIFO FILE-NAME in a loop."
  (load-fifo-file file-name)
  (read-eval-loop file-name))

(define (suitable-fifo-file? file-name)
  "Return #t if FILE-NAME is FIFO file owned by the current user with
the right permissions."
  (catch #t
    (lambda ()
      (let ((s (stat file-name)))
        (and (= (stat:uid s) (getuid))
             (eq? (stat:type s) 'fifo)
             (= (stat:perms s) %fifo-permissions))))
    (const #f)))

(define (make-fifo-file-maybe file-name)
  "Make FIFO FILE-NAME if it does not exist."
  (unless (suitable-fifo-file? file-name)
    (catch #t
      (lambda ()
        (delete-file-maybe file-name)
        (ensure-directory (dirname file-name))
        (mknod file-name 'fifo %fifo-permissions 0))
      (lambda (error . args)
        (print-error "Couldn't create FIFO file ~a:" file-name)
        (apply display-error #f (current-error-port) args)
        (exit 1)))))

(define (start-server file-name)
  "Create socket file at FILE-NAME and spawn REPL server there."
  (catch #t
    (lambda ()
      (delete-file-maybe file-name)
      (ensure-directory (dirname file-name))
      (spawn-server (make-unix-domain-server-socket #:path file-name)))
    (lambda (error . args)
      (print-error "Couldn't start server over socket file ~a:"
                   file-name)
      (apply display-error #f (current-error-port) args))))


;;; Main

(define (main name . args)
  (let* ((opts   (parse-args args))
         (config (assoc-ref opts 'config-file))
         (fifo   (assoc-ref opts 'fifo-file))
         (socket (assoc-ref opts 'socket-file)))
    (load-config-file config)
    (start-server socket)
    (make-fifo-file-maybe fifo)
    (read-eval-loop fifo)))

;;; daemon.scm ends here
