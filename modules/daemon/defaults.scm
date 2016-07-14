;;; defaults.scm --- Default file names for Guile-Daemon

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

;; This module provides variables to find default files and directories
;; for guile-daemon.

;;; Code:

(define-module (daemon defaults)
  #:use-module (srfi srfi-26)
  #:export (%default-config-directory
            %default-socket-directory
            %default-config-file
            %default-fifo-file
            %default-socket-file))

(define %default-config-directory
  ;; File name of the default configuration directory.
  (string-append (or (getenv "XDG_CONFIG_HOME")
                     (and=> (getenv "HOME")
                            (cut string-append <> "/.config")))
                 "/guile-daemon"))

(define %default-socket-directory
  ;; File name of the default directory for FIFO and socket files.
  ;; XXX Is there a better place?
  (string-append %default-config-directory "/run"))

(define %default-config-file
  ;; File name of the default user configuration.
  (string-append %default-config-directory "/init.scm"))

(define %default-fifo-file
  ;; File name of the default FIFO file.
  (string-append %default-socket-directory "/fifo"))

(define %default-socket-file
  ;; File name of the default socket file.
  (string-append %default-socket-directory "/socket"))

;;; defaults.scm ends here
