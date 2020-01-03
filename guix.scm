;;; guix.scm --- Guix package for Guile-Daemon

;; Copyright © 2016–2017, 2020 Alex Kost <alezost@gmail.com>

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

;; This file contains Guix package for development version of
;; Guile-Daemon.  To build or install, run:
;;
;;   guix build --file=guix.scm
;;   guix package --install-from-file=guix.scm

;; The main purpose of this file though is to make a development
;; environment for building Guile-Daemon:
;;
;;   guix environment --pure --load=guix.scm
;;   ./autogen.sh
;;   ./configure
;;   make

;;; Code:

(use-modules
 (ice-9 popen)
 (ice-9 rdelim)
 (guix gexp)
 (guix packages)
 (guix git-download)
 (guix build utils)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages pkg-config)
 (gnu packages texinfo))

(define %source-dir (dirname (current-filename)))

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-pipe port)
      (string-trim-right output #\newline))))

(define (current-commit)
  (git-output "log" "-n" "1" "--pretty=format:%H"))

(define guile-daemon-devel
  (let ((commit (current-commit)))
    (package
      (inherit guile-daemon)
      (version (string-append (package-version guile-daemon)
                              "-" (string-take commit 7)))
      (source (local-file %source-dir
                          #:recursive? #t
                          #:select? (git-predicate %source-dir)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'autogen
             (lambda _ (zero? (system* "sh" "autogen.sh")))))))
      (native-inputs
       (append (package-native-inputs guile-daemon)
               `(("autoconf" ,autoconf)
                 ("automake" ,automake)
                 ("texinfo" ,texinfo)))))))

guile-daemon-devel

;;; guix.scm ends here
