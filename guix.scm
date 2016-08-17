;;; guix.scm --- Guix package for Guile-Daemon

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

;; This file contains 2 Guix packages for Guile-Daemon: for the latest
;; release and for the development snapshot (i.e., for the current
;; commit of the git checkout).  To build or install, run:
;;
;;   guix build --file=guix.scm
;;   guix package --install-from-file=guix.scm
;;
;; (this will build/install the development package using the current
;; git checkout directory).

;; Also you can use this file to make a development environment for
;; building Guile-Daemon:
;;
;;   guix environment --pure --load=guix.scm
;;   ./autogen.sh
;;   ./configure
;;   make

;;; Code:

(use-modules
 (ice-9 match)
 (ice-9 popen)
 (ice-9 rdelim)
 (srfi srfi-1)
 (srfi srfi-26)
 (guix gexp)
 (guix packages)
 (guix download)
 (guix git-download)
 (guix licenses)
 (guix build utils)
 (guix build-system gnu)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages pkg-config))

(define %source-dir (dirname (current-filename)))


;;; Package for the latest release

(define guile-daemon
  (package
    (name "guile-daemon")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wsq9l6a4sijq4i1r3kcddfaznsak2jc5k59gzkhs5il5d2kn5yi"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.0)))
    (home-page "https://github.com/alezost/guile-daemon")
    (synopsis "Evaluate code in a running Guile process")
    (description
     "Guile-Daemon is a small Guile program that loads your initial
configuration file, and then reads and evaluates Guile expressions that
you send to a FIFO file.")
    (license gpl3+)))


;;; Git checkout and development package

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-port port)
      (string-trim-right output #\newline))))

(define (git-files)
  "Return a list of all git-controlled files."
  (string-split (git-output "ls-files") #\newline))

(define git-file?
  (let ((files (git-files)))
    (lambda (file stat)
      "Return #t if FILE is the git-controlled file in '%source-dir'."
      (match (stat:type stat)
        ('directory #t)
        ((or 'regular 'symlink)
         (any (cut string-suffix? <> file) files))
        (_ #f)))))

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
                          #:select? git-file?))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'autogen
             (lambda _ (zero? (system* "sh" "autogen.sh")))))))
      (native-inputs
       (append (package-native-inputs guile-daemon)
               `(("autoconf" ,autoconf)
                 ("automake" ,automake)))))))

guile-daemon-devel

;;; guix.scm ends here
