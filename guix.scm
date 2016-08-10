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
;; release and for the latest (more or less) development snapshot (i.e.,
;; for one of the latest commits of the guile-daemon git repository).
;; To build (install), run:
;;
;;   guix build --file=guix.scm
;;   guix package --install-from-file=guix.scm
;;
;; (this will build/install the development package).

;; Also you can use this file to make a development environment for
;; building Guile-Daemon:
;;
;;   guix environment --pure --load=guix.scm
;;   ./autogen.sh
;;   ./configure
;;   make

;;; Code:

(use-modules
 (guix packages)
 (guix download)
 (guix git-download)
 (guix licenses)
 (guix build-system gnu)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages pkg-config))

(define guile-daemon
  (package
    (name "guile-daemon")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s90h8qhblhhz4ahn3p5d573a24px6cdjq2w311ibpgwnsni4qvq"))))
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

(define guile-daemon-devel
  (let ((revision "1")
        (commit "e519f4448ee604af24d2a4c4c4ba207a5d2296ba"))
    (package
      (inherit guile-daemon)
      (version (string-append (package-version guile-daemon)
                              "-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url (string-append "git://github.com/alezost/"
                                          (package-name guile-daemon)
                                          ".git"))
                      (commit commit)))
                (file-name (string-append (package-name guile-daemon)
                                          "-" version "-checkout"))
                (sha256
                 (base32
                  "0aa333280s3gqdnr8xxnq9jsqbhxmq46ws9bj32vlx21b5v5ljcp"))))
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
