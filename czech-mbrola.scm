;;; Czech support for Festival Mbrola output module

;; Copyright (C) 2003 Brailcom, o.p.s.

;; Author: Milan Zamazal <pdm@brailcom.org>

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.


(require 'czech)
(require 'mbrola)

(defvar czech-mbrola-translations
  '(("#"   "_")
    ("c"   "ts")
    ("ch"  "x")
    ("c~"  "tS")
    ("d~"  "d'")
    ("dz~" "dZ")
    ("h"   "h\\")
    ("n~"  "n'")
    ("r~"  "r'")
    ("s~"  "S")
    ("t~"  "t'")
    ("z~"  "Z")))

(defvar czech-mbrola_database nil)

(define (czech-mbrola-init)
  (if (not czech-mbrola_database)
      (error "czech-mbrola_database variable not set"))
  (set! mbrola_database czech-mbrola_database)
  nil)

(defvar czech-mbrola-description
  (list (list 'phoneset-translation czech-mbrola-translations)
        (list 'synthesis-method 'MBROLA_Synth)
        (list 'synthesis-init czech-mbrola-init)))

(czech-proclaim-voice
 mbrola_cz2
 "Czech voice provided by the Mbrola cz2 database."
 (set! czech-description czech-mbrola-description))

(provide 'czech-mbrola)
