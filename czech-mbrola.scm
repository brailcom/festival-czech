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

(defvar czech-mbrola-phoneme_durations
  '(
    (#   0.25)
    (##  0.05)
    (a   0.05)
    (a:  0.125)
    (b   0.0752)
    (c   0.095)
    (c~  0.096)
    (ch  0.08)
    (d   0.05)
    (d~  0.07)
    (e   0.05)
    (e:  0.13)
    (f   0.08)
    (g   0.05)
    (h   0.05)
    (i   0.06)
    (i:  0.10)
    (j   0.06)
    (k   0.07)
    (l   0.05)
    (m   0.05)
    (n   0.05)
    (n~  0.05)
    (o   0.05)
    (o:  0.1)
    (p   0.05)
    (r   0.05)
    (r~  0.05)
    (s   0.05)
    (s~  0.05)
    (t   0.08)
    (t~  0.062)
    (u   0.06)
    (u:  0.12)
    (v   0.05)
    (z   0.07)
    (z~  0.05)
    (dz  0.05)
    (dz~ 0.07)
    ))

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
 (set! czech-description czech-mbrola-description)
 (set! czech-phoneme_durations czech-mbrola-phoneme_durations))

(provide 'czech-mbrola)
