;;; Czech support for Festival Mbrola output module

;; Copyright (C) 2003, 2004, 2005 Brailcom, o.p.s.

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
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.


(require 'czech)
(require 'mbrola)

(lts.ruleset
 czech-mbrola-lts
 ()
 (( [ z o: ] = z o )
  ;;;
  ( [ # ] = # )
  ( [ _ ] = _ )
  ( [ a ] = a )
  ( [ a: ] = a: )
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ c~ ] = c~ )
  ( [ ch ] = ch )
  ( [ ch* ] = ch* )
  ( [ d ] = d )
  ( [ d~ ] = d~ )
  ( [ e ] = e )
  ( [ e: ] = e: )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ i ] = i )
  ( [ i: ] = i: )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ n* ] = n* )
  ( [ n~ ] = n~ )
  ( [ o ] = o )
  ( [ o: ] = o: )
  ( [ p ] = p )
  ( [ r ] = r )
  ( [ r~ ] = r~ )
  ( [ r~* ] = r~* )
  ( [ s ] = s )
  ( [ s~ ] = s~ )
  ( [ t ] = t )
  ( [ t~ ] = t~ )
  ( [ u ] = u )
  ( [ u: ] = u: )
  ( [ v ] = v )
  ( [ z ] = z )
  ( [ z~ ] = z~ )
  ( [ dz ] = dz )
  ( [ dz~ ] = dz~ )))

(defvar czech-mbrola-translations
  '(("#"   "_")
    ("c"   "ts")
    ("ch"  "x")
    ("ch*" "x")
    ("c~"  "tS")
    ("d~"  "d'")
    ("dz~" "dZ")
    ("h"   "h\\")
    ("n*"  "n")
    ("n~"  "n'")
    ("o:"  "o")
    ("r~"  "r'")
    ("r~*"  "r'")
    ("s~"  "S")
    ("t~"  "t'")
    ("z~"  "Z")))

(defvar czech-mbrola-phoneme-durations
  '(
    (#   0.25)
    (_   0.01)
    (a   0.05)
    (a:  0.125)
    (b   0.0752)
    (c   0.095)
    (c~  0.096)
    (ch  0.08)
    (ch* 0.08)
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
    (n*  0.05)
    (n~  0.05)
    (o   0.05)
    (o:  0.1)
    (p   0.05)
    (r   0.05)
    (r~  0.05)
    (r~* 0.05)
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

(czech-proclaim-voice
 (mbrola_cz2 (gender male))
 "Czech voice provided by the Mbrola cz2 database."
 (set! czech-phoneset-translation* czech-mbrola-translations)
 (set! czech-phoneme-durations* czech-mbrola-phoneme-durations)
 (set! czech-lts-extra-rules* '(czech-mbrola-lts))
 (set! czech-volume-scale* 1.0)
 (set! czech-after-analysis-hooks* (list czech-translate-phonemes))
 (if czech-mbrola_database
     (set! mbrola_database czech-mbrola_database)
     (error "czech-mbrola_database variable not set"))
 (Param.set 'Synth_Method MBROLA_Synth))

(provide 'czech-mbrola)
