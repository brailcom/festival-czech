;;; Czech UniSyn based voice example definition

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


;; Since there is currently no Czech UniSyn diphone database available, this
;; file serves just as an example definition.


(require 'czech)


(define (czech-default-synthesis-init)
  (set! us_abs_offset 0.0)
  (set! window_factor 1.0)
  (set! us_rel_offset 0.0)
  (set! us_gain 0.9)
  (Parameter.set 'Synth_Method 'UniSyn)
  (Parameter.set 'us_sigpr 'lpc)
  (us_db_select 'czech)
  nil)

(defvar czech-index_file nil)

(if czech-index_file
    (us_diphone_init
     (list (list 'name "czech")
           (list 'index_file czech-index_file)
           (list 'grouped "false")
           (list 'coef_ext ".lpc")
           (list 'sig_ext ".res")
           (list 'default_diphone "#-#"))))

(defvar czech-default-unisyn-description
  '((phoneset-translation nil)
    (synthesis-method UniSyn)
    (synthesis-init czech-default-synthesis-init)))

(czech-proclaim-voice
 unisyn_default
 "Default Czech UniSyn voice."
 (set! czech-description czech-default-unisyn-description))

(provide 'czech-unisyn)
