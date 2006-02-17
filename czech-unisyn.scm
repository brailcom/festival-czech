;;; Czech UniSyn based voice example definition

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


;; Since there is currently no Czech UniSyn diphone database available, this
;; file serves just as an example definition.


(require 'czech)


(define (czech-unisyn-dirname name)
  (substring name 0 (- (length name) (+ 1 (length (basename name))))))

(define (czech-unisyn-db-init name index-file)
  (if (not (member name (us_list_dbs)))
      (let ((lpc-dir (path-append (czech-unisyn-dirname
                                   (czech-unisyn-dirname index-file))
                                  "lpc")))
        (us_diphone_init
          (list (list 'name name)
                (list 'index_file index-file)
                (list 'grouped "false")
                (list 'coef_dir lpc-dir)
                (list 'sig_dir lpc-dir)
                (list 'coef_ext ".lpc")
                (list 'sig_ext ".res")
                (list 'default_diphone "#-#"))))))

(define (czech-unisyn-group-db-init name group-file)
  (if (not (member name (us_list_dbs)))
      (us_diphone_init
        (list (list 'name name)
              (list 'index_file group-file)
              (list 'grouped "true")
              (list 'default_diphone "#-#")))))

(define (czech-unisyn-param-init)
  (set! us_abs_offset 0.0)
  (set! window_factor 1.0)
  (set! us_rel_offset 0.0)
  (set! us_gain 0.9)
  (Parameter.set 'us_sigpr 'lpc))
  
(define (czech-unisyn-init name index-file)
  ((if (string-matches index-file ".*\.group$")
       czech-unisyn-group-db-init
       czech-unisyn-db-init)
   name index-file)
  (czech-unisyn-param-init)
  (us_db_select name))


;; Example definition
; (czech-proclaim-voice
;   foo
;   "Foo Czech voice."
;   (czech-unisyn-init 'czech_foo "/path/to/the/index/file"))


(provide 'czech-unisyn)
