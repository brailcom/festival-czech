;;; Create lexicon entries from a word list by lts rules

;; Copyright (C) 2004 Brailcom, o.p.s.

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


(define (dump-pronounciation in-file out-file)
  (let ((in (fopen in-file "r"))
        (out (fopen out-file "w"))
        (done nil))
    (while (not done)
      (let ((word (readfp in)))
        (print word)
        (if (consp word)                ; eof
            (set! done t)
            (format out "(%l nil ((%l 0)))\n" word
                    (czech-basic-lts word nil)))))
    (fclose out)
    (fclose in)))


(provide 'dump-pronounciation)
