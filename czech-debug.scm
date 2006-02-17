;;; Miscellaneous debugging functions for Czech synthesis

;; Copyright (C) 2004, 2005 Brailcom, o.p.s.

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

;; Some data were created using the data files and tools contained in the
;; ispell-czech package available under GPL at
;; ftp://ftp.vslib.cz/pub/unix/ispell/czech.


(require 'czech)


(define (czech-debug-newline s)
  (format s "\n\n"))

(define (czech-debug-prompt s prompt)
  (format s "* %s:\n" prompt))
  
(define (czech-debug-print-relation s utt relation features)
  (czech-debug-prompt s relation)
  (let ((i (utt.relation.first utt relation)))
    (while i
      (format s "%s " (item.name i))
      (let ((feats '())
            (features* features))
        (while features*
          (let ((val (item.feat i (car features*))))
            (if (and (not (string-matches val "0?"))
                     (not (string-equal val "nil"))
                     (not (string-equal val "NB")))
                (set! feats (cons (cons (car features*) val) feats))))
          (set! features* (cdr features*)))
        (if feats
            (begin
              (format s "(")
              (mapcar (lambda (fv)
                        (if fv (format s " %s=%s " (car fv) (cdr fv))))
                      (reverse feats))
              (format s ") "))))
      (set! i (item.next i)))))

(define (czech-debug-print-randomization s utt)
  (format s "randomized = %s" czech-randomize))

(define (czech-debug-print-tokens s utt)
  (czech-debug-print-relation s utt 'Token '(punc prepunctuation)))

(define (czech-debug-print-words s utt)
  (czech-debug-print-relation s utt 'Word '(pbreak pos)))

(define (czech-debug-print-segments s utt)
  (czech-debug-print-relation s utt 'Segment '()))

(define (czech-debug-print-units s utt)
  (czech-debug-prompt s 'Units)
  (format s "||")
  (let ((i-unit (utt.relation.first utt 'IntStress)))
    (while i-unit
      (mapcar (lambda (s-unit)
                (format s " ")
                (mapcar (lambda (syl)
                          (mapcar (lambda (ph) (format s "%s " (item.name ph)))
                                  (item.relation.daughters syl 'SylStructure)))
                        (item.relation.daughters s-unit 'StressUnit))
                (format s "%s/%s %l |" (item.feat s-unit 'position)
                        (item.feat s-unit 'contourtype)
                        (mapcar (lambda (x) (* 100 x))
                                (item.feat s-unit 'contour))))
              (item.daughters i-unit))
      (format s "|")
      (set! i-unit (item.next i-unit)))))

(define (czech-debug-print-durfactors s utt)
  (czech-debug-prompt s "Duration factors")
  (let ((i (utt.relation.first utt 'Segment))
        (segs '())
        (last-dur 'none))
    (while i
      (while (and i (eqv? (item.feat i 'dur_factor) last-dur))
        (set! segs (cons (item.name i) segs))
        (set! i (item.next i)))
      (if segs
          (begin
            (format s "[")
            (mapcar (lambda (seg) (format s "%s " seg)) (reverse segs))
            (if (not (string-equal last-dur '0))
                (format s "= %s" last-dur))
            (format s "] ")
            (set! segs '())))
      (set! last-dur (and i (item.feat i 'dur_factor))))))

(define (czech-debug-print-durations s utt)
  (czech-debug-prompt s 'Duration)
  (let ((last-end 0))
    (mapcar
     (lambda (seg)
       (let ((dur (- (item.feat seg 'end) last-end)))
         (format s "%s %s " (item.name seg) dur))
       (if (item.next seg)
           (format s "- "))
       (set! last-end (item.feat seg 'end)))
     (utt.relation.items utt 'Segment))))

(define (czech-debug-print-f0 s utt)
  (czech-debug-prompt s 'F0)
  (let ((last-end 0))
    (mapcar
     (lambda (seg)
       (let ((dur (- (item.feat seg 'end) last-end)))
         (format s "%s " (item.name seg))
         (mapcar (lambda (f0)
                   (format s "%s/%d " (item.feat f0 'f0)
                           (/ (* 100 (- (item.feat f0 'pos) last-end)) dur)))
                 (item.relation.daughters seg 'Target)))
       (if (item.next seg)
           (format s "- "))
       (set! last-end (item.feat seg 'end)))
     (utt.relation.items utt 'Segment))))

(define (czech-debug-print* s utt)
  (czech-debug-print-randomization s utt)
  (czech-debug-newline s)
  (czech-debug-print-tokens s utt)
  (czech-debug-newline s)
  (czech-debug-print-words s utt)
  (czech-debug-newline s)
  (czech-debug-print-segments s utt)
  (czech-debug-newline s)
  (czech-debug-print-units s utt)
  (czech-debug-newline s)
  (czech-debug-print-durfactors s utt)
  (czech-debug-newline s)
  (czech-debug-print-durations s utt)
  (czech-debug-newline s)
  (czech-debug-print-f0 s utt)
  (czech-debug-newline s)
  (if (not (eq? s t))
      (fflush s)))

(define (czech-debug-print utt)
  (czech-debug-print* t utt))


(provide 'czech-debug)
