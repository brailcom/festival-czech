;;; Czech lexicon

;; Copyright (C) 2003, 2004 Brailcom, o.p.s.

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


(lex.create "czech")
(lex.set.phoneset "czech")

;;; Alphabet

(lex.add.entry '("a"  sym (((a:) 1))))
(lex.add.entry '("a"  nil (((a) 1))))
(lex.add.entry '("á"  nil (((d l o u) 1) ((h e:) 0) ((a:) 1))))
(lex.add.entry '("b"  nil (((b e:) 1))))
(lex.add.entry '("c"  nil (((c e:) 1))))
(lex.add.entry '("è"  nil (((c~ e:) 1))))
(lex.add.entry '("d"  nil (((d e:) 1))))
(lex.add.entry '("ï"  nil (((d~ e:) 1))))
(lex.add.entry '("e"  nil (((e:) 1))))
(lex.add.entry '("é"  nil (((d l o u) 1) ((h e:) 0) ((e:) 1))))
(lex.add.entry '("ì"  nil (((i) 1) ((j e) 0))))
(lex.add.entry '("f"  nil (((e f) 1))))
(lex.add.entry '("g"  nil (((g e:) 1))))
(lex.add.entry '("h"  nil (((h a:) 1))))
(lex.add.entry '("ch" nil (((ch a:) 1))))
(lex.add.entry '("í"  nil (((d l o u) 1) ((h e:) 0) ((i:) 1))))
(lex.add.entry '("j"  nil (((j e:) 1))))
(lex.add.entry '("k"  sym (((k a:) 1))))
(lex.add.entry '("k"  nil (((k) 0))))
(lex.add.entry '("l"  nil (((e l) 1))))
(lex.add.entry '("m"  nil (((e m) 1))))
(lex.add.entry '("n"  nil (((e n) 1))))
(lex.add.entry '("ò"  nil (((e n~) 1))))
(lex.add.entry '("o"  sym (((o:) 1))))
(lex.add.entry '("o"  nil (((o) 1))))
(lex.add.entry '("ó"  nil (((d l o u) 1) ((h e:) 0) ((o:) 1))))
(lex.add.entry '("p"  nil (((p e:) 1))))
(lex.add.entry '("q"  nil (((k v e:) 1))))
(lex.add.entry '("r"  nil (((e r) 1))))
(lex.add.entry '("ø"  nil (((e r~) 1))))
(lex.add.entry '("s"  sym (((e s) 1))))
(lex.add.entry '("s"  nil (((s) 0))))
(lex.add.entry '("¹"  nil (((e s~) 1))))
(lex.add.entry '("t"  nil (((t e:) 1))))
(lex.add.entry '("»"  nil (((t~ e:) 1))))
(lex.add.entry '("u"  sym (((u:) 1))))
(lex.add.entry '("u"  nil (((u) 1))))
(lex.add.entry '("ú"  nil (((d l o u) 1) ((h e:) 0) ((u:) 1))))
(lex.add.entry '("ù"  nil (((u:) 1) ((s k r o u) 1) ((z~ k e m) 0))))
(lex.add.entry '("v"  sym (((v e:) 1))))
(lex.add.entry '("v"  nil (((v) 0))))
(lex.add.entry '("w"  nil (((d v o) 1) ((j i) 0) ((t e:) 0) ((v e:) 1))))
(lex.add.entry '("x"  nil (((i k s) 1))))
(lex.add.entry '("y"  nil (((i) 1) ((p s i) 0) ((l o n) 0))))
(lex.add.entry '("ý"  nil (((d l o u) 1) ((h e:) 0) ((i) 1) ((p s i) 0) ((l o n) 0))))
(lex.add.entry '("z"  sym (((z e t) 1))))
(lex.add.entry '("z"  nil (((z) 0))))
(lex.add.entry '("¾"  nil (((z~ e t) 1))))

;;; Punctuation characters

(lex.add.entry '("+"  num (((p l u s) 1))))
(lex.add.entry '("-"  num (((m i) 1) ((n u s) 0))))
(lex.add.entry '("."  num (((t e c~) 1) ((k a) 0))))
(lex.add.entry '(","  num (((c e) 1) ((l i: ch) 0))))

(lex.add.entry '("-"  range (((a z~) 1))))

(lex.add.entry '("."  punc ()))
(lex.add.entry '(":"  punc ()))
(lex.add.entry '(";"  punc ()))
(lex.add.entry '(","  punc ()))
(lex.add.entry '("-"  punc ()))
(lex.add.entry '("?"  punc ()))
(lex.add.entry '("!"  punc ()))
(lex.add.entry '("`"  punc ()))
(lex.add.entry '("'"  punc ()))
(lex.add.entry '("\"" punc ()))
(lex.add.entry '("("  punc ()))
(lex.add.entry '(")"  punc ()))

(lex.add.entry '("."  nil (((t e c~) 1) ((k a) 0))))
(lex.add.entry '(":"  nil (((d v o j) 1) ((t e c~) 0) ((k a) 0))))
(lex.add.entry '(";"  nil (((s t r~ e) 1) ((d n i: k) 0))))
(lex.add.entry '(","  nil (((c~ a: r) 1) ((k a) 0))))
(lex.add.entry '("-"  nil (((p o) 1) ((m l c~) 0) ((k a) 0))))
(lex.add.entry '("?"  nil (((o) 1) ((t a) 0) ((z n i: k) 0))))
(lex.add.entry '("!"  nil (((v i) 1) ((k r~ i) 0) ((c~ n i: k) 0))))
(lex.add.entry '("`"  nil (((o) 1) ((b r a:) 0) ((c e) 0) ((n i:) 0) ((a) 1) ((p o) 0) ((s t r o f) 0))))
(lex.add.entry '("'"  nil (((a) 1) ((p o) 0) ((s t r o f) 0))))
(lex.add.entry '("\"" nil (((u) 1) ((v o) 0) ((z o f) 0) ((k i) 0))))

;;; Other non-alphanumeric characters

(lex.add.entry '("*"  nil (((h v j e) 1) ((z d~ i) 0) ((c~ k a) 0))))
(lex.add.entry '("%"  nil (((p r o) 1) ((c e n t) 0))))
(lex.add.entry '("&"  nil (((a m) 1) ((p r) 0) ((s a n t) 0))))
(lex.add.entry '("$"  nil (((d o) 1) ((l a r) 0))))
(lex.add.entry '("#"  nil (((m r~ i: z~) 1) ((k a) 0))))
(lex.add.entry '("@"  nil (((z a) 1) ((v i) 0) ((n a: c~) 0))))
(lex.add.entry '("+"  nil (((p l u s) 1))))
(lex.add.entry '("^"  nil (((s t r~ i:) 1) ((s~ k a) 0))))
(lex.add.entry '("~"  nil (((v l n) 1) ((k a) 0))))
(lex.add.entry '("="  nil (((r o) 1) ((v n a:) 0) ((s e) 0))))
(lex.add.entry '("/"  nil (((l o) 1) ((m e) 0) ((n o) 0))))
(lex.add.entry '("\\" nil (((z p j e) 1) ((t n e:) 0) ((l o) 1) ((m i: t) 0) ((k o) 0))))
(lex.add.entry '("_"  nil (((p o d) 1) ((t r) 0) ((z~ i: t) 0) ((k o) 0))))
(lex.add.entry '("|"  nil (((s v i) 1) ((s l i: t) 0) ((k o) 0))))
(lex.add.entry '(">"  nil (((v j e) 1) ((t s~ i:) 0) ((n e s~) 0))))
(lex.add.entry '("<"  nil (((m e n) 1) ((s~ i:) 0) ((n e s~) 0))))
(lex.add.entry '("["  nil (((l e) 1) ((v a:) 0) ((h r a) 1) ((n a) 0) ((t a:) 0))))
(lex.add.entry '("]"  nil (((p r a) 1) ((v a:) 0) ((h r a) 1) ((n a) 0) ((t a:) 0))))
(lex.add.entry '("{"  nil (((l e) 1) ((v a:) 0) ((s l o) 1) ((z~ e) 0) ((n a:) 0))))
(lex.add.entry '("}"  nil (((p r a) 1) ((v a:) 0) ((s l o) 1) ((z~ e) 0) ((n a:) 0))))
(lex.add.entry '("("  nil (((l e) 1) ((v a:) 0) ((k u) 1) ((l a) 0) ((t a:) 0))))
(lex.add.entry '(")"  nil (((p r a) 1) ((v a:) 0) ((k u) 1) ((l a) 0) ((t a:) 0))))
(lex.add.entry '(" "  nil (((m e) 1) ((z e) 0) ((r a) 0))))
(lex.add.entry '("\t" nil (((t a) 1) ((b u) 0) ((l a:) 0) ((t o r) 0))))
(lex.add.entry '("\n" nil (((n o) 1) ((v i:) 0) ((r~ a:) 1) ((d e k) 0))))

;;; Words with non-letter characters

(lex.add.entry '("OS/2" nil (((o: #) 1) ((e s) 1) ((d v a) 1))))
(lex.add.entry '("km/h" nil (((k i) 1) ((l o) 0) ((m e t) 0) ((r u:) 0) ((z a) 1) ((h o) 0) ((d~ i) 0) ((n u) 0))))
(lex.add.entry '("m/s" nil (((m e t) 1) ((r u:) 0) ((z a) 1) ((s e) 0) ((k u n) 0) ((d u) 0))))

;;; Words

(lex.add.entry '("Brailcom" nil (((b r a i l) 1) ((k o m) 0) )))
(lex.add.entry '("Brailcomem" nil (((b r a i l) 1) ((k o) 0) ((m e m) 0))))
(lex.add.entry '("Brailcomu" nil (((b r a i l) 1) ((k o) 0) ((m u) 0) )))
(lex.add.entry '("Emacs" nil (((i:) 1) ((m e k s) 0))))
(lex.add.entry '("Emacsem" nil (((i:) 1) ((m e) 0) ((k s e m) 0))))
(lex.add.entry '("Emacsu" nil (((i:) 1) ((m e) 0) ((k s u) 0))))
(lex.add.entry '("GNU" nil (((g n u:) 1))))
(lex.add.entry '("Kè"  nil (((k o) 1) ((r u n) 0))))
(lex.add.entry '("cca" nil (((c i r) 1) ((k a) 0))))
(lex.add.entry '("cm"  nil (((c e n) 1) ((t i) 0) ((m e) 0) ((t r u:) 0))))
(lex.add.entry '("control" nil (((k o n) 1) ((t r o l) 0))))
(lex.add.entry '("copyright" nil (((k o) 1) ((p i) 0) ((r a j t) 0))))
(lex.add.entry '("czech" nil (((c~ e k) 1))))
(lex.add.entry '("dispozici" nil (((d i s) 1) ((p o) 0) ((z i) 0) ((c i) 0))))
(lex.add.entry '("escape" nil (((i s ) 1) ((k e j p) 0))))
(lex.add.entry '("festival" nil (((f e s) 1) ((t i) 0) ((v a l) 0))))
(lex.add.entry '("freebsoft" nil (((f r i:) 1) ((b s o f t) 0))))
(lex.add.entry '("freebsoftem" nil (((f r i:) 1) ((b s o f ) 0) ((t e m) 0))))
(lex.add.entry '("freebsoftu" nil (((f r i:) 1) ((b s o f) 0) ((t u) 0))))
(lex.add.entry '("km"  nil (((k i) 1) ((l o) 0) ((m e) 0) ((t r u:) 0))))
(lex.add.entry '("mladistvý" nil (((m l a) 1) ((d~ i) 0) ((s t v i:) 0))))
(lex.add.entry '("mm"  nil (((m i) 1) ((l i) 0) ((m e) 0) ((t r u:) 0))))
(lex.add.entry '("protivný" nil (((p r o) 1) ((t~ i) 0) ((v n i:) 0))))
(lex.add.entry '("pst" nil (((p s t) 1))))
(lex.add.entry '("shift" nil (((s~ i f t) 1))))
(lex.add.entry '("softwaru" nil (((s o f t) 1) ((v e:) 0) ((r u) 0))))
(lex.add.entry '("technicky" nil (((t e ch) 1) ((n i c) 0) ((k i) 0))))

;;; Announce

(provide 'czech-lexicon)
