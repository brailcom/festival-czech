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


;;; Alphabet

("a"  sym (((a:) 0)))
("a"  nil (((a) 0)))
("b"  nil (((b e:) 0)))
("c"  nil (((c e:) 0)))
("è"  nil (((c~ e:) 0)))
("d"  nil (((d e:) 0)))
("ï"  nil (((d~ e:) 0)))
("e"  nil (((e:) 0)))
("ì"  nil (((i j e) 0)))
("f"  nil (((e f) 0)))
("g"  nil (((g e:) 0)))
("h"  nil (((h a:) 0)))
("ch" nil (((ch a:) 0)))
("j"  nil (((j e:) 0)))
("k"  sym (((k a:) 0)))
("k"  nil (((k) 0)))
("l"  nil (((e l) 0)))
("m"  nil (((e m) 0)))
("n"  nil (((e n) 0)))
("ò"  nil (((e n~) 0)))
("o"  sym (((o:) 0)))
("o"  nil (((o) 0)))
("p"  nil (((p e:) 0)))
("q"  nil (((k v e:) 0)))
("r"  nil (((e r) 0)))
("ø"  nil (((e r~) 0)))
("s"  sym (((e s) 0)))
("s"  nil (((s) 0)))
("¹"  nil (((e s~) 0)))
("t"  nil (((t e:) 0)))
("»"  nil (((t~ e:) 0)))
("u"  sym (((u:) 0)))
("u"  nil (((u) 0)))
("v"  sym (((v e:) 0)))
("v"  nil (((v) 0)))
("x"  nil (((i k s) 0)))
("y"  nil (((i p s i l o n) 0)))
("z"  sym (((z e t) 0)))
("z"  nil (((z) 0)))
("¾"  nil (((z~ e t) 0)))

;;; Punctuation characters

("+"  num (((p l u s) 0)))
("-"  num (((m i n u s) 0)))
("."  num (((t e c~ k a) 0)))
(","  num (((c e l i: ch) 0)))

("-"  range (((a z~) 0)))

("."  punc ())
(":"  punc ())
(";"  punc ())
(","  punc ())
("-"  punc ())
("?"  punc ())
("!"  punc ())
("`"  punc ())
("'"  punc ())
("\"" punc ())
("("  punc ())
(")"  punc ())
;; These are multiword entries, but there's not another easy way to handle the
;; punctuation
("["  punc (((l e v a:) 0) ((h r a n a t a:) 0)))
("]"  punc (((p r a v a:) 0) ((h r a n a t a:) 0)))
("{"  punc (((l e v a:) 0) ((s l o z~ e n a:) 0)))
("}"  punc (((p r a v a:) 0) ((s l o z~ e n a:) 0)))
("<"  punc (((m e n s~ i:) 0) ((n e z~) 0)))
(">"  punc (((v j e t s~ i:) 0) ((n e z~) 0)))

("."  nil (((t e c~ k a) 0)))
(":"  nil (((d v o j t e c~ k a) 0)))
(";"  nil (((s t r~ e d n i: k) 0)))
(","  nil (((c~ a: r k a) 0)))
("-"  nil (((p o m l c~ k a) 0)))
("?"  nil (((o t a z n i: k) 0)))
("!"  nil (((v i k r~ i c~ n i: k) 0)))
("'"  nil (((a p o s t r o f) 0)))
("\"" nil (((u v o z o f k i) 0)))
(" "  nil (((m e z e r a) 0)))

;;; Other non-alphanumeric characters

("*"  nil (((h v j e z d~ i c~ k a) 0)))
("%"  nil (((p r o c e n t) 0)))
("&"  nil (((a m p r s a n t) 0)))
("$"  nil (((d o l a r) 0)))
("#"  nil (((m r~ i: z~ k a) 0)))
("@"  nil (((z a v i n a: c~) 0)))
("+"  nil (((p l u s) 0)))
("^"  nil (((s t r~ i: s~ k a) 0)))
("~"  nil (((v l n k a) 0)))
("="  nil (((r o v n a:) 0) ((s e) 0)))
("/"  nil (((l o m e n o) 0)))
("_"  nil (((p o d t r z~ i: t k o) 0)))
("|"  nil (((s v i s l i: t k o) 0)))
("\t" nil (((t a b u l a: t o r) 0)))

;;; Words

("Brailcom" nil (((b r a j l k o m) 0) ))
("Brailcomem" nil (((b r a j l k o m e m) 0)))
("Brailcomu" nil (((b r a j l k o m u) 0) ))
("Emacs" nil (((i: m e k s) 0)))
("Emacsem" nil (((i: m e k s e m) 0)))
("Emacsu" nil (((i: m e k s u) 0)))
("GNU" nil (((g n u:) 0)))
("Kè"  nil (((k o r u n) 0)))
("cca" nil (((c i r k a) 0)))
("control" nil (((k o n t r o l) 0)))
("copyright" nil (((k o p i r a j t) 0)))
("czech" nil (((c~ e k) 0)))
("dispozici" nil (((d i s p o z i c i) 0)))
("escape" nil (((i s k e j p) 0)))
("festival" nil (((f e s t i v a l) 0)))
("freebsoft" nil (((f r i: p s o f t) 0)))
("freebsoftem" nil (((f r i: p s o f t e m) 0)))
("freebsoftu" nil (((f r i: p s o f t u) 0)))
("mladistvý" nil (((m l a d~ i s t v i:) 0)))
("protivný" nil (((p r o t~ i v n i:) 0)))
("pst" nil (((p s t) 0)))
("shift" nil (((s~ i f t) 0)))
("softwaru" nil (((s o f t v e: r u) 0)))
("technicky" nil (((t e ch n i c k i) 0)))
