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

(defvar czech-multiword-abbrevs
  '(("á" ("dlouhé" "a"))
    ("é" ("dlouhé" "e"))
    ("í" ("dlouhé" "i"))
    ("ó" ("dlouhé" "o"))
    ("ú" ("dlouhé" "u"))
    ("ù" ("u" "s" "krou¾kem"))
    ("w" ("dvojité" "v"))
    ("ý" ("dlouhé" "y"))
    ("`" ("obrácený" "apostrof"))
    ("\\" ("zpìtné" "lomítko"))
    (">" ("vìt¹í" "ne¾"))
    ("<" ("men¹í" "ne¾"))
    ("[" ("levá" "hranatá"))
    ("]" ("pravá" "hranatá"))
    ("{" ("levá" "slo¾ená"))
    ("}" ("pravá" "slo¾ená"))
    ("(" ("levá" "kulatá"))
    (")" ("pravá" "kulatá"))
    ("\n" ("nový" "øádek"))
    ("OS/2" ("OS" "2"))
    ("km/h" ("kilometrù" "za" "hodinu"))
    ("m/s" ("metrù" "za" "sekundu"))
    ))

;;; Alphabet

(lex.add.entry '("a"  sym (((a:) 0))))
(lex.add.entry '("a"  nil (((a) 0))))
(lex.add.entry '("b"  nil (((b e:) 0))))
(lex.add.entry '("c"  nil (((c e:) 0))))
(lex.add.entry '("è"  nil (((c~ e:) 0))))
(lex.add.entry '("d"  nil (((d e:) 0))))
(lex.add.entry '("ï"  nil (((d~ e:) 0))))
(lex.add.entry '("e"  nil (((e:) 0))))
(lex.add.entry '("ì"  nil (((i j e) 0))))
(lex.add.entry '("f"  nil (((e f) 0))))
(lex.add.entry '("g"  nil (((g e:) 0))))
(lex.add.entry '("h"  nil (((h a:) 0))))
(lex.add.entry '("ch" nil (((ch a:) 0))))
(lex.add.entry '("j"  nil (((j e:) 0))))
(lex.add.entry '("k"  sym (((k a:) 0))))
(lex.add.entry '("k"  nil (((k) 0))))
(lex.add.entry '("l"  nil (((e l) 0))))
(lex.add.entry '("m"  nil (((e m) 0))))
(lex.add.entry '("n"  nil (((e n) 0))))
(lex.add.entry '("ò"  nil (((e n~) 0))))
(lex.add.entry '("o"  sym (((o:) 0))))
(lex.add.entry '("o"  nil (((o) 0))))
(lex.add.entry '("p"  nil (((p e:) 0))))
(lex.add.entry '("q"  nil (((k v e:) 0))))
(lex.add.entry '("r"  nil (((e r) 0))))
(lex.add.entry '("ø"  nil (((e r~) 0))))
(lex.add.entry '("s"  sym (((e s) 0))))
(lex.add.entry '("s"  nil (((s) 0))))
(lex.add.entry '("¹"  nil (((e s~) 0))))
(lex.add.entry '("t"  nil (((t e:) 0))))
(lex.add.entry '("»"  nil (((t~ e:) 0))))
(lex.add.entry '("u"  sym (((u:) 0))))
(lex.add.entry '("u"  nil (((u) 0))))
(lex.add.entry '("v"  sym (((v e:) 0))))
(lex.add.entry '("v"  nil (((v) 0))))
(lex.add.entry '("x"  nil (((i k s) 0))))
(lex.add.entry '("y"  nil (((i p s i l o n) 0))))
(lex.add.entry '("z"  sym (((z e t) 0))))
(lex.add.entry '("z"  nil (((z) 0))))
(lex.add.entry '("¾"  nil (((z~ e t) 0))))

;;; Punctuation characters

(lex.add.entry '("+"  num (((p l u s) 0))))
(lex.add.entry '("-"  num (((m i n u s) 0))))
(lex.add.entry '("."  num (((t e c~ k a) 0))))
(lex.add.entry '(","  num (((c e l i: ch) 0))))

(lex.add.entry '("-"  range (((a z~) 0))))

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
(lex.add.entry '("["  punc (((l e v a:) 0) ((h r a n a t a:) 0))))
(lex.add.entry '("]"  punc (((p r a v a:) 0) ((h r a n a t a:) 0))))
(lex.add.entry '("{"  punc (((l e v a:) 0) ((s l o z~ e n a:) 0))))
(lex.add.entry '("}"  punc (((p r a v a:) 0) ((s l o z~ e n a:) 0))))
(lex.add.entry '("<"  punc (((l e v a:) 0) ((l o m e n a:) 0))))
(lex.add.entry '(">"  punc (((p r a v a:) 0) ((l o m e n a:) 0))))

(lex.add.entry '("."  nil (((t e c~ k a) 0))))
(lex.add.entry '(":"  nil (((d v o j t e c~ k a) 0))))
(lex.add.entry '(";"  nil (((s t r~ e d n i: k) 0))))
(lex.add.entry '(","  nil (((c~ a: r k a) 0))))
(lex.add.entry '("-"  nil (((p o m l c~ k a) 0))))
(lex.add.entry '("?"  nil (((o t a z n i: k) 0))))
(lex.add.entry '("!"  nil (((v i k r~ i c~ n i: k) 0))))
(lex.add.entry '("'"  nil (((a p o s t r o f) 0))))
(lex.add.entry '("\"" nil (((u v o z o f k i) 0))))
(lex.add.entry '(" "  nil (((m e z e r a) 0))))

;;; Other non-alphanumeric characters

(lex.add.entry '("*"  nil (((h v j e z d~ i c~ k a) 0))))
(lex.add.entry '("%"  nil (((p r o c e n t) 0))))
(lex.add.entry '("&"  nil (((a m p r s a n t) 0))))
(lex.add.entry '("$"  nil (((d o l a r) 0))))
(lex.add.entry '("#"  nil (((m r~ i: z~ k a) 0))))
(lex.add.entry '("@"  nil (((z a v i n a: c~) 0))))
(lex.add.entry '("+"  nil (((p l u s) 0))))
(lex.add.entry '("^"  nil (((s t r~ i: s~ k a) 0))))
(lex.add.entry '("~"  nil (((v l n k a) 0))))
(lex.add.entry '("="  nil (((r o v n a:) 0) ((s e) 0))))
(lex.add.entry '("/"  nil (((l o m e n o) 0))))
(lex.add.entry '("_"  nil (((p o d t r z~ i: t k o) 0))))
(lex.add.entry '("|"  nil (((s v i s l i: t k o) 0))))
(lex.add.entry '("\t" nil (((t a b u l a: t o r) 0))))

;;; Words

(lex.add.entry '("Brailcom" nil (((b r a j l k o m) 0) )))
(lex.add.entry '("Brailcomem" nil (((b r a j l k o m e m) 0))))
(lex.add.entry '("Brailcomu" nil (((b r a j l k o m u) 0) )))
(lex.add.entry '("Emacs" nil (((i: m e k s) 0))))
(lex.add.entry '("Emacsem" nil (((i: m e k s e m) 0))))
(lex.add.entry '("Emacsu" nil (((i: m e k s u) 0))))
(lex.add.entry '("GNU" nil (((g n u:) 0))))
(lex.add.entry '("Kè"  nil (((k o r u n) 0))))
(lex.add.entry '("cca" nil (((c i r k a) 0))))
(lex.add.entry '("cm"  nil (((c e n t i m e t r u:) 0))))
(lex.add.entry '("control" nil (((k o n t r o l) 0))))
(lex.add.entry '("copyright" nil (((k o p i r a j t) 0))))
(lex.add.entry '("czech" nil (((c~ e k) 0))))
(lex.add.entry '("dispozici" nil (((d i s p o z i c i) 0))))
(lex.add.entry '("escape" nil (((i s k e j p) 0))))
(lex.add.entry '("festival" nil (((f e s t i v a l) 0))))
(lex.add.entry '("freebsoft" nil (((f r i: p s o f t) 0))))
(lex.add.entry '("freebsoftem" nil (((f r i: p s o f t e m) 0))))
(lex.add.entry '("freebsoftu" nil (((f r i: p s o f t u) 0))))
(lex.add.entry '("km"  nil (((k i l o m e t r u:) 0))))
(lex.add.entry '("mladistvý" nil (((m l a d~ i s t v i:) 0))))
(lex.add.entry '("mm"  nil (((m i l i m e t r u:) 0))))
(lex.add.entry '("protivný" nil (((p r o t~ i v n i:) 0))))
(lex.add.entry '("pst" nil (((p s t) 0))))
(lex.add.entry '("shift" nil (((s~ i f t) 0))))
(lex.add.entry '("softwaru" nil (((s o f t v e: r u) 0))))
(lex.add.entry '("technicky" nil (((t e ch n i c k i) 0))))

;;; Announce

(provide 'czech-lexicon)
