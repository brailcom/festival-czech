;;; Czech support for Festival

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

(defvar czech-default-description
  '((phoneset-translation nil)
    (synthesis-method UniSyn)
    (synthesis-init czech-default-synthesis-init)))

(defvar czech-description czech-default-description)

(define (czech-parameter parameter)
  (cadr (assoc parameter czech-description)))

;;; Phone set

(defPhoneSet czech
  (;; vowel or consonant: vowel consonant
   ;; (it's necessary to use exactly this symbol with the defined values, so
   ;;  that the default rules identify vowels and consonants correctly)
   (vc + - 0)
   ;; vowel length: short long
   (vl 1 2 0)
   ;; vowel height: low mid high
   (vh 1 2 3 0)
   ;; vowel lip rounding: yes no
   (vr + - 0)
   ;; consonant voicing: yes no
   (cv + - 0)
   ;; consonant able to create sylable: yes no
   (cs + - 0)
   ;; consonant hardness: hard soft
   (cf + - 0)
   ;; consonant can be lengthen: short limited unlimited
   (cl 1 2 3 0)
   ;; consonant place of articulation: lips tongue sibilant nasal neck
   (ca l t s g n 0)
   )
  (
   (#   0 0 0 0 0 0 0 0 0)
   (##  0 0 0 0 0 0 0 0 0)
   (a   + 1 3 - 0 0 0 0 0)
   (a:  + 2 3 - 0 0 0 0 0)
   (b   - 0 0 0 + - 0 1 l)
   (c   - 0 0 0 - - + 1 s)
   (ch  - 0 0 0 - - 0 2 n)
   (c~  - 0 0 0 - - - 1 s)
   (d   - 0 0 0 + - + 1 t)
   (d~  - 0 0 0 + - - 1 t)
   (dz  - 0 0 0 + - + 2 s)
   (dz~ - 0 0 0 + - - 2 s)
   (e   + 1 2 - 0 0 0 0 0)
   (e:  + 2 2 - 0 0 0 0 0)
   (f   - 0 0 0 - - 0 2 l)
   (g   - 0 0 0 + - 0 1 g)
   (h   - 0 0 0 + - 0 2 n)
   (i   + 1 1 - 0 0 0 0 0)
   (i:  + 2 1 - 0 0 0 0 0)
   (j   - 0 0 0 0 - 0 1 t)
   (k   - 0 0 0 - - 0 1 n)
   (l   - 0 0 0 0 + + 1 t)
   (m   - 0 0 0 0 - 0 1 l)
   (n   - 0 0 0 0 - + 1 g)
   (n~  - 0 0 0 0 - - 1 g)
   (o   + 1 2 + 0 0 0 0 0)
   (o:  + 2 2 + 0 0 0 0 0)
   (p   - 0 0 0 - - 0 1 l)
   (r   - 0 0 0 0 + + 3 t)
   (r~  - 0 0 0 0 - - 3 t)
   (s   - 0 0 0 - - + 3 s)
   (s~  - 0 0 0 - - - 3 s)
   (t   - 0 0 0 - - + 1 t)
   (t~  - 0 0 0 - - - 1 t)
   (u   + 1 1 + 0 0 0 0 0)
   (u:  + 2 1 + 0 0 0 0 0)
   (v   - 0 0 0 + - 0 2 l)
   (z   - 0 0 0 + - + 3 s)
   (z~  - 0 0 0 + - - 3 s)
  )
)
(PhoneSet.silences '(# ##))

;;; Text to phones

(lex.create "czech")
(lex.set.phoneset "czech")

(lts.ruleset
 czech-downcase
 ()
 (
  ( [ a ] = a )
  ( [ á ] = á )
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ è ] = è )
  ( [ d ] = d )
  ( [ ï ] = ï )
  ( [ e ] = e )
  ( [ é ] = é )
  ( [ ì ] = ì )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ i ] = i )
  ( [ í ] = í )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ ò ] = ò )
  ( [ o ] = o )
  ( [ ó ] = ó )
  ( [ p ] = p )
  ( [ q ] = q )
  ( [ r ] = r )
  ( [ ø ] = ø )
  ( [ s ] = s )
  ( [ ¹ ] = ¹ )
  ( [ t ] = t )
  ( [ » ] = » )
  ( [ u ] = u )
  ( [ ú ] = ú )
  ( [ ù ] = ù )
  ( [ v ] = v )
  ( [ w ] = w )
  ( [ x ] = x )
  ( [ y ] = y )
  ( [ ý ] = ý )
  ( [ z ] = z )
  ( [ ¾ ] = ¾ )
  ( [ A ] = a )
  ( [ Á ] = á )
  ( [ B ] = b )
  ( [ C ] = c )
  ( [ È ] = è )
  ( [ D ] = d )
  ( [ Ï ] = ï )
  ( [ E ] = e )
  ( [ É ] = é )
  ( [ Ì ] = ì )
  ( [ F ] = f )
  ( [ G ] = g )
  ( [ H ] = h )
  ( [ I ] = i )
  ( [ Í ] = í )
  ( [ J ] = j )
  ( [ K ] = k )
  ( [ L ] = l )
  ( [ M ] = m )
  ( [ N ] = n )
  ( [ Ò ] = ò )
  ( [ O ] = o )
  ( [ Ó ] = ó )
  ( [ P ] = p )
  ( [ Q ] = q )
  ( [ R ] = r )
  ( [ Ø ] = ø )
  ( [ S ] = s )
  ( [ © ] = ¹ )
  ( [ T ] = t )
  ( [ « ] = » )
  ( [ U ] = u )
  ( [ Ú ] = ú )
  ( [ Ù ] = ù )
  ( [ V ] = v )
  ( [ W ] = w )
  ( [ X ] = x )
  ( [ Y ] = y )
  ( [ Ý ] = ý )
  ( [ Z ] = z )
  ( [ ® ] = ¾ )
  ( [ 0 ] = 0 )
  ( [ 1 ] = 1 )
  ( [ 2 ] = 2 )
  ( [ 3 ] = 3 )
  ( [ 4 ] = 4 )
  ( [ 5 ] = 5 )
  ( [ 6 ] = 6 )
  ( [ 7 ] = 7 )
  ( [ 8 ] = 8 )
  ( [ 9 ] = 9 )
  ))
 
(lts.ruleset
 czech
 ((BPV b p v)
  (DTN d t n)
  (ÌI ì i í)
  (SZ s z)
  (Vowel a á e é i í o ó u ú ù y ý))
 (
  ;; Special combinations
  ( [ d ] i SZ m u = d )
  ( [ n ] i SZ m u = n )
  ( [ t ] i SZ m u = t )
  ( m l a [ d ] i s t = d~ )
  ( [ d ] i s t = d )
  ( [ n ] i s t = n )
  ( [ t ] i s t = t )
  ( [ t ] i c k = t )
  ( [ t ] i è t ì = t )
  ( # a n [ t ] i = t )
  ( # a n t [ i ] Vowel = i # )
  ( p r o [ t ] i v n = t~ )
  ( [ t ] i v n = t )
  ( [ d ] ÌI = d~ )
  ( [ t ] ÌI = t~ )
  ( [ n ] ÌI = n~ )
  ( DTN [ ì ] = e )
  ( BPV [ ì ] = j e )
  ( m [ ì ] = n~ e )
  ;; Special combinations (maybe...)
  ( # [ i ] Vowel = j )
  ( [ i ] Vowel = i j )
  ( [ í ] Vowel = i: j )
  ( Vowel [ i ] = j )
  ( Vowel [ y ] = j )
  ( Vowel [ í ] = j i: )
  ( Vowel [ ý ] = j i: )
  ( [ n n ] ÌI = n~ )
  ( [ n n ] = n )

  ;; Endings (maybe...)
  ( # [ b ] = b )
  ( # [ d ] = d )
  ( # [ d z ] = dz )
  ( # [ d ¾ ] = dz~ )
  ( # [ g ] = g )
  ( # [ h ] = h )
  ( # [ v ] = v )
  ( # [ w ] = v )
  ( # [ z ] = z )
  ( [ b ] # = p )
  ( [ d ] # = t )
  ( [ d z ] # = c )
  ( [ d ¾ ] # = c~ )
  ( [ g ] # = k )
  ( [ h ] # = ch )
  ( [ v ] # = f )
  ( [ w ] # = f )
  ( [ z ] # = s )
  
  ;; Two-letter phonems
  ( [ d ¾ ] = dz~ )
  ( [ d z ] = dz )
  ( [ c h ] = ch )

  ;; Special letters
  ( [ ì ] = j e )
  ;; Simple letters
  ( [ a ] = a )
  ( [ á ] = a: )
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ è ] = c~ )
  ( [ d ] = d )
  ( [ ï ] = d~ )
  ( [ e ] = e )
  ( [ é ] = e: )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ i ] = i )
  ( [ í ] = i: )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ ò ] = n~ )
  ( [ o ] = o )
  ( [ ó ] = o: )
  ( [ p ] = p )
  ( [ q ] = k v )
  ( [ r ] = r )
  ( [ ø ] = r~ )
  ( [ s ] = s )
  ( [ ¹ ] = s~ )
  ( [ t ] = t )
  ( [ » ] = t~ )
  ( [ u ] = u )
  ( [ ú ] = u: )
  ( [ ù ] = u: )
  ( [ v ] = v )
  ( [ w ] = v )
  ( [ x ] = k s )
  ( [ y ] = i )
  ( [ ý ] = i: )
  ( [ z ] = z )
  ( [ ¾ ] = z~ )
  ))

(defvar czech-unknown-symbol-word "neznámý")
(define (czech-lts word features)
  (list word
        nil
        (lex.syllabify.phstress
         (lts.apply
          (lts.apply
           (if (lts.in.alphabet word 'czech-downcase)
               word
               czech-unknown-symbol-word)
           'czech-downcase)
          'czech))))
(lex.set.lts.method 'czech-lts)

;;; Tokenization

(defvar czech-token.unknown_word_name "neznámé")
(defvar czech-token.whitespace "  \t\n\r")
(defvar czech-token.punctuation "\"'`-.,:;!?(){}[]<>")
(defvar czech-token.prepunctuation "\"'`({[<")

;;; Token to words processing

(define (czech-remove element list)
  (cond
   ((null? list) list)
   ((equal? element (car list)) (czech-remove element (cdr list)))
   (t (cons (car list) (czech-remove element (cdr list))))))

(define (czech-number name)
  (cond
   ((string-matches name "^[-+].*")
    (cons (substring name 0 1)
          (czech-number (substring name 1 (- (length name) 1)))))
   ((string-matches name ".*[,.].*")
    (let ((comma (if (string-matches name ".*,.*") "," ".")))
      (append (czech-number (string-before name comma))
              (list comma)
              (czech-number (string-after name comma)))))
   ((string-equal name "0")
    (list "nula"))
   (t
    (czech-number-from-digits (czech-remove (car (symbolexplode " "))
                                            (symbolexplode name))))))

(define (czech-number-from-digits digits)
  (let ((len (length digits)))
    (cond
     ((equal? len 1)
      (let ((d (car digits)))
	(cond
	 ((string-equal d "0") ())
	 ((string-equal d "1") (list "jedna"))
	 ((string-equal d "2") (list "dva"))
	 ((string-equal d "3") (list "tøi"))
	 ((string-equal d "4") (list "ètyøi"))
	 ((string-equal d "5") (list "pìt"))
	 ((string-equal d "6") (list "¹est"))
	 ((string-equal d "7") (list "sedm"))
	 ((string-equal d "8") (list "osm"))
	 ((string-equal d "9") (list "devìt")))))
     ((equal? len 2)
      (if (string-equal (car digits) "1")
	  (let ((d (car (cdr digits))))
	    (cond
	     ((string-equal d "0") (list "deset"))
	     ((string-equal d "1") (list "jedenáct"))
	     ((string-equal d "2") (list "dvanáct"))
	     ((string-equal d "3") (list "tøináct"))
	     ((string-equal d "4") (list "ètrnáct"))
	     ((string-equal d "5") (list "patnáct"))
	     ((string-equal d "6") (list "¹estnáct"))
	     ((string-equal d "7") (list "sedmnáct"))
	     ((string-equal d "8") (list "osmnáct"))
	     ((string-equal d "9") (list "devatenáct"))))
	  (append
	   (let ((d (car digits)))
	     (cond
	      ((string-equal d "0") ())
	      ((string-equal d "2") (list "dvacet"))
	      ((string-equal d "3") (list "tøicet"))
	      ((string-equal d "4") (list "ètyøicet"))
	      ((string-equal d "5") (list "padesát"))
	      ((string-equal d "6") (list "¹edesát"))
	      ((string-equal d "7") (list "sedmdesát"))
	      ((string-equal d "8") (list "osmdesát"))
	      ((string-equal d "9") (list "devadesát"))))
	   (czech-number-from-digits (cdr digits)))))
     ((equal? len 3)
      (append
       (let ((d (car digits)))
	 (cond
	  ((string-equal d "0") ())
	  ((string-equal d "1") (list "sto"))
	  ((string-equal d "2") (list "dvì" "stì"))
	  ((string-equal d "3") (list "tøi" "sta"))
	  ((string-equal d "4") (list "ètyøi" "sta"))
	  ((string-equal d "5") (list "pìt" "set"))
	  ((string-equal d "6") (list "¹est" "set"))
	  ((string-equal d "7") (list "sedm" "set"))
	  ((string-equal d "8") (list "osm" "set"))
	  ((string-equal d "9") (list "devìt" "set"))))
       (czech-number-from-digits (cdr digits))))
     ((<= len 12)
      (let ((concatenations '((t "tisíc" "tisíce" "tisíc")
			      (t "milion" "miliony" "milionù")
			      (nil "miliarda" "miliardy" "miliard")))
	    (n (- len 3)))
	(while (> n 3)
	  (set! concatenations (cdr concatenations))
	  (set! n (- n 3)))
	(let ((m n)
	      (head-digits ())
	      (tail-digits digits)
	      (words (car concatenations)))
	  (while (> m 0)
	    (set! head-digits (cons (car tail-digits) head-digits))
	    (set! tail-digits (cdr tail-digits))
	    (set! m (- m 1)))
	  (set! head-digits (reverse head-digits))
	  (append
	   (cond
	    ((and (equal? n 1) (string-equal (car digits) "1"))
	     (list (car (cdr words))))
	    ((and (equal? n 1) (string-matches (car digits) "[2-4]"))
	     (list
	      (cond
	       ((string-equal (car digits) "2")
		(if (car words) "dva" "dvì"))
	       ((string-equal (car digits) "3") "tøi")
	       ((string-equal (car digits) "4") "ètyøi"))
	      (car (cdr (cdr words)))))
	    (t
	     (append
	      (czech-number-from-digits head-digits)
	      (list (car (cdr (cdr (cdr words))))))))
	   (czech-number-from-digits tail-digits)))))
     (t
      (apply append (mapcar czech-number digits))))))

(define (czech-tokenize-on-nonalphas string)
  (cond
   ((string-equal string "")
    nil)
   ((string-matches string "^[a-zA-Záèïéìíòóø¹»úùý¾ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®]*$")
    (list string))
   ((string-matches string "^[0-9]+$")
    (symbolexplode string))
   (t
    (let ((i 0))
      (while (string-matches (substring string i 1)
                             "[a-zA-Záèïéìíòóø¹»úùý¾ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®]")
        (set! i (+ i 1)))
      (if (eq? i 0)
          (while (string-matches (substring string i 1) "[0-9]")
                 (set! i (+ i 1))))
      (append (if (> i 0)
                  (let ((s (substring string 0 i)))
                    (if (string-matches s "[0-9]+")
                        (symbolexplode s)
                        (list s)))
                  nil)
              (list (substring string i 1))
              (czech-tokenize-on-nonalphas
               (substring string (+ i 1)
                          (- (length string) (+ i 1)))))))))

(define (czech-token_to_words token name)
  (cond
   ((and (string-matches name "^[0-9]+$")
         (string-equal (item.feat token 'punc) ".")
         (item.next token)
         (not (string-matches (item.feat (item.next token) 'whitespace)
                              "  +")))
    (if (not (assoc 'punctype (item.features token)))
        (item.set_feat token 'punctype 'num))
    (append (czech-number name)
            (list ".")))
   ((string-matches name "^0[0-9]*$")
    (apply append (mapcar czech-number (symbolexplode name))))
   ((or (string-matches name "^[-+]*[0-9][0-9 ]*$")
        (string-matches name "^[-+]*[0-9][0-9 ]*[.,] *[0-9][0-9]*$"))
    (if (not (assoc 'punctype (item.features token)))
        (item.set_feat token 'punctype 'num))
    (czech-number name))
   ((and (string-matches
          name
          "^[bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQSTVWXZèïòø¹»¾ÈÏÒØ©«®][bcdfghjkmnpqstvwxzBCDFGHJKMNPQSTVWXZèïòø¹»¾ÈÏÒØ©«®]+$")
         (not (lex.lookup_all name)))
    (symbolexplode name))
   ((or (string-matches name "^[a-zA-Záèïéìíòóø¹»úùý¾ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®]+$")
        (string-matches name "^[^a-zA-Záèïéìíòóø¹»úùý¾ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®0-9]+$"))
    (list name))
   (t
    (if (not (string-matches name
                             "^[-a-zA-Záèïéìíòóø¹»úùý¾ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®]+$"))
        (item.set_feat token 'punctype nil))
    (apply
     append
     (mapcar (lambda (name) (czech-token_to_words token name))
             (czech-tokenize-on-nonalphas name))))))

;;; Lexicon

(lex.add.entry '("neznámé" nil (((n e) 1) ((z n a:) 0) ((m e:) 0))))

(lex.add.entry '("ch" nil (((ch a:) 1))))
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

(lex.add.entry '("pst" nil (((p s t) 1))))

(lex.add.entry '("cca" nil (((c i r) 1) ((k a) 0))))
(lex.add.entry '("mm"  nil (((m i) 1) ((l i) 0) ((m e) 0) ((t r u:) 0))))
(lex.add.entry '("cm"  nil (((c e n) 1) ((t i) 0) ((m e) 0) ((t r u:) 0))))
(lex.add.entry '("km"  nil (((k i) 1) ((l o) 0) ((m e) 0) ((t r u:) 0))))

(lex.add.entry '("Emacs" nil (((i:) 1) ((m e k s) 0))))
(lex.add.entry '("Emacsu" nil (((i:) 1) ((m e) 0) ((k s u) 0))))
(lex.add.entry '("Emacsem" nil (((i:) 1) ((m e) 0) ((k s e m) 0))))
(lex.add.entry '("copyright" nil (((k o) 1) ((p i) 0) ((r a j t) 0))))
(lex.add.entry '("Brailcom" nil (((b r a i l) 1) ((k o m) 0) )))
(lex.add.entry '("Brailcomu" nil (((b r a i l) 1) ((k o) 0) ((m u) 0) )))
(lex.add.entry '("Brailcomem" nil (((b r a i l) 1) ((k o) 0) ((m e m) 0))))
(lex.add.entry '("freebsoft" nil (((f r i:) 1) ((b s o f t) 0))))
(lex.add.entry '("freebsoftu" nil (((f r i:) 1) ((b s o f) 0) ((t u) 0))))
(lex.add.entry '("freebsoftem" nil (((f r i:) 1) ((b s o f ) 0) ((t e m) 0))))
(lex.add.entry '("shift" nil (((s~ i f t) 1))))
(lex.add.entry '("control" nil (((k o n) 1) ((t r o l) 0))))
(lex.add.entry '("escape" nil (((i s ) 1) ((k e j p) 0))))

;;; Part of Speech

(define (czech-pos utt)
  (mapcar
   (lambda (w)
     (let ((name (item.name w))
           (token (item.root (item.relation w 'Token))))
       (cond
        ((and (assoc 'punctype (item.features token))
              (string-matches name
                              "^[^a-zA-Záèïéìíòóø¹»úùý¾ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®0-9]+$"))
         (item.set_feat w "pos" (item.feat token 'punctype)))
        ((member (item.name w) '("\"" "'" "`" "-" "." "," ":" ";" "!" "?"
                                 "(" ")"))
         (item.set_feat w "pos" "punc"))
        ((and (eq? (string-length name) 1)
              (or (assoc 'punc (item.features token))
                  (not (item.next token))))
         (item.set_feat w "pos" "sym")))))
   (utt.relation.items utt 'Word))
  utt)

;;; Phrase breaks

(set! czech-phrase_cart_tree
      '((lisp_token_end_punc in ("?" "!" "." ":" "-"))
	((BB))
	((lisp_token_end_punc in ("'" "\"" "," ";" ")"))
	 ((B))
	 ((n.name is 0)  ; end of utterance
	  ((BB))
	  ((NB))))))

;;; Pauses

(define (czech-pause_method utt)
  (Classic_Pauses utt)
  (let ((silence (list '##)))
    (mapcar (lambda (w)
              (let ((syl (item.relation.daughter1 w 'SylStructure)))
                (let ((seg (and syl
                                (item.relation.daughter1 syl 'SylStructure))))
                  (if (and seg
                           (string-matches (item.name seg) "[aeiou]:?")
                           (or (not (item.relation.prev seg 'Segment))
                               (not (equal? (item.name (item.relation.prev
                                                        seg 'Segment))
                                            "#"))))
                      (item.relation.insert seg 'Segment silence 'before)))))
            (utt.relation.items utt 'Word)))
  utt)

;;; Intonation

(set! czech-int_simple_params '((f0_mean 110) (f0_std 25)))

(set! czech-accent_cart_tree
      '((R:SylStructure.parent.gpos is content)
	((stress is 1)
	 ((Accented))
	 ((position_type is single)
	  ((Accented))
	  ((NONE))))
	((NONE))))

;;; Duration

(set! czech-phoneme_durations
      '(
	(# 0.25)
        (## 0.05)
	(a 0.05)
	(a: 0.125)
	(b 0.0752)
	(c 0.095)
	(c~ 0.096)
	(ch 0.08)
	(d 0.05)
	(d~ 0.07)
	(e 0.05)
	(e: 0.13)
	(f 0.08)
	(g 0.05)
	(h 0.05)
	(i 0.06)
	(i: 0.12)
	(j 0.06)
	(k 0.07)
	(l 0.05)
	(m 0.05)
	(n 0.05)
	(n~ 0.05)
	(o 0.05)
	(o: 0.1)
	(p 0.05)
	(r 0.05)
	(r~ 0.05)
	(s 0.05)
	(s~ 0.05)
	(t 0.08)
	(t~ 0.062)
	(u 0.06)
	(u: 0.09)
	(v 0.05)
	(z 0.07)
	(z~ 0.05)
	(dz 0.05)
	(dz~ 0.07)
	))

;; Final phonem translation

(define (czech-phone-adjustment utt)
  (if (eq? (Parameter.get 'Language) 'czech)
      (begin
        (mapcar (lambda (item)
                  (if (equal? (item.name item) "##")
                      (item.set_name item "#")))
                (utt.relation.items utt 'Segment))
        (let ((table (czech-parameter 'phoneset-translation)))
          (if table
              (mapcar (lambda (item)
                        (let ((tr (assoc (item.name item) table)))
                          (if tr
                              (item.set_name item (cadr tr)))))
                      (utt.relation.items utt 'Segment)))))))

;; Finally, the language definition itself

(define (voice_czech)
  (voice_reset)
  (Parameter.set 'Language 'czech)
  ;; Phone set
  (Parameter.set 'PhoneSet 'czech)
  (PhoneSet.select 'czech)
  (set! pos_lex_name nil)
  ;; Tokenization
  (set! token.unknown_word_name czech-token.unknown_word_name)
  (set! token.whitespace czech-token.whitespace)
  (set! token.punctuation czech-token.punctuation)
  (set! token.prepunctuation czech-token.prepunctuation)
  (set! token_to_words czech-token_to_words)
  ;; Lexicon selection
  (lex.select "czech")
  ;; Part of speech
  (Parameter.set 'POS_Method czech-pos)
  ;; Simple phrase break prediction by punctuation
  (set! pos_supported nil)
  (set! phrase_cart_tree czech-phrase_cart_tree)
  (Parameter.set 'Phrase_Method 'cart_tree)
  ;; Pauses
  (Parameter.set 'Pause_Method czech-pause_method)
  ;; Accent prediction and intonation
  (set! int_simple_params czech-int_simple_params)
  (set! int_accent_cart_tree czech-accent_cart_tree)
  (Parameter.set 'Int_Method 'simple)
  (Parameter.set 'Int_Target_Method Int_Targets_Simple)
  ;; Duration prediction
  (set! phoneme_durations czech-phoneme_durations)
  (Parameter.set 'Duration_Method 'Averages)
  ;; Postlex rules
  (set! postlex_rules_hooks (list))
  (set! after_analysis_hooks (list czech-phone-adjustment))
  ;; Waveform synthesizer
  (Parameter.set 'Synth_Method (czech-parameter 'synthesis-method))
  (let ((synthesis-init (czech-parameter 'synthesis-init)))
    (if synthesis-init
        (synthesis-init)))
  ;; Set current voice
  (set! current-voice 'czech))

(provide 'czech)
