;;; Czech support for Festival

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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.


;;; Utility functions

(define (czech-min x y)
  (if (<= x y) x y))

(define (czech-max x y)
  (if (>= x y) x y))

(define (czech-item.has-feat item feat)
  (assoc feat (item.features item)))

(define (czech-item.feat? item feat value)
  (and item (string-equal (item.feat item feat) value)))

(define (czech-item.feat*? item feat value)
  (and item (string-matches (item.feat item feat) value)))

(define (czech-all-same lst)
  (or (<= (length lst) 1)
      (and (string-equal (car lst) (cadr lst))
           (czech-all-same (cdr lst)))))

(defvar czech-randomize t)

(defvar czech-rand-range nil)

(defvar czech-insert-filling-vowels t)

(define (czech-rand)
  (if czech-randomize
      (begin
        (if (not czech-rand-range)
            (let ((n 100)
                  (max 0))
              (while (> n 0)
                (let ((r (rand)))
                  (if (> r max)
                      (set! max r)))
                (set! n (- n 1)))
              (set! czech-rand-range 1)
              (while (> max czech-rand-range)
                (set! czech-rand-range (* 2 czech-rand-range)))))
        (/ (rand) czech-rand-range))
      0.5))

(define (czech-random-choice lst)
  (let ((max (length lst)))
    (let ((n (* (czech-rand) max)))
      (nth n lst))))

(define (czech-next-token-punc word)
  (if (item.relation.next word "Token")
      "0"
      (item.feat word "R:Token.n.daughter1.prepunctuation")))

(define (czech-next-punc word)
  (let ((token (item.next (item.parent (item.relation word 'Token)))))
    (while (and token (not (string-matches (item.feat token 'punc) "[^0]+")))
      (set! token (item.next token)))
    (and token (item.feat token 'punc))))

(define (czech-prev-punc word)
  (let ((token (item.prev (item.parent (item.relation word 'Token)))))
    (while (and token (not (string-matches (item.feat token 'punc) "[^0]+")))
      (set! token (item.prev token)))
    (and token (item.feat token 'punc))))

(define (czech-word-stress-unit word)
  (let ((sylword (item.relation word 'SylStructure)))
    (if sylword
        (item.parent (item.relation (item.daughter1 sylword) 'StressUnit)))))

(define (czech-stress-unit-punc unit)
  (and unit
       (item.feat unit "daughtern.R:SylStructure.parent.R:Token.parent.punc")))

(define (czech-token-end-punc word)
  (if (and (item.relation.next word 'SylStructure)
           (eq? (item.relation.parent word 'Token)
                (item.relation.parent (item.relation.next word 'SylStructure)
                                      'Token)))
      0
      (item.feat word "R:Token.parent.punc")))

;;; Phone set

(defPhoneSet czech
  (;; vowel or consonant: vowel consonant
   (vc + - 0)
   ;; vowel length: short long diphthong schwa
   (vlng s l d a 0)
   ;; consonant type: nasal, sonorant, stop, affricate, fricative
   (ctype n l s a f 0)
   ;; consonant voicing: yes no
   (cvox + - 0)
   ;; can create a syllable: yes no
   (syl + - 0)
   )
  (
   ;;   c l t v s
   (#   0 0 0 0 0)                      ; pause
   (_   0 0 0 0 -)                      ; vowel-vowel stroke
   (@   + a 0 0 +)                      ; schwa
   (a   + s 0 0 +)
   (a:  + l 0 0 +)
   (au  + d 0 0 +)
   (b   - 0 s + -)
   (c   - 0 a - -)
   (ch  - 0 f - -)
   (c~  - 0 a - -)
   (d   - 0 s + -)
   (d~  - 0 s + -)
   (dz  - 0 a + -)
   (dz~ - 0 a + -)
   (e   + s 0 0 +)
   (e:  + l 0 0 +)
   (eu  + d 0 0 +)
   (f   - 0 f - -)
   (g   - 0 s + -)
   (h   - 0 f + -)
   (i   + s 0 0 +)
   (i:  + l 0 0 +)
   (j   - 0 l 0 -)
   (k   - 0 s - -)
   (l   - 0 l 0 +)
   (m   - 0 n + -)
   (n   - 0 n + -)
   (n~  - 0 n + -)
   (o   + s 0 0 +)
   (o:  + l 0 0 +)
   (ou  + d 0 0 +)
   (p   - 0 s - -)
   (r   - 0 l 0 +)
   (r~  - 0 f + -)
   (s   - 0 f - -)
   (s~  - 0 f - -)
   (t   - 0 s - -)
   (t~  - 0 s - -)
   (u   + s 0 0 +)
   (u:  + l 0 0 +)
   (v   - 0 f + -)
   (z   - 0 f + -)
   (z~  - 0 f + -)
  )
)
(PhoneSet.silences '(#))

(defvar czech-phoneset-translation
  '(("_" "#")))
(defvar czech-phoneset-translation* nil)

;;; Text to phones

(lts.ruleset
 czech-normalize
 ()
 (
  ( [ a ] = a )
  ( [ á ] = á )
  ( [ ä ] = e )
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
  ( [ ö ] = e )
  ( [ p ] = p )
  ( [ q ] = q )
  ( [ r ] = r )
  ( [ ø ] = ø )
  ( [ s ] = s )
  ( [ ¹ ] = ¹ )
  ( [ ß ] = s )
  ( [ t ] = t )
  ( [ » ] = » )
  ( [ u ] = u )
  ( [ ú ] = ú )
  ( [ ù ] = ù )
  ( [ ü ] = y )
  ( [ v ] = v )
  ( [ w ] = w )
  ( [ x ] = x )
  ( [ y ] = y )
  ( [ ý ] = ý )
  ( [ z ] = z )
  ( [ ¾ ] = ¾ )
  ( [ A ] = a )
  ( [ Á ] = á )
  ( [ Ä ] = e )
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
  ( [ Ö ] = e )
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
  ( [ Ü ] = y )
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
  (I i y)
  (Í í ý)
  (Vowel a á e é ì i í o ó u ú ù y ý)
  (Vowel-I a á e é ì o ó u ú ù)
  (SZ s z))
 (
  ;; Special rules
  ( [ d ] i SZ m u = d )
  ( [ n ] i SZ m u = n )
  ( [ t ] i SZ m u = t )
  ( [ n ] i s t = n )
  ( [ t ] i s t = t )
  ( [ t ] i c k = t )
  ( [ t ] i è t ì = t )
  ( # a n [ t ] i = t )
  ( # a n t [ i ] Vowel = i )
  ( t e c h [ n ] i = n )
  ( [ d ] i s p = d )

  ( l i [ c ] o m = c )
  ( [ c ] o m = k )
  
  ( f r [ e e ] = i: )
  
  ( m l a [ d ] i s t = d~ )
  ( [ d ] i s t = d )
  
  ( # t r a [ t ] i v = t~ )
  ( á [ t ] i v = t~ )
  ( b o l e s [ t ] i v = t~ )
  ( c [ t ] i v = t~ )
  ( c e [ t ] i v = t~ )
  ( c h [ t ] i v = t~ )
  ( c h a m [ t ] i v = t~ )
  ( c h r a p [ t ] i v = t~ )
  ( c h r o p [ t ] i v = t~ )
  ( è [ t ] i v = t~ )
  ( d r [ t ] i v = t~ )
  ( ì [ t ] i v = t~ )
  ( f i n [ t ] i v = t~ )
  ( h l [ t ] i v = t~ )
  ( h o [ t ] i v = t~ )
  ( h ø m o [ t ] i v = t~ )
  ( í [ t ] i v = t~ )
  ( k l e v e [ t ] i v = t~ )
  ( k r o u [ t ] i v = t~ )
  ( o s [ t ] i v = t~ )
  ( p i [ t ] i v = t~ )
  ( p l e [ t ] i v = t~ )
  ( p o l [ t ] i v = t~ )
  ( r o [ t ] i v = t~ )
  ( s e [ t ] i v = t~ )
  ( s m r [ t ] i v = t~ )
  ( s o p [ t ] i v = t~ )
  ( ¹ [ t ] i v = t~ )
  ( v r [ t ] i v = t~ )
  ( y [ t ] i v = t~ )
  ( ¾ á d o s [ t ] i v = t~ )
  ( d i g e s [ t ] i v = t )
  ( f e s [ t ] i v = t )
  ( k o n t r a s [ t ] i v = t )
  ( r e z i s [ t ] i v = t )
  ( s u g e s [ t ] i v = t )
  ( s [ t ] i v = t~ )
  ( [ t ] i v = t )

  ;; Special orthography rules
  ( [ d ] ÌI = d~ )
  ( [ t ] ÌI = t~ )
  ( [ n ] ÌI = n~ )
  ( DTN [ ì ] = e )
  ( BPV [ ì ] = j e )
  ( m [ ì ] = n~ e )
  ;; `i' handling
  ( # m e z [ i ] Vowel = i _ )
  ( # [ I ] Vowel = j )
  ( [ I ] Vowel = i j )
  ( [ Í ] Vowel = i: j )
  ( Vowel [ I ] = j )
  ( Vowel [ Í ] = j i: )
  ;; Diphthongs
  ( [ a u ] = au )
  ( [ e u ] = eu )
  ( [ o u ] = ou )
  ;; Some vowel-vowel pairs
  ( m i m [ o ] Vowel = o _ )
  ( # m n o h [ o ] Vowel = o _ )
;   ( Vowel-I [ a ] = _ a )
;   ( Vowel-I [ e ] = _ e )
;   ( Vowel-I [ é ] = _ e: )
;   ( Vowel-I [ o ] = _ o )
;   ( Vowel-I [ ó ] = _ o: )
  ;; Other two-letter phonemes
  ( [ d ¾ ] = dz~ )
  ( [ d z ] = dz )
  ( [ c h ] = ch )
  ;; Special letters
  ( [ ì ] = j e )
  ( # [ ú ] = u: )
  ( b e z [ ú ] = _ u: )
  ( o [ ú ] = _ u: )
  ( [ ú ] h = _ u: )
  ( [ ú ] è e = _ u: )
  ( [ ú ] è t = _ u: )
  ( [ ú ] d r ¾ = _ u: )
  ( [ ú ] l o h = _ u: )
  ( [ ú ] r o è = _ u: )
  ( [ ú ] r o d = _ u: )
  ( [ ú ] r o v ò = _ u: )
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

(defvar czech-lts-extra-rules '())

(define (czech-basic-lts word)
  (lts.apply
   (lts.apply
    (if (lts.in.alphabet word 'czech-normalize)
        word
        czech-unknown-symbol-word)
    'czech-normalize)
   'czech))

(define (czech-syllabify-phstress phones)
  (if (null? phones)
      ()
      (list (list phones 0))))

(define (czech-lts word features)
  (list word
        nil
        (if (string-equal word "")
            '()
            (czech-syllabify-phstress
              (let ((transformed (czech-basic-lts word))
                    (rules czech-lts-extra-rules*))
                (while rules
                  (set! transformed (lts.apply transformed (car rules)))
                  (set! rules (cdr rules)))
                transformed)))))

(define (czech-downcase word)
  (if (lts.in.alphabet word 'czech-normalize)
      (apply string-append (lts.apply word 'czech-normalize))
      word))

;;; Tokenization

(defvar czech-token.unknown-word-name "neznámé")
(defvar czech-token.separator-word-name "oddìlovaè") ; our own variable
(defvar czech-token.garbage-word-name "smetí")       ; our own variable
(defvar czech-token.whitespace "  \t\n\r")
(defvar czech-token.punctuation "\"'`.,:;!?-(){}[]<>")
(defvar czech-token.prepunctuation "\"'`({[<")

;;; Token to words processing

(defvar czech-chars "a-zA-Záäèïéìíòóöø¹»úùüý¾ÁÄÈÏÉÌÍÒÓÖØ©«ÚÙÜÝ®")
(defvar czech-char-regexp (string-append "[" czech-chars "]"))

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
    ("=" ("rovná" "se"))
    ("\n" ("nový" "øádek"))
    ("os/2" ("OS" "2"))
    ("km/h" ("kilometrù" "za" "hodinu"))
    ("m/s" ("metrù" "za" "sekundu"))
    ))

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

(define (czech-prepend-numprefix token name)
  (if (czech-item.has-feat token 'numprefix)
      (string-append (item.feat token 'numprefix) name)
      name))

(define (czech-number* token name)
  (czech-number (czech-prepend-numprefix token name)))

(define (czech-number@ name)
  (cond
   ((string-equal name "0")
    '("nula"))
   ((string-equal name "00")
    '("nula" "nula"))
   ((string-matches name "0[1-9]")
    (cons "nula" (czech-number (string-after name "0"))))
   (t
    (czech-number name))))

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
            ((let ((all-zero t)
                   (d head-digits))
               (while (and all-zero d)
                 (if (string-equal (car d) "0")
                     (set! d (cdr d))
                     (set! all-zero nil)))
               all-zero)
             nil)
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
   ((string-matches string (string-append "^" czech-char-regexp "*$"))
    (list string))
   ((string-matches string "^[0-9]+$")
    (symbolexplode string))
   (t
    (let ((i 0))
      (while (string-matches (substring string i 1) czech-char-regexp)
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

(define (czech-token-to-words token name)
  (cond
   ;; Special terms
   ((assoc_string (czech-downcase name) czech-multiword-abbrevs)
    (apply append (mapcar (lambda (w) (czech-token-to-words token w))
                          (cadr (assoc_string (czech-downcase name)
                                              czech-multiword-abbrevs)))))
   ((and (string-matches name "[ckm]m")
         (item.prev token)
         (czech-item.feat*? token "p.name" "[-+]?[0-9]+[.,]?[0-9]*"))
    (list (cadr (assoc_string name '(("cm" "centimetrù") ("km" "kilometrù")
                                     ("mm" "milimetrù"))))))
   ;; Spaced numbers
   ((and (or (string-matches name "^[-+]?[1-9][0-9]?[0-9]?$")
             (czech-item.has-feat token 'numprefix))
         (not (czech-item.has-feat token 'punc))
         (item.feat token "n.whitespace" " ")
         (string-matches (item.feat token "n.name") "^[0-9][0-9][0-9]$"))
    (item.set_feat (item.next token) 'numprefix
                   (czech-prepend-numprefix token name))
    nil)
   ;; Ordinal numbers
   ((and (string-matches name "^[0-9]+$")
         (czech-item.feat? token 'punc ".")
         (item.next token)
         (not (string-matches (item.feat token "n.whitespace") "  +")))
    (if (not (czech-item.has-feat token 'punctype))
        (item.set_feat token 'punctype 'num))
    (append (czech-number* token name)
            (list ".")))
   ;; Numbers beginning with the zero digit
   ((and (string-matches name "^0[0-9]*$")
         (not (czech-item.has-feat token 'numprefix)))
    (apply append (mapcar czech-number (symbolexplode name))))
   ;; Any other numbers
   ((let ((nname (czech-prepend-numprefix token name)))
      (or (string-matches nname "^[-+]?[0-9]+$")
          (string-matches nname "^[-+]?[0-9]+[.,][0-9]+$")
          (string-matches nname "^[-+]?[0-9]+,-$")))
    (if (not (czech-item.has-feat token 'punctype))
        (item.set_feat token 'punctype 'num))
    (let ((nname (czech-prepend-numprefix token name)))
      (if (and (czech-item.feat? token "n.name" "Kè")
               (string-matches nname "^[-+]?[0-9]+,[-0-9]+$"))
          (append
           (czech-number (string-before nname ","))
           (list "korun")
           (let ((hellers (string-after nname ",")))
             (if (not (string-equal hellers "-"))
                 (append
                  (czech-number hellers)
                  (list "haléøù")))))
          (czech-number nname))))
   ;; Monetary sign
   ((and (string-equal name "Kè")
         (string-matches (item.feat token "p.name") "^[-+]?[0-9]+,[-0-9]+$"))
    nil)
   ;; Acronyms
   ((let ((capitals "^[A-ZÁÄÈÏÉÌÍÒÓÖØ©«ÚÙÜÝ®]+$"))
      (and (string-matches name capitals)
           (not (lex.lookup_all name))
           (not (string-matches (item.feat token "p.name") capitals))
           (not (string-matches (item.feat token "p.next") capitals))
           (<= (length name) 3) ; longer pronouncable acronyms are not spelled
           ))
    (mapcar (lambda (phoneme) `((name ,phoneme) (pos sym)))
            (lts.apply name 'czech-normalize)))
   ;; Abbreviations and other unpronouncable words
   ((and (string-matches
          name
          "^[bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQSTVWXZèïòø¹»¾ÈÏÒØ©«®][bcdfghjkmnpqstvwxzBCDFGHJKMNPQSTVWXZèïòø¹»¾ÈÏÒØ©«®]+$")
         (not (lex.lookup_all name)))
    (mapcar (lambda (phoneme) `((name ,phoneme) (pos sym)))
            (lts.apply name 'czech-normalize)))
   ;; Separators
   ((and (string-matches name (string-append "^[^" czech-chars "0-9]+$"))
         (>= (length name) 4)
         (czech-all-same (symbolexplode name)))
    (list czech-token.separator-word-name))
   ((and (string-matches name (string-append "^[^" czech-chars "0-9]$"))
         (eqv? (length (item.daughters token)) 0)
         (let ((punc (item.feat token 'punc)))
           (and (string-matches punc "...+") ; excludes, among others, punc==0
                (string-equal (substring punc 0 1) name)
                (czech-all-same (symbolexplode punc)))))
    (item.set_feat token 'punc 0)
    (list czech-token.separator-word-name))
   ;; Time (just a few of many possible forms)
   ((and (string-matches name "^[0-9]+:[0-9][0-9]$")
         ;; try to identify ratios -- should be better done in POS tagging
         (not (string-matches (item.feat token "p.name")
                              "^[Pp][Oo][Mm][Ìì].*"))
         (not (string-matches (item.feat token "p.name")
                              "^[Pp][Rr][Aa][Vv][Dd][Ìì][Pp][Oo][Dd][Oo].*"))
         (not (string-matches (item.feat token "p.name")
                              "^[©¹][Aa][Nn][Cc].*")))
    (append (czech-number@ (string-before name ":"))
            (czech-number@ (string-after name ":"))))
   ((string-matches name "^[0-9]+:[0-9][0-9]:[0-9][0-9]$")
    (append (czech-number@ (string-before name ":"))
            (czech-number@ (string-before (string-after name ":") ":"))
            (czech-number@ (string-after (string-after name ":") ":"))))
   ;; Ratios
   ((string-matches name "^[0-9]+:[0-9]+$")
    (append (czech-number (string-before name ":"))
            '("ku")
            (czech-number (string-after name ":"))))
   ;; Numeric ranges (might be minus as well, but that's rare)
   ((string-matches name "[0-9]+[.,]*[0-9]*-[0-9]+[.,]*[0-9]*$")
    ;; we don't include signs here not to break phone numbers and such a
    ;; written form is incorrect anyway
    (append
     (czech-token-to-words token (string-append
                                  (substring name 0 1)
                                  (string-before (substring name 1 1000) "-")))
     '(((name "-") (pos range)))
     (czech-token-to-words token (string-after (substring name 1 1000) "-"))))
   ;; Homogenous tokens
   ((string-matches name (string-append "^" czech-char-regexp "+$"))
    (list name))
   ((string-matches name (string-append "^[^" czech-chars "0-9]+$"))
    (cond
     ((> (length name) 10)
      (list czech-token.garbage-word-name))
     ((and (eqv? (length name) 1)
           (string-equal (item.name token) name)
           (or (not (string-matches (item.feat token 'prepunctuation) "0?"))
               (not (string-matches (item.feat token 'punctuation) "0?"))))
      ;; This handles the case when the whole token consists of two or more
      ;; punctuation characters.  In such a case Festival picks one of the
      ;; characters as the name, while the other characters are treated as
      ;; punctuation.  We want all the character being handled as punctuation.
      `(((name ,name) (pos punc))))
     ((assoc_string name czech-multiword-abbrevs)
      (cadr (assoc_string name czech-multiword-abbrevs)))
     (t
      (symbolexplode name))))
   ;; Hyphens
   ((string-matches name (string-append "^" czech-char-regexp "+-$"))
    (czech-token-to-words token (string-before name "-")))
   ((string-matches name
      (string-append "^" czech-char-regexp "+-[-" czech-chars "]+$"))
    (append
     (czech-token-to-words token (string-before name "-"))
     '(((name "-") (pos punc)))       ; necessary for punctuation reading modes
     (czech-token-to-words token (string-after name "-"))))
   ;; Lexicon words
   ((lex.lookup_all name)
    (list name))
   ;; TODO: roman numerals
   ;; Heterogenous tokens -- mixed alpha, numeric and non-alphanumeric
   ;; characters
   (t
    (if (not (string-matches name (string-append "^[-" czech-chars "]+$")))
        (item.set_feat token 'punctype nil))
    (apply
     append
     (mapcar (lambda (name) (czech-token-to-words token name))
             (czech-tokenize-on-nonalphas name))))))

;;; Lexicon

(defvar czech-lexicon-file "czech-lexicon.out")

(lex.create "czech")
(lex.set.phoneset "czech")
(lex.select "czech")
(let ((dirs '("." "/usr/share/festival"))
      (lexfile nil))
  (while dirs
    (let ((file (path-append (car dirs) czech-lexicon-file)))
      (if (probe_file file)
          (begin
            (set! lexfile file)
            (set! dirs nil))))
    (set! dirs (cdr dirs)))
  (if lexfile
      (lex.set.compile.file lexfile)
      (format t "warning: Czech lexicon file not found\n")))
(lex.set.lts.method 'czech-lts)
(lex.add.entry '("neznámé" nil (((n e z n a: m e:) 0))))

;;; Part of Speech

(defvar czech-guess-pos
  '((prep0 "k" "s" "v" "z")
    (prep "bez" "beze" "bìhem" "do" "ke" "ku" "krom" "kromì" "mezi" "mimo"
          "místo" "na" "nad" "nade" "o" "od" "ode" "okolo" "po" "pod" "pode"
          "pro" "proti" "pøed" "pøede" "pøes" "pøeze" "pøi" "se" "skrz"
          "skrze" "u" "ve" "vyjma" "za" "ze" "zpoza")
    (conj "a" "i" "ani" "nebo" "anebo")
    (particle "a»" "ké¾" "nech»")
    (question "co" "èemu" "èí" "jak" "jaká" "jaké" "jaký" "kam" "kde"
              "kdo" "kdy" "koho" "kolik" "kolikátá" "kolikáté" "kolikátý"
              "komu" "kterak" "která" "které" "kterého" "kterému" "který"
              "kudy" "naè" "nakolik" "odkud" "pokolikáté" "proè")
    (misc "aby" "abych" "abys" "abychom" "abyste" "ale" "alespoò" "aneb" "ani"
          "ani¾" "an¾to" "aspoò" "av¹ak" "aè" "a¾" "aèkoli" "aèkoliv" "buï"
          "buïto" "buïsi" "by" "by»" "by»si" "coby" "èi" "èili" "div"
          "dokdy" "dokonce" "dokud" "dotud" "jakby" "jakkoli" "jakkoliv"
          "jakmile" "jako" "jakoby" "jako¾" "jako¾to" "jednak" "jednou"
          "jeliko¾" "jen" "jenom" "jenom¾e" "jen¾e" "jestli" "jestli¾e" "je¹tì"
          "je¾to" "jinak" "kde¾to" "kdybych" "kdybys"
          "kdyby" "kdybychom" "kdybyste" "kdy¾" "kvùli"
          "leda" "leda¾e" "leè" "mezitímco" "mimoto" "naèe¾" "neb" "neboli"
          "nebo»" "nejen" "nejen¾e" "ne¾" "ne¾li" "neøkuli" "nicménì" "nýbr¾"
          "odkdy" "odkud" "pak" "pakli" "pakli¾e" "podle" "podmínky" "pokud"
          "ponìvad¾" "popøípadì" "potom" "potud" "poté" "proèe¾" "proto"
          "proto¾e" "právì" "pøece" "pøesto¾e" "pøitom" "respektive" "sic"
          "sice" "sotva" "sotva¾e" "tak" "takový" "taktak" "tak¾e" "také"
          "tedy" "ten" "teprve" "to" "toho" "tolik" "tomu" "toti¾" "tu" "tudí¾"
          "tím" "tøeba" "tøebas" "tøebas¾e" "tøeba¾e" "v¹ak" "v¾dy»" "zatímco"
          "zda" "zdali" "zejména" "zrovna" "zvlá¹tì" "¾e")))

(define (czech-word-pos? word pos)
  (member (item.name word)
          (apply append (mapcar (lambda (p) (cdr (assoc p czech-guess-pos)))
                                (if (consp pos) pos (list pos))))))

(define (czech-pos-in-phrase-from word)
  (let ((result 1)
        (w word))
    (while (and (item.prev w)
                (or (not (czech-item.feat*? w "R:Token.p.name" "0?"))
                    (and (czech-item.feat*? w "p.R:Token.parent.punc" "0?")
                         (czech-item.feat*? w "R:Token.parent.prepunctuation"
                                            "0?")
                         (not (czech-item.feat*?
                               w "p.name"
                               (string-append "^[^" czech-chars "0-9]+$"))))))
      (set! result (+ result 1))
      (set! w (item.prev w)))
    result))

(define (czech-pos-in-phrase-to word)
  (let ((result 1)
        (w word))
    (while (and (item.next w)
                (or (czech-item.feat*? w "R:Token.n.name" "0?")
                    (and (czech-item.feat*? w "R:Token.parent.punc" "0?")
                         (czech-item.feat*?
                          w "R:Token.parent.n.prepunctuation" "0?")
                         (not (czech-item.feat*?
                               w "n.name"
                               (string-append "^[^" czech-chars "0-9]+$"))))))
      (set! result (+ result 1))
      (set! w (item.next w)))
    result))

(define (czech-pos-last-in-phrase? word)
  (<= (czech-pos-in-phrase-to word) 1))

(define (czech-pos utt)
  (mapcar
   (lambda (w)
     (let ((name (czech-downcase (item.name w)))
           (token (item.parent (item.relation w 'Token))))
       (cond
        ;; Feature already assigned
        ((czech-item.has-feat w 'pos)
         nil)
        ;; Word followed by a punctuation
        ((and (czech-item.has-feat token 'punctype)
              (string-matches name (string-append "^[^" czech-chars "0-9]+$")))
         (item.set_feat w 'pos (item.feat token 'punctype)))
        ;; Punctuation
        ((member name '("\"" "'" "`" "-" "." "," ":" ";" "!" "?" "(" ")"))
         ;; Is it a separate punctuation character?
         (if (eqv? (length
                    (item.daughters (item.parent (item.relation w 'Token))))
                   1)
             (item.set_feat w 'pos nil)
             (item.set_feat w 'pos 'punc)))
        ;; Single letter, not in the role of a word
        ((and (eq? (string-length name) 1)
              (czech-pos-last-in-phrase? w))
         (item.set_feat w 'pos 'sym))
        ;; Word "se", not in the role of a preposition
        ((and (string-equal name "se")  ; the word "se"
              (item.prev w)             ; not the first word
              (or (czech-pos-last-in-phrase? w) ; final word
                  (czech-word-pos? (item.next w) '(prep0 prep))
                                        ; followed by a preposition
                  ))
         (item.set_feat w 'pos 'se))
        ;; Question words with the `pak' suffix
        ((and (string-matches name ".*pak")
              (member (substring name 0 (- (length name) 3))
                      (cdr (assoc 'question czech-guess-pos))))
         (item.set_feat w 'pos 'question))
        ;; Nothing special: check the czech-guess-pos tree
        (t
         (let ((pos-sets czech-guess-pos))
           (while pos-sets
             (if (member name (cdar pos-sets))
                 (begin
                   (item.set_feat w 'pos (caar pos-sets))
                   (set! pos-sets nil))
                 (set! pos-sets (cdr pos-sets)))))
         ))))
   (utt.relation.items utt 'Word))
  ;; Add commas before conjunctions
  (mapcar (lambda (token)
            (if (and (czech-item.feat*? token 'punc "0?")
                     (czech-item.feat? token "daughtern.R:Word.n.gpos" 'conj))
                (item.set_feat token 'punc ",")))
          (utt.relation.items utt 'Token))
  utt)

;;; Phrase breaks

(define (czech-next-simple-punc word)
  (let ((unit (item.next (czech-word-stress-unit word))))
    (cond
     ((not unit)
      0)
     ((string-matches (czech-stress-unit-punc unit) ".*[.?!;:,-]")
      (czech-stress-unit-punc unit))
     ((czech-item.feat? unit 'preelement 1)
      (czech-next-punc word))
     (t
      0))))

(define (czech-prev-simple-punc word)
  (let ((unit (item.prev (czech-word-stress-unit word))))
    (cond
     ((not unit)
      0)
     ((string-matches (czech-stress-unit-punc unit) ".*[.?!;:,-]")
      (czech-stress-unit-punc unit))
     (t
      (let ((token (item.prev (item.parent (item.relation word 'Token)))))
        (while (and token (not (string-matches (item.feat token 'punc) ".+")))
          (set! token (item.prev token)))
        (if (czech-item.feat? (czech-word-stress-unit
                               (item.daughter1 (item.next token)))
                              'preelement 1)
            (item.feat token 'punc)
            0))))))

(defvar czech-phrase-cart-tree
  ;; Note: Additional corrections are applied in czech-adjust-phrase-breaks
  ;; SB = (very) short break
  '(;; end of utterance
    (n.name is 0)
    ((BB))
    ;; exclude "punctuation words"
    ((name matches "[][\"'`.,:;!?(){}<>-]+")
     ((NB))
     ;; parentheses
     ((R:Token.parent.n.prepunctuation matches "(.*")
      ((R:Token.n.name is 0)
       ((B))
       ((NB)))
      ((lisp_czech-token-end-punc matches ".*)")
       ((B))
       ;;
       ;; phonetic rules
       ;;
       ;; "big" punctuations
       ((lisp_czech-token-end-punc matches ".*[.?!;]\"")
        ((BB))
        ((lisp_czech-token-end-punc matches ".*[.?!;]")
         ((lisp_czech-next-token-punc matches "\".*")
          ((BB))
          ((XB1)))                       ; for following adjustments
         ;; "smaller" punctuations
         ((lisp_czech-token-end-punc matches ".*[:-]")
          ;; dashes are treated as pbreaks only if separated by whitespaces
          ((R:Token.parent.n.daughter1.name is "-")
           ((R:Token.n.name is 0)
            ((B))
            ((NB)))
           ((B)))
          ;; "comma" punctuations
          ((lisp_czech-token-end-punc matches ".*,")
           ((XB2))                      ; for following adjustments
           ;; nothing applies -- no break by default
           ((NB)))))))))))

(define (czech-adjust-phrase-breaks utt)
  ;; This must be called after stress units are identified
  (mapcar (lambda (w)
            (cond
             ((czech-item.feat? w 'pbreak 'XB1) ; "big" punctuations
              ;; only one stress unit between punctuations makes them shorter
              (item.set_feat
               w 'pbreak
               (cond
                ((czech-item.feat? w "R:SylStructure.name" 0)
                 ;; not a word actually
                 'BB)
                ((or (czech-item.feat*? (czech-word-stress-unit w)
                                        "n.lisp_czech-stress-unit-punc"
                                        ".*[.?!;]\"?")
                     (czech-item.feat*? (czech-word-stress-unit w)
                                        "p.lisp_czech-stress-unit-punc"
                                        ".*[.?!;]\"?"))
                 'B)
                (t
                 'BB))))
             ((czech-item.feat? w 'pbreak 'XB2) ; "comma" punctuations
              ;; if only one stress unit separates from other punctuation or
              ;; the neighbor stress unit contains preelement, phrase break
              ;; *may* become shorter
              (item.set_feat
               w 'pbreak
               (cond
                ((czech-item.feat? w "R:SylStructure.name" 0)
                 ;; not a word actually
                 'B)
                ((czech-item.feat*? w "lisp_czech-next-simple-punc" ".*,")
                 'SB)
                ((czech-item.feat*? w "lisp_czech-prev-simple-punc" ".*,")
                 'B)
                ((czech-item.feat*? w "lisp_czech-prev-simple-punc"
                                    ".*[-.?!;:]\"?")
                 'SB)
                ((czech-item.feat*? (czech-word-stress-unit w)
                                    "n.lisp_czech-stress-unit-punc"
                                    ".*[-.?!;:]\"?")
                 'SB)
                (t
                 'B))))))
          (utt.relation.items utt 'Word)))

;;; Segmentation

(define (czech-intonation-units utt)
  ;; Mark syllables before phrase breaks
  (let ((token (utt.relation utt 'Token)))
    (while token
      (if (or (czech-item.feat*? token "daughtern.pbreak" "[SBX]?B[12]?")
              (czech-item.feat*? token "daughtern.p.pbreak" "[SBX]?B[12]?"))
          (let ((w (item.daughtern token)))
            (while (and w
                        (not (item.daughters (item.relation w 'SylStructure))))
              (set! w (item.prev w)))
            (if w
                (item.set_feat (item.daughtern (item.relation w 'SylStructure))
                               "sentence_break" 1))))
      (set! token (item.next token))))
  ;; Make the intonation units
  (utt.relation.create utt 'IntUnit)
  (let ((sylwords (utt.relation.items utt 'Syllable))
        (id 1)
        (unit-sylwords '()))
    (while sylwords
      (let ((w (car sylwords)))
        (set! unit-sylwords (cons w unit-sylwords))
        (set! sylwords (cdr sylwords))
        ;; If `w' is a last syllable before a relevant phrase break, make new
        ;; intonation unit
        (if (or (czech-item.feat*? w "sentence_break" 1)
                ;; This is the very last syllable (we reach this point when the
                ;; last token generates no words for whatever reason)
                (not (item.next w)))
            (begin
              (utt.relation.append
               utt 'IntUnit
               `("int" ((name ,(format nil "IUnit%d" id)))))
              (set! id (+ id 1))
              ;; Add the syllables to the intonation unit
              (let ((i (utt.relation.last utt 'IntUnit)))
                (set! unit-sylwords (reverse unit-sylwords))
                (while unit-sylwords
                  (item.append_daughter i (car unit-sylwords))
                  (set! unit-sylwords (cdr unit-sylwords))))))))))

(define (czech-yes-no-question int-unit)
  (and (string-matches (item.feat
                        int-unit
                        "daughtern.R:SylStructure.parent.R:Token.parent.punc")
                       ".*\\?")
       (not (czech-item.feat? int-unit
                              "daughter1.R:SylStructure.parent.R:Word.pos"
                              'question))
       (not (czech-item.feat? int-unit
                              "daughter2.R:SylStructure.parent.R:Word.pos"
                              'question))))

(defvar czech-proper-single-syl-prepositions
  '("bez" "do" "ke" "ku" "na" "nad" "o" "od" "po" "pod" "pro" "pøed" "pøes"
    "pøi" "se" "u" "ve" "za" "ze"))
(defvar czech-special-final-words
  '("ho" "je" "jej" "ji" "jsem" "jsi" "jste" "mì" "mi" "se" "si" "tì" "ti"))

(define (czech-syllable-kernels phonemes)
  (let ((kernels '()))
    (while phonemes
      ;; Starting syllabic consonant doesn't constitute syllable
      (if (and (czech-item.feat? (car phonemes) 'ph_vc '-)
               (czech-item.feat? (car phonemes) 'ph_syl '+))
          (set! phonemes (cdr phonemes)))
      ;; Skip non-syllabic consonants
      (while (and phonemes (czech-item.feat? (car phonemes) 'ph_syl '-))
        (set! phonemes (cdr phonemes)))
      (if phonemes
          ;; Now take the kernel
          (let ((kc '())
                (kv '()))
            (if (czech-item.feat? (car phonemes) 'ph_vc '-)
                (while (and phonemes
                            (czech-item.feat? (car phonemes) 'ph_vc '-)
                            (czech-item.feat? (car phonemes) 'ph_syl '+))
                  (set! kc (cons (car phonemes) kc))
                  (set! phonemes (cdr phonemes))))
            (while (and phonemes
                        (czech-item.feat? (car phonemes) 'ph_vc '+)
                        (czech-item.feat? (car phonemes) 'ph_syl '+))
              (set! kv (cons (car phonemes) kv))
              (set! phonemes (cdr phonemes)))
            (let ((k (reverse (or kv kc))))
              (let ((seg (and k (item.prev (car k)))))
                (while (and seg (czech-item.feat? seg 'ph_cvox '+))
                  (set! k (cons seg k))
                  (set! seg (item.prev seg))))
              (set! kernels (cons k kernels))))))
    (reverse kernels)))

(define (czech-syllable-count phonemes)
  (length (czech-syllable-kernels phonemes)))

(define (czech-stress-unit-phonemes unit)
  (if (and unit (not (consp unit)))
      (set! unit (item.daughters unit)))
  (apply append (mapcar (lambda (syl)
                          (if (not (eq? syl 'preelement))
                              (item.daughters
                               (item.relation syl 'SylStructure))))
                        unit)))

(define (czech-unit-syllable-count unit)
  (czech-syllable-count (czech-stress-unit-phonemes unit)))

(define (czech-identify-stress-units sylwords)
  (let ((units (mapcar list sylwords))
        (unit-word (lambda (unit)
                     (and (eqv? (length unit) 1)
                          (item.parent
                           (item.relation (car unit) 'SylStructure)))))
        (unit-word-name (lambda (unit)
                          (and (eqv? (length unit) 1)
                               (item.feat (car unit)
                                          "R:SylStructure.parent.name"))))
        (merge (lambda (list)
                 (set-car! list (append (car list) (cadr list)))
                 (set-cdr! list (cddr list)))))
    ;; Nothing to do if there is at most one word
    (if (<= (length units) 1)
        units
        (begin
          ;; Basic joining    
          (let ((units* units))
            (while units*
              (let ((w (unit-word (car units*))))
                (if (or ;; Join non-syllabic prepositions
                     (czech-item.feat? w 'pos 'prep0)
                     ;; Join proper single-syllabic prepositions
                     (and (member (czech-downcase (item.name w))
                                  czech-proper-single-syl-prepositions)
                          (not (czech-item.feat? w "pos" "se"))))
                    (merge units*)))
              (set! units* (cdr units*))))
          ;; At most 1 word now?
          (if (<= (length units) 1)
              units
              (let ((last-unit (car (last units))))
                ;; Final single-syllabic word
                (if (and (<= (czech-unit-syllable-count last-unit) 1)
                         (not (member (unit-word-name last-unit)
                                      czech-special-final-words)))
                    (set-cdr! (nth_cdr (- (length units) 2) units) '())
                    (set! last-unit '()))
                ;; Initial single-syllabic words
                (let ((units* units)
                      (singles '()))
                  (while (and units*
                              (<= (czech-unit-syllable-count (car units*)) 1))
                    (set! singles (cons (car units*) singles))
                    (set! units* (cdr units*)))
                  (set! singles (reverse singles))
                  (let ((len (length singles)))
                    (cond
                     ((<= len 0)
                      nil)
                     ((<= len 1)
                      (set! units (cons (append (car singles) '(preelement)
                                                (car units*))
                                        (cdr units*)))
                      (set! units* units))
                     ((<= len 4)
                      (set! units (cons (apply append singles) units*)))
                     (t
                      (let ((first-unit '())
                            (n (/ len 2))
                            (i 0))
                        (while (< i n)
                          (set! first-unit (append (car singles) first-unit))
                          (set! singles (cdr singles))
                          (set! i (+ i 1)))
                        (set! units (cons (reverse first-unit)
                                          (cons (apply append singles)
                                                units*)))))))
                  ;; Middle word processing
                  (while units*
                    (let ((u (car units*)))
                      ;; The word "a"
                      (if (string-equal (unit-word-name u) "a")
                          (merge units*))
                      ;; Single-syllabic words
                      (let ((len (czech-unit-syllable-count u))
                            (singles '())
                            (slen 0)
                            (next-units* (cdr units*)))
                        (while (and next-units*
                                    (<= (czech-unit-syllable-count
                                         (car next-units*)) 1)
                                    (not (string-equal
                                          (unit-word-name (car next-units*))
                                          "a")))
                          (set! singles (cons (car next-units*) singles))
                          (set! slen (+ slen 1))
                          (set! next-units* (cdr next-units*)))
                        (set! singles (reverse singles))
                        (let ((merge-n (lambda (n units)
                                         (while (> n 0)
                                           (merge units)
                                           (set! n (- n 1))))))
                          (cond
                           ((eqv? slen 0)
                            nil)
                           ((eqv? slen 1)
                            (merge units*))
                           ((eqv? slen 2)
                            (if (and (<= len 4)
                                     (czech-random-choice '(t nil)))
                                (merge-n 2 units*)
                                (merge (cdr units*))))
                           ((eqv? slen 3)
                            (if (<= len 3)
                                (merge-n 3 units*)
                                (merge-n 2 (cdr units*))))
                           ((eqv? slen 4)
                            (cond
                             ((>= len 5)
                              (merge-n 3 (cdr units*)))
                             ((and (<= len 2)
                                   (czech-random-choice '(t nil)))
                              (merge-n 4 units*))
                             (t
                              (merge-n 2 units*)
                              (merge-n 1 (cdr units*)))))
                           ((eqv? slen 5)
                            (cond
                             ((<= len 3)
                              (merge-n 2 units*)
                              (merge-n 2 (cdr units*)))
                             ((<= len 4)
                              (merge-n 1 (cdr units*))
                              (merge-n 2 (cddr units*)))
                             (t
                              (merge-n 2 (cdr units*))
                              (merge-n 1 (cddr units*)))))
                           ((eqv? slen 6)
                            (cond
                             ((>= len 4)
                              (merge-n 2 (cdr units*))
                              (merge-n 2 (cddr units*)))
                             ((czech-random-choice '(t nil))
                              (merge-n 2 units*)
                              (merge-n 3 (cdr units*)))
                             (t
                              (merge-n 2 units*)
                              (merge-n 1 (cdr units*))
                              (merge-n 1 (cddr units*)))))
                           (t
                            ;; This very rare case is not defined in the rules
                            (while (>= slen 4)
                              (merge-n 1 (cdr units*))
                              (set! units* (cdr units*))
                              (set! slen (- slen 2)))
                            (merge-n (- slen 1) (cdr units*))
                            ))
                          (set! units* next-units*)))))
                  ;; That's all
                  (if last-unit
                      (append units (list last-unit))
                      units))))))))

(define (czech-stress-units utt)
  (utt.relation.create utt 'IntStress)
  (utt.relation.create utt 'StressUnit)
  (let ((id 1)
        (int-unit (utt.relation.first utt 'IntUnit)))
    (while int-unit
      (let ((stress-units (czech-identify-stress-units
                           (item.daughters int-unit))))
        ;; Add the intonation unit at the top of the StressUnit relation
        (utt.relation.append utt 'IntStress int-unit)
        (while stress-units
          ;; Create new stress unit
          (item.relation.append_daughter int-unit 'IntStress
            `("stress" ((name ,(format nil "SUnit%d" id)) (position "M"))))
          (set! id (+ id 1))
          (utt.relation.append utt 'StressUnit
                               (item.relation.daughtern int-unit 'IntStress))
          ;; Fill it with its words
          (let ((i (utt.relation.last utt 'StressUnit)))
            (mapcar (lambda (syl)
                      (if (eq? syl 'preelement)
                          (item.set_feat i "preelement" 1)
                          (begin
                            (item.append_daughter i syl)
                            (let ((j (item.daughtern i)))
                              (mapcar (lambda (seg)
                                        (item.append_daughter j seg))
                                      (item.daughters syl))))))
                    (car stress-units)))
          (set! stress-units (cdr stress-units))))
      ;; The first stress unit in an intonation unit has position I
      (item.set_feat (item.relation.daughter1 int-unit 'IntStress)
                     "position" "I")
      ;; The last stress unit in an intonation unit has position F or FF
      ;; (overrides I in case of a conflict)
      (item.set_feat (item.relation.daughtern int-unit 'IntStress) "position"
       (if (string-matches
            (item.feat int-unit
                       "daughtern.R:SylStructure.parent.R:Token.parent.punc")
            ".*[.!?;:].*")
           (if (czech-yes-no-question int-unit) "FF-IT" "FF-KKL")
           "F"))
      ;; Special case: F-1 positions overriding I and M
      (if (not (equal? (item.relation.daughtern int-unit 'IntStress)
                       (item.relation.daughter1 int-unit 'IntStress)))
          (let ((last-pos (item.feat int-unit
                                     "R:IntStress.daughtern.position")))
            (item.set_feat (item.prev
                            (item.relation.daughtern int-unit 'IntStress))
                           "position" (string-append last-pos "-1"))))
      (set! int-unit (item.next int-unit)))))

(define (czech-word utt)
  (Classic_Word utt)
  (czech-intonation-units utt)
  (czech-stress-units utt)
  (czech-adjust-phrase-breaks utt)
  utt)

;;; Pauses

(define (czech-add-strokes utt)
  (let ((stroke '(_ (("name" _))))
        (i (utt.relation.first utt 'SylStructure)))
    (while i
      (if (or
           ;; Insert _ between vowels on word boundaries
           (and (czech-item.feat? i "daughter1.daughter1.ph_vc" '+)
                (czech-item.feat? i "p.daughtern.daughtern.ph_vc" '+)
                (not (czech-item.feat? i "daughter1.daughter1.R:Segment.p.name"
                                       '#)))
           ;; Insert _ between a non-syllabic preposition and a vowel
           (and (czech-item.feat? i "p.pos" 'prep0)
                (czech-item.feat? i "daughter1.daughter1.ph_vc" '+)
                (not (czech-item.feat? i "daughter1.daughter1.R:Segment.p.name"
                                       '#))))
          (item.insert
           (item.relation (item.daughter1 (item.daughter1 i)) 'Segment)
           stroke 'before))
      (set! i (item.next i)))))

(define (czech-pause-breaks utt)
  (Classic_Pauses utt)
  (let ((words (utt.relation.items utt 'Word)))
    ;; Handle SB -- Classic_Pauses doesn't know about it
    (mapcar
     (lambda (w)
       (if (czech-item.feat? w "pbreak" 'SB)
           (insert_pause utt w)))
     words)))

(define (czech-pause utt)
  (czech-pause-breaks utt)
  (czech-add-strokes utt)
  utt)

;;; Accents

(defvar czech-accent-cart-tree '(NONE))

;; Intonation

(defvar czech-int-contours
  '(((A 1) (0.02 -0.05) (0.02 -0.04) (0 0))
    ((B 1) (-0.01 0.02) (-0.02 0.04) (-0.02 0.05))
    ((C 1) (-0.04 -0.10) (0.02 -0.16) (-0.02 -0.12) (-0.02 -0.14))
    ((D 1) (-0.14 0.16) (-0.14 0.20))
    ((FA 1) (0.02 -0.04) (0 0))
    ((FB 1) (-0.02 0.04) (-0.02 0.05))
    ((A 2) (0.02 -0.05) (0.04 -0.08) (-0.03 0))
    ((B 2) (-0.04 0.06) (-0.02 0.04) (-0.02 0.07))
    ((C 2) (0 -0.10) (-0.04 -0.10) (-0.02 -0.12) (0.02 -0.16))
    ((D 2) (-0.06 0.08) (-0.10 0.14))
    ((FA 2) (0.04 -0.08) (-0.03 0))
    ((FB 2) (-0.02 0.04) (-0.02 0.07))
    ((A 3) (0.02 -0.02 -0.04) (0.02 -0.04 -0.02) (0.04 -0.04 -0.04)
           (0 0 -0.02) (0 -0.04 0) (-0.04 0.08 -0.10) (-0.04 0.04 -0.04)
           (-0.02 -0.01 0))
    ((B 3) (0 -0.04 0.04) (0 -0.06 0.04) (-0.06 0.04 0.02)
           (-0.01 0.04 0.02) (-0.06 0 0.06) (-0.06 0.02 0.04)
           (-0.04 0.04 -0.04))
    ((C 3) (0 -0.05 -0.05) (-0.04 -0.02 -0.08) (-0.06 -0.04 -0.04)
           (-0.06 -0.10 -0.02))
    ((D 3) (-0.06 -0.01 0.09) (-0.06 0.08 -0.01))
    ((FA 3) (-0.04 0.08 -0.10) (-0.04 0.04 -0.04) (-0.02 -0.01 0))
    ((FB 3) (-0.06 0 0.06) (-0.06 0.02 0.04) (-0.04 0.04 -0.04))
    ((A 4) (0 0 -0.02 -0.01) (-0.02 0 -0.03 0) (-0.03 0.03 -0.02 -0.01)
           (0 0 -0.01 0))
    ((B 4) (0 -0.03 0.01 0.02) (-0.02 0 0.02 0.02) (0 -0.03 0.03 0.02))
    ((C 4) (-0.04 -0.06 -0.02 -0.02) (-0.02 -0.02 -0.04 -0.06)
           (-0.02 -0.08 -0.04 -0.02))
    ((D 4) (-0.06 0 -0.01 0.12) (-0.06 0.12 0 -0.03))
    ((FA 4) (-0.03 0.03 -0.02 -0.01) (0 0 -0.01 0))
    ((FB 4) (-0.02 0 0.02 0.02) (0 -0.03 0.03 0.02))
    ((A 5) (-0.02 0.02 -0.02 -0.01 0) (-0.03 0.03 0 0 -0.03)
           (-0.02 0.02 0 0 -0.02))
    ((B 5) (0 -0.03 0.01 0.02 0.01) (0.01 -0.02 0 0 0.02)
           (-0.02 0 0.02 0.02 0))
    ((C 5) (-0.02 0 -0.02 -0.04 -0.06) (-0.02 -0.08 -0.02 -0.02 -0.02)
           (-0.02 -0.02 -0.08 -0.02 -0.02))
    ((D 5) (-0.06 0 -0.01 -0.01 0.13) (-0.06 0.13 0 -0.04 -0.04))
    ((FA 5) (-0.02 0.02 0 0 -0.02))
    ((FB 5)  (-0.02 0 0.02 0.02 0))
    ((A 6) (-0.02 0.02 -0.01 0 (0) -0.02 -0.01))
    ((B 6) (0 -0.01 0 0 (0) 0.01 0.01) (0 -0.02 0.01 0.01 (0) 0.01 0.02))
    ((C 6) (-0.02 0 -0.02 -0.04 -0.06 0 (0))
           (-0.02 -0.08 -0.02 -0.02 -0.02 (0))
           (-0.02 -0.02 -0.08 -0.02 -0.02 -0.02 (0)))
    ((D 6) (-0.06 0 -0.01 -0.01 0 (0) 0.13) (0.13 0 -0.02 0 (0) -0.04 -0.04))
    ((FA 6) (-0.02 0.02 -0.01 0 (0) -0.02 -0.01))
    ((FB 6) (0 -0.02 0.01 0.01 (0) 0.01 0.02))
    ))

(defvar czech-int-contour-tree
  ;; Contourtype set: A, B, C, D, FA and FB (for F and F-1 positions)
  '((position is I)
    ((preelement > 0)
     ((B))
     ((A)))
    ((position is M)
     ((p.contourtype is B)
      ((A))
      ((B)))
     ((position is F-1) ((FB))
      ((position is F) ((FA))
       ((position is FF-KKL-1) ((A))
        ((position is FF-KKL) ((C))
         ((position is FF-IT-1) ((B))
          ((position is FF-IT) ((D))
           ((ERROR)))))))))))

(define (czech-int-select-contours utt)
  (let ((unit (utt.relation utt 'StressUnit))
        (last-contour nil))
    (while unit
      (let ((position (item.feat unit 'position)))
        ;; Determine appropriate contour type
        (let ((contourtype (wagon_predict unit czech-int-contour-tree)))
          (item.set_feat unit "contourtype" contourtype)
          ;; Find particular contour
          (let ((nsyls (czech-unit-syllable-count unit)))
            (let ((contour (czech-random-choice
                            (cdr (assoc (list contourtype
                                              (if (<= nsyls 6) nsyls 6))
                                        czech-int-contours)))))
              ;; Adjust the first syllables of final contours
              (if (or (string-equal position "F")
                      (string-matches position "FF.*[A-Z]"))
                  (let ((adjust-contour
                         (lambda (c adj)
                           (if last-contour
                               (cons (+ (car (last last-contour)) adj) (cdr c))
                               c))))
                    (cond
                     ((string-equal position "F")
                      (set! contour (adjust-contour contour -0.02)))
                     ((string-equal position "FF-KKL")
                      (set! contour (adjust-contour contour 0.02)))
                     ((string-equal position "FF-IT")
                      (set! contour (adjust-contour contour -0.02))))))
              ;; Set contour values for preelements
              (if (czech-item.feat? unit 'preelement 1)
                  (set! contour (cons (- (car contour) 0.02) contour)))
              ;; Finalize contours of long units
              (let ((n (- nsyls 6)))
                (if (>= n 0)
                    (let ((prefix '())
                          (contour* contour))
                      (while (not (consp (car contour*)))
                        (set! prefix (cons (car contour*) prefix))
                        (set! contour* (cdr contour*)))
                      (let ((val (caar contour*)))
                        (set! contour* (cdr contour*))
                        (while (> n 0)
                          (set! contour* (cons val contour*))
                          (set! n (- n 1)))
                        (set! contour (append (reverse prefix)
                                              contour*))))))
              (set! last-contour contour)
              (item.set_feat unit "contour" contour)))))
      (set! unit (item.next unit)))
    ;; Spread the contours on sylwords
    (set! unit (utt.relation utt 'StressUnit))
    (while unit
      (let ((contour (item.feat unit "contour"))
            (kernels (czech-syllable-kernels
                      (czech-stress-unit-phonemes unit))))
        (if (eqv? (length kernels) 1)
            ;; One-syllabic units have two-number contours
            ;; (they can occur only in the final positions)
            (let ((k (car kernels))
                  (contour-1 (car contour))
                  (contour-2 (cadr contour)))
              (let ((k* (reverse k))
                    (last-k (car (last k))))
                (if (eqv? (length k) 1)
                    ;; Single phone in kernel -- put both values on it
                    (item.set_feat (car k) 'contourval contour)
                    ;; Multiple phones -- spread the values over true kernel
                    (begin
                      (while (czech-item.feat? (cadr k*) 'ph_vc '+)
                        (set! k* (cdr k*)))
                      (if (eq? (car k*) last-k)
                          (item.set_feat last-k 'contourval contour)
                          (begin
                            (item.set_feat (car k*) 'contourval contour-1)
                            (item.set_feat last-k 'contourval contour-2)))))
                ;; Extend the contour pair to certain neighbors
                (set! k* (cdr k*))
                (while k*
                  (item.set_feat (car k*) 'contourval contour-1)
                  (set! k* (cdr k*)))
                (let ((next-k (item.next last-k)))
                  (while (czech-item.feat? next-k 'ph_cvox '+)
                    (item.set_feat next-k 'contourval contour-2)
                    (set! next-k (item.next next-k))))))
            ;; Otherwise spread the contour value over all kernels
            (while kernels
              (let ((contourval (car contour)))
                (mapcar (lambda (seg)
                          (item.set_feat seg "contourval" contourval))
                        (car kernels)))
              (set! kernels (cdr kernels))
              (set! contour (cdr contour)))))
      (set! unit (item.next unit)))))

(defvar czech-int-simple-params '((f0_mean 100) (f0_std 10)))

(define (czech-int-targets utt syl)
  (let ((segments (item.relation.daughters syl 'SylStructure))
        (syl-start (item.feat syl 'syllable_start))
        (f0-base (cadr (assq 'f0_mean int_general_params)))
        (f0-std (/ (cadr (assq 'f0_std int_general_params)) 10))
        (times-values '()))
    (let ((last-seg-end syl-start)
          (f0-value (lambda (contourval)
                      (* f0-base (+ 1 (* f0-std contourval))))))
      (while segments
        (let ((s (car segments)))
          (let ((contourval (and (czech-item.has-feat s 'contourval)
                                 (item.feat s 'contourval)))
                (seg-end (item.feat s 'end)))
            (cond
             ((consp contourval)
              (let ((tlen (- seg-end last-seg-end))
                    (place 0.1))
                (set! times-values
                      (append
                       (list (list (+ last-seg-end (* (- 1.0 place) tlen))
                                   (f0-value (cadr contourval)))
                             (list (+ last-seg-end (* place tlen))
                                   (f0-value (car contourval))))
                       times-values))))
             (contourval
              (let ((time (/ (+ last-seg-end seg-end) 2.0))
                    (value (f0-value contourval)))
                (set! times-values (cons (list time value) times-values)))))
            (set! last-seg-end seg-end)
            (set! segments (cdr segments))))))
    (reverse times-values)))

;;; Duration

(defvar czech-phoneme-durations
  '(
    (#   0.10)
    (_   0.01)
    (@   0.02)
    (a   0.09)
    (a:  0.12)
    (au  0.10)
    (b   0.07)
    (c   0.07)
    (c~  0.07)
    (ch  0.07)
    (d   0.07)
    (d~  0.07)
    (e   0.08)
    (e:  0.11)
    (eu  0.10)
    (f   0.07)
    (g   0.07)
    (h   0.07)
    (i   0.07)
    (i:  0.10)
    (j   0.04)
    (k   0.07)
    (l   0.07)
    (m   0.07)
    (n   0.065)
    (n~  0.07)
    (o   0.09)
    (o:  0.12)
    (ou  0.10)
    (p   0.07)
    (r   0.07)
    (r~  0.07)
    (s   0.07)
    (s~  0.07)
    (t   0.07)
    (t~  0.07)
    (u   0.09)
    (u:  0.12)
    (v   0.035)
    (z   0.07)
    (z~  0.07)
    (dz  0.07)
    (dz~ 0.07)
    ))

(defvar czech-silence-durations
  '(("BB" 0.206 0.238) ("B" 0.082 0.095) ("SB" 0.008 0.010)))

(defvar czech-stress-duration-factors
  '((1  1.03)
    (2  1.02)
    (3  1.01)
    (4  1.00)
    (5  1.00)
    (6  0.99)
    (7  0.98)
    (8  0.96)
    (9  0.94)
    (10 0.93)
    (11 0.91)
    (12 0.90)))

(defvar czech-duration-random-factor 0.2)

(define (czech-duration utt)
  ;; Distinguish silence lengths
  (let ((word (utt.relation.first utt 'Word)))
    (while word
      
      (let ((durspec (assoc_string (item.feat word "pbreak")
                                   czech-silence-durations)))
        (if durspec
            (let ((min (nth 1 durspec))
                  (max (nth 2 durspec))
                  (seg (find_last_seg word)))
              (if seg
                  (item.set_feat
                   (item.next (item.relation seg 'Segment))
                   'dur_factor
                   (* 10 (+ min (* (- max min) (czech-rand)))))))))
      (set! word (item.next word))))
  ;; Set general duration factors
  (let ((sunit (utt.relation.first utt 'StressUnit)))
    (while sunit
      (let ((nphones (length (czech-stress-unit-phonemes sunit))))
        (cond
         ((> nphones 12)
          (set! nphones 12))
         ((< nphones 1)
          (set! nphones 1)))
        (let ((factor (cadr (assoc nphones czech-stress-duration-factors))))
          (mapcar (lambda (syl)
                    (mapcar (lambda (seg)
                              (item.set_feat seg "dur_factor" factor))
                            (item.relation.daughters syl 'SylStructure)))
                  (item.relation.leafs sunit 'StressUnit))))
      (set! sunit (item.next sunit))))
  ;; Adjust duration factors for initial single-syllabic word
  ;; (Take the initial word from Word, not just SylStructure, which may contain
  ;; prepunctuation.)
  (let ((1st-word (utt.relation.first utt 'Word)))
    (while (and 1st-word
                (item.daughter1 1st-word)
                (item.daughter1 (item.daughter1 1st-word)))
      (set! 1st-word (item.next 1st-word)))
    (let ((phonemes (and 1st-word
                         (apply append
                                (mapcar item.daughters
                                        (item.daughters
                                         (item.relation 1st-word
                                                        'SylStructure)))))))
      (if (eqv? (czech-syllable-count phonemes) 1)
          (let ((durfact (cadr (assoc (czech-min (length phonemes) 12)
                                      czech-stress-duration-factors))))
            (mapcar (lambda (ph) (item.set_feat ph 'dur_factor durfact))
                    phonemes)))))
  ;; Compute durations
  (mapcar
   (lambda (seg)
     (let ((factor (* (item.feat seg "dur_factor")
                      (Param.get 'Duration_Stretch))))
       (item.set_feat seg "end"
                      (+ (item.feat seg "start")
                         (* (if (<= factor 0) 1 factor)
                            (cadr (assoc_string (item.name seg)
                                                czech-phoneme-durations*)))))))
   (utt.relation.items utt 'Segment))
  utt)

;;; Volume

(defvar czech-volume-scale 1.8)
(defvar czech-volume-scale* nil)
  
(define (czech-adjust-volume utt)
  (utt.wave.rescale utt czech-volume-scale*))

;;; Final phoneme translation

(define (czech-translate-split-diphthongs utt)
  (if (string-equal (Param.get 'Language) 'czech)
      (let ((i (utt.relation.first utt 'Segment))
            (diphthong-vowels '((au a u) (eu e u) (ou o u)))
            (last-end 0.0))
        (while i
          (let ((end (item.feat i 'end)))
            (if (czech-item.feat? i 'ph_vlng 'd)
                (let ((vowels
                       (cdr (assoc_string (item.name i) diphthong-vowels))))
                  (item.insert i (cons (car vowels) (list (item.features i)))
                               'before)
                  (let ((new-item (item.prev i)))
                    (item.set_name new-item (car vowels))
                    (item.set_feat new-item 'end (/ (+ last-end end) 2))
                    (item.relation.insert i 'SylStructure new-item 'before))
                  (item.set_name i (cadr vowels))))
            (set! last-end end))
          (set! i (item.next i)))))
  utt)

(define (czech-translate-add-vowels utt)
  (if (and (string-equal (Param.get 'Language) 'czech)
           czech-insert-filling-vowels)
      (let ((i (utt.relation.first utt 'Segment))
            (diphthong-vowels '((au a) (eu e) (ou o)))
            (insert-item (lambda (name orig-ph end pos)
                           (let ((feats (item.features orig-ph))
                                 (new-feats `((name ,name) (end ,end))))
                             (while feats
                               (if (not (member (caar feats) '(id name end)))
                                   (set! new-feats (cons (car feats)
                                                         new-feats)))
                               (set! feats (cdr feats)))
                             (item.insert orig-ph (cons name (list new-feats))
                                          pos)
                             (let ((new ((if (eq? pos 'after)
                                             item.next item.prev)
                                         orig-ph)))
                               (if (member 'SylStructure
                                           (item.relations orig-ph))
                                   (item.relation.insert orig-ph 'SylStructure
                                                         new pos))))))
            (vowel? (lambda (ph) (czech-item.feat? ph 'ph_vc '+)))
            (last-end 0.0))
        (while i
          (let ((end (item.feat i 'end)))
            (cond
             ;; Duplicate both vowels of a diphthong
             ((czech-item.feat? i 'ph_vlng 'd)
              (let ((vowel (cadr
                            (assoc_string (item.name i) diphthong-vowels)))
                    (total-len (- end last-end)))
                (insert-item vowel i (+ last-end (* total-len 0.3)) 'before)
                (insert-item 'u i end 'after)
                (item.set_feat i 'end (+ last-end (* total-len 0.7))))
              (set! i (item.next i)))
             ;; Duplicate vowels
             ((vowel? i)
              (insert-item (item.name i) i (/ (+ last-end end) 2) 'before)))
            (set! last-end end))
          (set! i (item.next i)))))
  utt)

(define (czech-translate-phonemes utt)
  (if (and (string-equal (Param.get 'Language) 'czech)
           czech-phoneset-translation*)
      (mapcar
       (lambda (item)
         (let ((tr (assoc (item.name item) czech-phoneset-translation*)))
           (if tr (item.set_name item (cadr tr)))))
       (utt.relation.items utt 'Segment)))
  utt)

(defvar czech-after-analysis-hooks
  (list czech-translate-split-diphthongs czech-translate-add-vowels czech-translate-phonemes))

;;; Finally, the language definition itself

(define (czech-reset-parameters)
  (set! czech-lts-extra-rules* czech-lts-extra-rules)
  (set! czech-int-simple-params* czech-int-simple-params)
  (set! czech-phoneme-durations* czech-phoneme-durations)
  (set! czech-volume-scale* czech-volume-scale)
  (set! czech-phoneset-translation* czech-phoneset-translation)
  (set! czech-after-analysis-hooks* czech-after-analysis-hooks)
  (Param.set 'Synth_Method 'UniSyn))

(define (voice-czech-common)
  (voice_reset)
  (Param.set 'Language 'czech)
  ;; Phone set
  (Param.set 'PhoneSet 'czech)
  (PhoneSet.select 'czech)
  (set! pos_lex_name nil)
  ;; Tokenization
  (set! token.unknown_word_name czech-token.unknown-word-name)
  (set! token.whitespace czech-token.whitespace)
  (set! token.punctuation czech-token.punctuation)
  (set! token.prepunctuation czech-token.prepunctuation)
  (set! token_to_words czech-token-to-words)
  (Param.set 'Token_Method 'Token_Any)
  ;; Lexicon selection
  (lex.select "czech")
  ;; Segmentation
  (Param.set 'Word_Method 'czech-word)
  ;; Part of speech
  (set! guess_pos czech-guess-pos)      ; not actually used
  (Param.set 'POS_Method czech-pos)
  ;; Simple phrase break prediction by punctuation
  (set! pos_supported nil)
  (set! phrase_cart_tree czech-phrase-cart-tree)
  (Param.set 'Phrase_Method 'cart_tree)
  (Param.set 'Phrasify_Method Classic_Phrasify)
  ;; Pauses
  (Param.set 'Pause_Method czech-pause)
  ;; Accent prediction and intonation
  (set! int_accent_cart_tree czech-accent-cart-tree)
  (Param.set 'Int_Method czech-int-select-contours)
  (set! int_general_params (cons (list 'targ_func czech-int-targets)
                                 czech-int-simple-params*))
  (Param.set 'Int_Target_Method Int_Targets_General)
  ;; Duration prediction
  (Param.set 'Duration_Method czech-duration)
  ;; Postlex rules
  (set! postlex_rules_hooks '())
  (set! after_analysis_hooks czech-after-analysis-hooks*)
  ;; Final voice adjustment
  (set! after_synth_hooks (list czech-adjust-volume))
  ;; Set current voice
  (set! current-voice 'czech))

(defmac (czech-proclaim-voice form)
  (let ((name (nth 1 form))
        (description (nth 2 form))
        (body (nth_cdr 3 form))
        (options ()))
    (if (consp name)
        (begin
          (set! options (cdr name))
          (set! name (car name))))
    (set! name (intern (string-append 'czech_ name)))
    (let ((parameters `((language czech)
                        (dialect ,(cdr (assoc 'dialect options)))
                        (gender ,(cadr (assoc 'gender options)))
                        (coding ISO-8859-2)
                        (description ,description))))
      `(begin
         (define (,(intern (string-append 'voice_ name)))
           (czech-reset-parameters)
           ,@body
           (voice-czech-common)
           (set! current-voice (quote ,name)))
         (proclaim_voice
          (quote ,name)
          (quote ,parameters))))))

(provide 'czech)
