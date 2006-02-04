;;; Czech support for Festival

;; Copyright (C) 2003, 2004, 2005, 2006 Brailcom, o.p.s.

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

(define (czech-suffix string i)
  (substring string i (- (string-length string) i)))

(defvar czech-randomize t)

(defvar czech-rand-range nil)

(defvar czech-moravian t)

(defvar czech-insert-filling-vowels t)

(defvar czech-group-digits 3)

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
    (if token
        (item.feat token 'punc)
        0)))

(define (czech-prev-punc word)
  (let ((token (item.prev (item.parent (item.relation word 'Token)))))
    (while (and token (not (string-matches (item.feat token 'punc) "[^0]+")))
      (set! token (item.prev token)))
    (if token
        (item.feat token 'punc)
        0)))

(define (czech-word-stress-unit word)
  (let ((sylword (item.relation word 'SylStructure)))
    (if (and sylword (item.daughter1 sylword))
        (item.parent (item.relation (item.daughter1 sylword) 'StressUnit)))))

(define (czech-stress-unit-punc unit)
  (and unit
       (item.feat unit "daughtern.R:SylStructure.parent.R:Token.parent.punc")))

;;; Phone set

(defPhoneSet czech
  (;; vowel or consonant: vowel consonant
   (vc + - 0)
   ;; vowel length: short long
   (vlng s l 0)
   ;; consonant voicing: yes no unique
   (cvox + - u 0)
   ;; can create a syllable: yes no
   (syl + - 0)
   ;; can make previous consonant nasal: yes no
   (postnas + - 0)
   ;; voiced/unvoiced counterpart: phone
   (partner b c c~ ch d d~ dz dz~ f g h k p r~* s s~ t t~ v z z~ 0)
   )
  (
   ;;   c l v s n p
   (#   0 0 0 0 0 0)                    ; pause
   (_   0 0 0 - 0 0)                    ; vowel-vowel stroke
   (a   + s 0 + - 0)
   (a:  + l 0 + - 0)
   (b   - 0 + - - p)
   (c   - 0 - - - dz)
   (c~  - 0 - - - dz~)
   (ch  - 0 - - - 0)
   (d   - 0 + - - t)
   (d~  - 0 + - - t~)
   (dz  - 0 + - - c)
   (dz~ - 0 + - - c~)
   (e   + s 0 + - 0)
   (e:  + l 0 + - 0)
   (f   - 0 - - - v)
   (g   - 0 + - + k)
   (h   - 0 + - - ch)
   (i   + s 0 + - 0)
   (i:  + l 0 + - 0)
   (j   - 0 u - - 0)
   (k   - 0 - - + g)
   (l   - 0 u + - 0)
   (m   - 0 u - - 0)
   (n   - 0 u - - 0)
   (n*  - 0 u - - 0)                    ; n before k or g
   (n~  - 0 u - - 0)
   (o   + s 0 + - 0)
   (o:  + l 0 + - 0)
   (p   - 0 - - - b)
   (r   - 0 u + - 0)
   (r~  - 0 + - - r~*)                  ; (default) voiced r~, may change to r~*
   (r~* - 0 - - - 0)                    ; unvoiced r~, can't change back to r~
   (s   - 0 - - - z)
   (s~  - 0 - - - z~)
   (t   - 0 - - - d)
   (t~  - 0 - - - d~)
   (u   + s 0 + - 0)
   (u:  + l 0 + - 0)
   (v   - 0 + - - f)
   (z   - 0 + - - s)
   (z~  - 0 + - - s~)
  )
)
(PhoneSet.silences '(#))

(defvar czech-phoneset-translation '())
(defvar czech-phoneset-translation* nil)

;;; Text to phones

(lts.ruleset
 czech-normalize
 ;; just transforms the texts to a canonical form
 ()
 (
  ( [ a ] = a )
  ( [ � ] = � )
  ( [ � ] = e )
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ � ] = � )
  ( [ d ] = d )
  ( [ � ] = � )
  ( [ e ] = e )
  ( [ � ] = � )
  ( [ � ] = � )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ i ] = i )
  ( [ � ] = � )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ � ] = � )
  ( [ o ] = o )
  ( [ � ] = � )
  ( [ � ] = e )
  ( [ p ] = p )
  ( [ q ] = q )
  ( [ r ] = r )
  ( [ � ] = � )
  ( [ s ] = s )
  ( [ � ] = � )
  ( [ � ] = s )
  ( [ t ] = t )
  ( [ � ] = � )
  ( [ u ] = u )
  ( [ � ] = � )
  ( [ � ] = � )
  ( [ � ] = y )
  ( [ v ] = v )
  ( [ w ] = w )
  ( [ x ] = x )
  ( [ y ] = y )
  ( [ � ] = � )
  ( [ z ] = z )
  ( [ � ] = � )
  ( [ A ] = a )
  ( [ � ] = � )
  ( [ � ] = e )
  ( [ B ] = b )
  ( [ C ] = c )
  ( [ � ] = � )
  ( [ D ] = d )
  ( [ � ] = � )
  ( [ E ] = e )
  ( [ � ] = � )
  ( [ � ] = � )
  ( [ F ] = f )
  ( [ G ] = g )
  ( [ H ] = h )
  ( [ I ] = i )
  ( [ � ] = � )
  ( [ J ] = j )
  ( [ K ] = k )
  ( [ L ] = l )
  ( [ M ] = m )
  ( [ N ] = n )
  ( [ � ] = � )
  ( [ O ] = o )
  ( [ � ] = � )
  ( [ � ] = e )
  ( [ P ] = p )
  ( [ Q ] = q )
  ( [ R ] = r )
  ( [ � ] = � )
  ( [ S ] = s )
  ( [ � ] = � )
  ( [ T ] = t )
  ( [ � ] = � )
  ( [ U ] = u )
  ( [ � ] = � )
  ( [ � ] = � )
  ( [ � ] = y )
  ( [ V ] = v )
  ( [ W ] = w )
  ( [ X ] = x )
  ( [ Y ] = y )
  ( [ � ] = � )
  ( [ Z ] = z )
  ( [ � ] = � )
  ;; digits are here to make this rule set usable in some other cases
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
 czech-orthography
 ;; transforms Czech written text to a phonetic form
 ((BPV b p v)
  (DTN d t n)
  (�I � i �)
  (IY i y)
  (�� � �)
  (#_ # _)
  (Vowel a � e � � i � o � u � � y �)
  (Vowel+# a � e � � i � o � u � � y � #)
  (SZ s z))
 (
  ;; Special rules
  ( [ d ] i SZ m u = d )
  ( [ n ] i SZ m u = n )
  ( [ t ] i SZ m u = t )
  ( [ n ] i s t = n )
  ( [ t ] i s t = t )
  ( [ t ] i c k = t )
  ( [ t ] i � t � = t )
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
  ( � [ t ] i v = t~ )
  ( b o l e s [ t ] i v = t~ )
  ( c [ t ] i v = t~ )
  ( c e [ t ] i v = t~ )
  ( c h [ t ] i v = t~ )
  ( c h a m [ t ] i v = t~ )
  ( c h r a p [ t ] i v = t~ )
  ( c h r o p [ t ] i v = t~ )
  ( � [ t ] i v = t~ )
  ( d r [ t ] i v = t~ )
  ( � [ t ] i v = t~ )
  ( f i n [ t ] i v = t~ )
  ( h l [ t ] i v = t~ )
  ( h o [ t ] i v = t~ )
  ( h � m o [ t ] i v = t~ )
  ( � [ t ] i v = t~ )
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
  ( � [ t ] i v = t~ )
  ( v r [ t ] i v = t~ )
  ( y [ t ] i v = t~ )
  ( � � d o s [ t ] i v = t~ )
  ( d i g e s [ t ] i v = t )
  ( f e s [ t ] i v = t )
  ( k o n t r a s [ t ] i v = t )
  ( r e z i s [ t ] i v = t )
  ( s u g e s [ t ] i v = t )
  ( s [ t ] i v = t~ )
  ( [ t ] i v = t )

  ;; Special orthography rules
  ( [ d ] �I = d~ )
  ( [ t ] �I = t~ )
  ( [ n ] �I = n~ )
  ( DTN [ � ] = e )
  ( BPV [ � ] = j e )
  ( m [ � ] = n~ e )
  ;; `i' handling
  ( # m e z [ i ] Vowel = i _ )
  ( #_ [ IY ] #_ = i )
  ( Vowel+# [ IY ] Vowel+# = j )
  ( Vowel [ �� ] Vowel = j i: j )
  ( [ IY ] Vowel = i j )
  ( [ �� ] Vowel = i: j )
  ( Vowel [ IY ] = j )
  ( Vowel [ �� ] = j i: )
  ;; Some vowel-vowel pairs
  ( m i m [ o ] Vowel = o _ )
  ( # m n o h [ o ] Vowel = o _ )
  ;; Two-letter phonemes
  ( [ d � ] = dz~ )
  ( [ d z ] = dz )
  ( [ c h ] = ch )
  ;; Special letters
  ( [ � ] = j e )
  ( # [ � ] = u: )
  ( b e z [ � ] = _ u: )
  ( o [ � ] = _ u: )
  ( [ � ] h = _ u: )
  ( [ � ] � e = _ u: )
  ( [ � ] � t = _ u: )
  ( [ � ] d r � = _ u: )
  ( [ � ] l o h = _ u: )
  ( [ � ] r o � = _ u: )
  ( [ � ] r o d = _ u: )
  ( [ � ] r o v � = _ u: )
  ;; Simple letters
  ( [ a ] = a )
  ( [ � ] = a: )
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ � ] = c~ )
  ( [ d ] = d )
  ( [ � ] = d~ )
  ( [ e ] = e )
  ( [ � ] = e: )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ i ] = i )
  ( [ � ] = i: )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ � ] = n~ )
  ( [ o ] = o )
  ( [ � ] = o: )
  ( [ p ] = p )
  ( [ q ] = k v )
  ( [ r ] = r )
  ( [ � ] = r~ )
  ( [ s ] = s )
  ( [ � ] = s~ )
  ( [ t ] = t )
  ( [ � ] = t~ )
  ( [ u ] = u )
  ( [ � ] = u: )
  ( [ � ] = u: )
  ( [ v ] = v )
  ( [ w ] = v )
  ( [ x ] = k s )
  ( [ y ] = i )
  ( [ � ] = i: )
  ( [ z ] = z )
  ( [ � ] = z~ )
  ))


;; -- missing diphones: n-f n-g n-k
;; -- special diphones: a-a: a-e: a-o: a-u: a:-a a:-a: a:-e a:-e: a:-o a:-o:
;;                      a:-u a:-u: e-a: e-e: e-o: e-u: e:-a e:-a: atd.
;;;;

(defvar czech-unknown-symbol-word "nezn�m�")

(defvar czech-lts-extra-rules '())

(define (czech-basic-lts word)
  (let ((word (if (lts.in.alphabet word 'czech-normalize)
                  word
                  czech-unknown-symbol-word)))
    (if (string-equal word "")
        nil
        (let ((phonetic-form (lts.apply
                              (lts.apply word 'czech-normalize)
                              'czech-orthography))
              phonetic-form*)
          phonetic-form))))

(define (czech-syllabify-phstress phones)
  (if (null? phones)
      ()
      (list (list phones 0))))

(define (czech-lts word features)
  (list word
        nil
        (let ((transformed (and (not (string-equal word ""))
                                (czech-basic-lts word))))
          (if transformed
              (czech-syllabify-phstress
               (let ((rules czech-lts-extra-rules*))
                 (while rules
                   (set! transformed (lts.apply transformed (car rules)))
                   (set! rules (cdr rules)))
                 transformed))
              '()))))

(define (czech-downcase word)
  (if (lts.in.alphabet word 'czech-normalize)
      (apply string-append (lts.apply word 'czech-normalize))
      word))

;;; Tokenization

(defvar czech-token.unknown-word-name "nezn�m�")
(defvar czech-token.separator-word-name "odd�lova�") ; our own variable
(defvar czech-token.garbage-word-name "smet�")       ; our own variable
(defvar czech-token.whitespace " �\t\n\r")
(defvar czech-token.punctuation "\"'`.,:;!?-(){}[]<>")
(defvar czech-token.prepunctuation "\"'`({[<")

;;; Token to words processing

(defvar czech-chars "a-zA-Z����������������������������ة����ݮ")
(defvar czech-char-regexp (string-append "[" czech-chars "]"))

(defvar czech-multiword-abbrevs
  '(("�" ("dlouh�" "a"))
    ("�" ("dlouh�" "e"))
    ("�" ("dlouh�" "i"))
    ("�" ("dlouh�" "o"))
    ("�" ("dlouh�" "u"))
    ("�" ("u" "s" "krou�kem"))
    ("w" ("dvojit�" "v"))
    ("�" ("dlouh�" "y"))
    ("`" ("obr�cen�" "apostrof"))
    ("\\" ("zp�tn�" "lom�tko"))
    (">" ("v�t��" "ne�"))
    ("<" ("men��" "ne�"))
    ("[" ("lev�" "hranat�"))
    ("]" ("prav�" "hranat�"))
    ("{" ("lev�" "slo�en�"))
    ("}" ("prav�" "slo�en�"))
    ("(" ("lev�" "kulat�"))
    (")" ("prav�" "kulat�"))
    ("=" ("rovn�" "se"))
    ("\n" ("nov�" "��dek"))
    ("os/2" ("OS" "2"))
    ("km/h" ("kilometr�" "za" "hodinu"))
    ("m/s" ("metr�" "za" "sekundu"))
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
          (czech-number (czech-suffix name 1))))
   ((string-matches name ".*[,.].*")
    (let ((comma (if (string-matches name ".*,.*") "," ".")))
      (append (czech-number (string-before name comma))
              (list comma)
              (czech-number (string-after name comma)))))
   ((string-equal name "0")
    (list "nula"))
   ((string-matches name "^0..*")
    (cons "nula" (czech-number (czech-suffix name 1))))
   (t
    (czech-number-from-digits (czech-remove (car (symbolexplode " "))
                                            (symbolexplode name))))))

(define (czech-digits-1 digits)
  (if czech-group-digits
      (let ((n (string-length digits)))
        (while (> (- n czech-group-digits) 0)
          (set! n (- n czech-group-digits)))
        (append (czech-number (substring digits 0 n))
                (if (> (length digits) czech-group-digits)
                    (czech-digits (czech-suffix digits n))
                    nil)))
      (czech-number digits)))

(define (czech-digits digits)
  (cond
   ((string-equal digits "")
    '())
   ((string-matches digits "^0.*")
    (append (czech-number "0") (czech-digits (czech-suffix digits 1))))
   (t
    (czech-digits-1 digits))))

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
	 ((string-equal d "3") (list "t�i"))
	 ((string-equal d "4") (list "�ty�i"))
	 ((string-equal d "5") (list "p�t"))
	 ((string-equal d "6") (list "�est"))
	 ((string-equal d "7") (list "sedm"))
	 ((string-equal d "8") (list "osm"))
	 ((string-equal d "9") (list "dev�t")))))
     ((equal? len 2)
      (if (string-equal (car digits) "1")
	  (let ((d (car (cdr digits))))
	    (cond
	     ((string-equal d "0") (list "deset"))
	     ((string-equal d "1") (list "jeden�ct"))
	     ((string-equal d "2") (list "dvan�ct"))
	     ((string-equal d "3") (list "t�in�ct"))
	     ((string-equal d "4") (list "�trn�ct"))
	     ((string-equal d "5") (list "patn�ct"))
	     ((string-equal d "6") (list "�estn�ct"))
	     ((string-equal d "7") (list "sedmn�ct"))
	     ((string-equal d "8") (list "osmn�ct"))
	     ((string-equal d "9") (list "devaten�ct"))))
	  (append
	   (let ((d (car digits)))
	     (cond
	      ((string-equal d "0") ())
	      ((string-equal d "2") (list "dvacet"))
	      ((string-equal d "3") (list "t�icet"))
	      ((string-equal d "4") (list "�ty�icet"))
	      ((string-equal d "5") (list "pades�t"))
	      ((string-equal d "6") (list "�edes�t"))
	      ((string-equal d "7") (list "sedmdes�t"))
	      ((string-equal d "8") (list "osmdes�t"))
	      ((string-equal d "9") (list "devades�t"))))
	   (czech-number-from-digits (cdr digits)))))
     ((equal? len 3)
      (append
       (let ((d (car digits)))
	 (cond
	  ((string-equal d "0") ())
	  ((string-equal d "1") (list "sto"))
	  ((string-equal d "2") (list "dv�" "st�"))
	  ((string-equal d "3") (list "t�i" "sta"))
	  ((string-equal d "4") (list "�ty�i" "sta"))
	  ((string-equal d "5") (list "p�t" "set"))
	  ((string-equal d "6") (list "�est" "set"))
	  ((string-equal d "7") (list "sedm" "set"))
	  ((string-equal d "8") (list "osm" "set"))
	  ((string-equal d "9") (list "dev�t" "set"))))
       (czech-number-from-digits (cdr digits))))
     ((<= len 12)
      (let ((concatenations '((t "tis�c" "tis�ce" "tis�c")
			      (t "milion" "miliony" "milion�")
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
		(if (car words) "dva" "dv�"))
	       ((string-equal (car digits) "3") "t�i")
	       ((string-equal (car digits) "4") "�ty�i"))
	      (car (cdr (cdr words)))))
	    (t
	     (append
	      (czech-number-from-digits head-digits)
	      (list (car (cdr (cdr (cdr words))))))))
	   (czech-number-from-digits tail-digits)))))
     (t
      (if czech-group-digits
          (czech-digits (apply string-append digits))
          (apply append (mapcar czech-number digits)))))))

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
               (czech-suffix string (+ i 1))))))))

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
    (list (cadr (assoc_string name '(("cm" "centimetr�") ("km" "kilometr�")
                                     ("mm" "milimetr�"))))))
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
    (czech-digits name))
   ;; Any other numbers
   ((let ((nname (czech-prepend-numprefix token name)))
      (or (string-matches nname "^[-+]?[0-9]+$")
          (string-matches nname "^[-+]?[0-9]+[.,][0-9]+$")
          (string-matches nname "^[-+]?[0-9]+,-$")))
    (if (not (czech-item.has-feat token 'punctype))
        (item.set_feat token 'punctype 'num))
    (let ((nname (czech-prepend-numprefix token name)))
      (if (and (czech-item.feat? token "n.name" "K�")
               (string-matches nname "^[-+]?[0-9]+,[-0-9]+$"))
          (append
           (czech-number (string-before nname ","))
           (list "korun")
           (let ((hellers (string-after nname ",")))
             (if (not (string-equal hellers "-"))
                 (append
                  (czech-number hellers)
                  (list "hal���")))))
          (czech-number nname))))
   ;; Monetary sign
   ((and (string-equal name "K�")
         (string-matches (item.feat token "p.name") "^[-+]?[0-9]+,[-0-9]+$"))
    nil)
   ;; Acronyms
   ((let ((capitals "^[A-Z����������ة����ݮ]+$"))
      (and (string-matches name capitals)
           (not (lex.lookup_all name))
           (not (string-matches (item.feat token "p.name") capitals))
           (not (string-matches (item.feat token "p.next") capitals))
           (<= (length name) 3) ; longer pronouncable acronyms are not spelled
           (not (string-equal name "�")) ; Festival bug workaround
           ))
    (let ((words ()))
      (mapcar
       (lambda (phoneme)
         (let ((expansion (cadr (assoc_string (czech-downcase phoneme)
                                              czech-multiword-abbrevs))))
           (if expansion
               (set! words (append words
                                   (mapcar (lambda (w)
                                             `((name ,w) (pos sym)))
                                           expansion)))
               (set! words (append words
                                   (list `((name ,phoneme) (pos sym))))))))
       (lts.apply name 'czech-normalize))
      words))
   ;; Abbreviations and other unpronouncable words
   ((and (string-matches
          name
          "^[bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQSTVWXZ����������ة��][bcdfghjkmnpqstvwxzBCDFGHJKMNPQSTVWXZ����������ة��]+$")
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
                              "^[Pp][Oo][Mm][��].*"))
         (not (string-matches (item.feat token "p.name")
                              "^[Pp][Rr][Aa][Vv][Dd][��][Pp][Oo][Dd][Oo].*"))
         (not (string-matches (item.feat token "p.name")
                              "^[��][Aa][Nn][Cc].*")))
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
    (if (string-equal (czech-downcase name) "�") ; Festival bug workaround
        (list "e�")
        (list name)))
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
      (string-append "^[" czech-chars "0-9]+-[-" czech-chars "0-9]+$"))
    (append
     (czech-token-to-words token (string-before name "-"))
     '(((name "-") (pos punc)))       ; necessary for punctuation reading modes
     (czech-token-to-words token (string-after name "-"))))
   ;; Starting with digits
   ((string-matches name "^[0-9].*")
    (let ((i 0))
      (while (member (substring name i 1)
                     '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
        (set! i (+ i 1)))
      (append (czech-digits (substring name 0 i))
              (czech-token-to-words token (czech-suffix name i)))))
   ;; Digits inside
   ((string-matches name "^.*[0-9].*")
    (let ((i 0)
          j
          (digits '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
      (while (not (member (substring name i 1) digits))
        (set! i (+ i 1)))
      (set! j (+ i 1))
      (while (member (substring name j 1) digits)
        (set! j (+ j 1)))
      (append (czech-token-to-words token (substring name 0 i))
              (czech-digits (substring name i (- j i)))
              (czech-token-to-words token (czech-suffix name j)))))
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
(lex.add.entry '("nezn�m�" nil (((n e z n a: m e:) 0))))

;;; Part of Speech

(defvar czech-guess-pos
  '((prep0 "k" "s" "v" "z")
    (prep "bez" "beze" "b�hem" "do" "ke" "ku" "krom" "krom�" "mezi" "mimo"
          "m�sto" "na" "nad" "nade" "o" "od" "ode" "okolo" "po" "pod" "pode"
          "pro" "proti" "p�ed" "p�ede" "p�es" "p�eze" "p�i" "se" "skrz"
          "skrze" "u" "ve" "vyjma" "za" "ze" "zpoza")
    (conj "a" "i" "ani" "nebo" "anebo")
    (particle "a�" "k�" "nech�")
    (question "co" "�emu" "��" "jak" "jak�" "jak�" "jak�" "kam" "kde"
              "kdo" "kdy" "koho" "kolik" "kolik�t�" "kolik�t�" "kolik�t�"
              "komu" "kterak" "kter�" "kter�" "kter�ho" "kter�mu" "kter�"
              "kudy" "na�" "nakolik" "odkud" "pokolik�t�" "pro�")
    (misc "aby" "abych" "abys" "abychom" "abyste" "ale" "alespo�" "aneb" "ani"
          "ani�" "an�to" "aspo�" "av�ak" "a�" "a�" "a�koli" "a�koliv" "bu�"
          "bu�to" "bu�si" "by" "by�" "by�si" "coby" "�i" "�ili" "div"
          "dokdy" "dokonce" "dokud" "dotud" "jakby" "jakkoli" "jakkoliv"
          "jakmile" "jako" "jakoby" "jako�" "jako�to" "jednak" "jednou"
          "jeliko�" "jen" "jenom" "jenom�e" "jen�e" "jestli" "jestli�e" "je�t�"
          "je�to" "jinak" "kde�to" "kdybych" "kdybys"
          "kdyby" "kdybychom" "kdybyste" "kdy�" "kv�li"
          "leda" "leda�e" "le�" "mezit�mco" "mimoto" "na�e�" "neb" "neboli"
          "nebo�" "nejen" "nejen�e" "ne�" "ne�li" "ne�kuli" "nicm�n�" "n�br�"
          "odkdy" "odkud" "pak" "pakli" "pakli�e" "podle" "podm�nky" "pokud"
          "pon�vad�" "pop��pad�" "potom" "potud" "pot�" "pro�e�" "proto"
          "proto�e" "pr�v�" "p�ece" "p�esto�e" "p�itom" "respektive" "sic"
          "sice" "sotva" "sotva�e" "tak" "takov�" "taktak" "tak�e" "tak�"
          "tedy" "ten" "teprve" "to" "toho" "tolik" "tomu" "toti�" "tu" "tud�"
          "t�m" "t�eba" "t�ebas" "t�ebas�e" "t�eba�e" "v�ak" "v�dy�" "zat�mco"
          "zda" "zdali" "zejm�na" "zrovna" "zvl�t�" "�e")))

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
        (let ((pword (and token
                          (item.next token)
                          (item.daughter1 (item.next token)))))
          (if (and pword
                   (czech-item.feat? (czech-word-stress-unit pword)
                                     'preelement 1))
              (item.feat token 'punc)
              0)))))))

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
      ((lisp_token_end_punc matches ".*)")
       ((B))
       ;;
       ;; phonetic rules
       ;;
       ;; "big" punctuations
       ((lisp_token_end_punc matches ".*[.?!;]\"")
        ((BB))
        ((lisp_token_end_punc matches ".*[.?!;]")
         ((lisp_czech-next-token-punc matches "\".*")
          ((BB))
          ((XB1)))                       ; for following adjustments
         ;; "smaller" punctuations
         ((lisp_token_end_punc matches ".*[:-]")
          ;; dashes are treated as pbreaks only if separated by whitespaces
          ((R:Token.parent.n.daughter1.name is "-")
           ((R:Token.n.name is 0)
            ((B))
            ((NB)))
           ((B)))
          ;; "comma" punctuations
          ((lisp_token_end_punc matches ".*,")
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

(define (czech-adjust-segments segments)
  (if (not (null? segments))
      (let ((item1 (nth 0 segments))
            (item2 (nth 1 segments))
            (item3 (nth 2 segments))
            (item-word (lambda (i)
                         (item.parent
                          (item.parent
                           (item.relation i 'SylStructure))))))
        (let ((name1 (and item1 (item.name item1)))
              (name2 (and item2 (item.name item2)))
              (name3 (and item3 (item.name item3)))
              (same-word? (lambda (i1 i2)
                            (equal? (item-word i1) (item-word i2)))))
          ;; nasals
          (if (and (string-equal name1 "n")
                   (czech-item.feat? item2 "ph_postnas" '+)
                   (same-word? item1 item2))
              (item.set_name item1 "n*"))
          ;; sh
          (if (and (string-equal name1 "s")
                   (string-equal name2 "h")
                   (same-word? item1 item2))
              (if czech-moravian
                  (item.set_name item1 "z")
                  (item.set_name item2 "ch")))
          ;; unvoiced-r~
          (if (and (string-equal name2 "r~")
                   (czech-item.feat? item1 "ph_cvox" '-)
                   (same-word? item1 item2))
              (item.set_name item2 "r~*"))
          ;; voiced-unvoiced
          (if (and (czech-item.feat? item1 "ph_cvox" '+)
                   (not (czech-item.feat? item1 "ph_partner" 0))
                   item2
                   (or (string-equal name2 "#")
                       (string-equal name2 "_")
                       (czech-item.feat? item2 "ph_cvox" '-)
                       (and (czech-item.feat? item2 "ph_cvox" 'u)
                            (not (same-word? item1 item2))
                            (not (member
                                  (item.name (item-word item1))
                                  (append
                                   (list "v" "z")
                                   czech-proper-single-syl-prepositions))))))
              (item.set_name item1 (item.feat item1 "ph_partner")))
          ;; unvoiced-voiced
          (if (and (czech-item.feat? item1 "ph_cvox" '-)
                   (not (czech-item.feat? item1 "ph_partner" 0))
                   item2
                   (czech-item.feat? item2 "ph_cvox" '+)
                   (not (string-equal name2 "v"))
                   (not (string-equal name2 "r~")))
              (item.set_name item1 (item.feat item1 "ph_partner"))))
        (czech-adjust-segments (cdr segments)))))

(define (czech-adjust-phonetic-form utt)
  (let ((items (utt.relation.items utt 'Segment)))
    (let ((names (mapcar item.name items))
          (old-names '()))
      (while (not (equal? old-names names))
        (czech-adjust-segments items)
        (set! old-names names)
        (set! names (mapcar item.name (utt.relation.items utt 'Segment))))))
  utt)

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
  '("bez" "do" "ke" "ku" "na" "nad" "o" "od" "po" "pod" "pro" "p�ed" "p�es"
    "p�i" "se" "u" "ve" "za" "ze"))
(defvar czech-special-final-words
  '("ho" "je" "jej" "ji" "jsem" "jsi" "jste" "m�" "mi" "se" "si" "t�" "ti"))

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
                (while (and seg (or (czech-item.feat? seg 'ph_cvox '+)
                                    (czech-item.feat? seg 'ph_cvox 'u)))
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
      ;; Insert _ before vowels at the beginning of word boundaries
      (if (and (czech-item.feat? i "daughter1.daughter1.ph_vc" '+)
               (item.prev i)
               (not (czech-item.feat? i "daughter1.daughter1.R:Segment.p.name"
                                      '#)))
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
  (czech-adjust-phonetic-form utt)
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
              (item.set_feat unit 'contour contour)))))
      (set! unit (item.next unit)))
    ;; Spread the contours on sylwords
    (set! unit (utt.relation utt 'StressUnit))
    (while unit
      (let ((contour (item.feat unit 'contour))
            (kernels (czech-syllable-kernels
                      (czech-stress-unit-phonemes unit))))
        (if (eqv? (length kernels) 1)
            ;; One-syllabic units have two-number contours
            ;; (they can occur only in the final positions)
            (let ((k (car kernels))
                  (contour-1 (car contour))
                  (contour-2 (cadr contour)))
              (let ((k* (reverse k))
                    (last-k (car (last k)))
                    (contour-list (list (list 0.1 contour-1)
                                        (list 0.9 contour-2))))
                (if (eqv? (length k) 1)
                    ;; Single phone in kernel -- put both values on it
                    (item.set_feat (car k) 'contourval contour-list)
                    ;; Multiple phones -- spread the values over true kernel
                    (begin
                      (while (czech-item.feat? (cadr k*) 'ph_vc '+)
                        (set! k* (cdr k*)))
                      (if (eq? (car k*) last-k)
                          (item.set_feat last-k 'contourval contour-list)
                          (begin
                            (item.set_feat (car k*) 'contourval contour-1)
                            (item.set_feat last-k 'contourval contour-2)))))
                ;; Extend the contour pair to certain neighbors
                (set! k* (cdr k*))
                (while k*
                  (item.set_feat (car k*) 'contourval contour-1)
                  (set! k* (cdr k*)))
                (let ((next-k (item.next last-k)))
                  (while (or (czech-item.feat? next-k 'ph_cvox '+)
                             (czech-item.feat? next-k 'ph_cvox 'u))
                    (item.set_feat next-k 'contourval contour-2)
                    (set! next-k (item.next next-k))))))
            ;; Otherwise spread the contour value over all kernels
            (while kernels
              (let ((contourval (car contour)))
                (mapcar (lambda (seg)
                          (item.set_feat seg 'contourval contourval))
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
              (let ((tlen (- seg-end last-seg-end)))
                (set! times-values
                      (append
                       (mapcar (lambda (v)
                                 (list (+ last-seg-end
                                          (* (read-from-string (car v)) tlen))
                                       (f0-value (cadr v))))
                               (reverse contourval))
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
  '((#   0.100)
    (_   0.025)
    (a   0.098)
    (a:  0.142)
    (b   0.067)
    (c   0.102)
    (ch  0.087)
    (c~  0.099)
    (d   0.062)
    (dz  0.108)
    (dz~ 0.094)
    (d~  0.077)
    (e   0.099)
    (e:  0.126)
    (f   0.089)
    (g   0.067)
    (h   0.064)
    (i   0.077)
    (i:  0.120)
    (j   0.065)
    (k   0.080)
    (l   0.057)
    (m   0.068)
    (n   0.075)
    (n*  0.098)
    (n~  0.079)
    (o   0.089)
    (o:  0.137)
    (p   0.079)
    (r   0.060)
    (r~  0.065)
    (r~* 0.073)
    (s   0.098)
    (s~  0.090)
    (t   0.082)
    (t~  0.090)
    (u   0.082)
    (u:  0.139)
    (v   0.058)
    (z   0.077)
    (z~  0.074)
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

(define (czech-duration-pauses utt)
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
      (set! word (item.next word)))))

(define (czech-duration-factors utt)
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
                    phonemes))))))

(define (czech-duration-compute utt)
  (mapcar
   (lambda (seg)
     (let ((factor (* (item.feat seg "dur_factor")
                      (Param.get 'Duration_Stretch))))
       (item.set_feat seg "end"
                      (+ (item.feat seg "start")
                         (* (if (<= factor 0) 1 factor)
                            (cadr (assoc_string (item.name seg)
                                                czech-phoneme-durations*)))))))
   (utt.relation.items utt 'Segment)))

(define (czech-duration utt)
  (czech-duration-pauses utt)
  (czech-duration-factors utt)
  (czech-duration-compute utt)
  utt)

;;; Volume

(defvar czech-volume-scale 1.8)
(defvar czech-volume-scale* nil)
  
(define (czech-adjust-volume utt)
  (utt.wave.rescale utt czech-volume-scale*))

;;; Final phoneme translation

(define (czech-translate-add-vowels utt)
  (if (and (string-equal (Param.get 'Language) 'czech)
           czech-insert-filling-vowels)
      (let ((i (utt.relation.first utt 'Segment))
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
  (list czech-translate-add-vowels czech-translate-phonemes))

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
  (set! current_voice_reset nil)
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
