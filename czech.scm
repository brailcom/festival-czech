;;; Czech support for Festival

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

;; Some data were created using the data files and tools contained in the
;; ispell-czech package available under GPL at
;; ftp://ftp.vslib.cz/pub/unix/ispell/czech.


;;; Utility functions

(define (czech-item.has_feat item feat)
  (assoc feat (item.features item)))

(define (czech-all-same lst)
  (or (<= (length lst) 1)
      (and (string-equal (car lst) (cadr lst))
           (czech-all-same (cdr lst)))))

;;; Phone set

(defPhoneSet czech
  (;; The properties try to be as much close as possible to the phone
   ;; properties defined in radio_phones.scm; so that they work with English
   ;; prosody rules.  Also, there should be no two phones with the same
   ;; properties present.  Correspondence to real phone properties is less
   ;; important in the context of the previous rules.
   
   ;; vowel or consonant: vowel consonant
   (vc + - 0)
   ;; vowel length: short long dipthong schwa
   (vlng s l d a 0)
   ;; vowel height: low mid high
   (vheight 1 2 3 0)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 0)
   ;; vowellip rounding: yes no
   (vrnd + - 0)
   ;; consonant type: stop fricative affricate nasal lateral approximant
   (ctype s f a n l r 0)
   ;; place of articulation: labial alveolar palatal labio-dental dental velar
   ;;                        glottal other
   (cplace l a p b d v g o 0)
   ;; consonant voicing: yes no
   (cvox + - 0)
   )
  (
   ;;   c l h f r t p v
   (#   0 0 0 0 0 0 0 0)
   (a   + s 3 2 - 0 0 0)
   (a:  + l 3 2 - 0 0 0)
   (b   - 0 0 0 0 s l +)
   (c   - 0 0 0 0 a a -)
   (ch  - 0 0 0 0 f g -)
   (c~  - 0 0 0 0 a p -)
   (d   - 0 0 0 0 s a +)
   (d~  - 0 0 0 0 a o +)
   (dz  - 0 0 0 0 a a +)
   (dz~ - 0 0 0 0 a p +)
   (e   + s 2 1 - 0 0 0)
   (e:  + l 2 1 - 0 0 0)
   (f   - 0 0 0 0 f b -)
   (g   - 0 0 0 0 s v +)
   (h   - 0 0 0 0 f g +)
   (i   + s 1 1 - 0 0 0)
   (i:  + l 1 1 - 0 0 0)
   (j   - 0 0 0 0 r p +)
   (k   - 0 0 0 0 s v -)
   (l   - 0 0 0 0 l a +)
   (m   - 0 0 0 0 n l +)
   (n   - 0 0 0 0 n a +)
   (n~  - 0 0 0 0 n p +)
   (o   + s 2 3 + 0 0 0)
   (o:  + l 2 3 + 0 0 0)
   (p   - 0 0 0 0 s l -)
   (r   - 0 0 0 0 r a +)
   (r~  - 0 0 0 0 r o +)
   (s   - 0 0 0 0 f a -)
   (s~  - 0 0 0 0 f p -)
   (t   - 0 0 0 0 s a -)
   (t~  - 0 0 0 0 a o -)
   (u   + s 1 3 + 0 0 0)
   (u:  + l 1 3 + 0 0 0)
   (v   - 0 0 0 0 f b +)
   (z   - 0 0 0 0 f a +)
   (z~  - 0 0 0 0 f p +)
  )
)
(PhoneSet.silences '(#))

(defvar czech-phoneset-translation* nil)

;;; Text to phones

(lex.create "czech")
(lex.set.phoneset "czech")

(lts.ruleset
 czech-normalize
 ()
 (
  ( [ a ] = a )
  ( [ · ] = · )
  ( [ ‰ ] = e )
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ Ë ] = Ë )
  ( [ d ] = d )
  ( [ Ô ] = Ô )
  ( [ e ] = e )
  ( [ È ] = È )
  ( [ Ï ] = Ï )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ i ] = i )
  ( [ Ì ] = Ì )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ Ú ] = Ú )
  ( [ o ] = o )
  ( [ Û ] = Û )
  ( [ ˆ ] = e )
  ( [ p ] = p )
  ( [ q ] = q )
  ( [ r ] = r )
  ( [ ¯ ] = ¯ )
  ( [ s ] = s )
  ( [ π ] = π )
  ( [ t ] = t )
  ( [ ª ] = ª )
  ( [ u ] = u )
  ( [ ˙ ] = ˙ )
  ( [ ˘ ] = ˘ )
  ( [ ¸ ] = y )
  ( [ v ] = v )
  ( [ w ] = w )
  ( [ x ] = x )
  ( [ y ] = y )
  ( [ ˝ ] = ˝ )
  ( [ z ] = z )
  ( [ æ ] = æ )
  ( [ A ] = a )
  ( [ ¡ ] = · )
  ( [ ƒ ] = e )
  ( [ B ] = b )
  ( [ C ] = c )
  ( [ » ] = Ë )
  ( [ D ] = d )
  ( [ œ ] = Ô )
  ( [ E ] = e )
  ( [ … ] = È )
  ( [ Ã ] = Ï )
  ( [ F ] = f )
  ( [ G ] = g )
  ( [ H ] = h )
  ( [ I ] = i )
  ( [ Õ ] = Ì )
  ( [ J ] = j )
  ( [ K ] = k )
  ( [ L ] = l )
  ( [ M ] = m )
  ( [ N ] = n )
  ( [ “ ] = Ú )
  ( [ O ] = o )
  ( [ ” ] = Û )
  ( [ ÷ ] = e )
  ( [ P ] = p )
  ( [ Q ] = q )
  ( [ R ] = r )
  ( [ ÿ ] = ¯ )
  ( [ S ] = s )
  ( [ © ] = π )
  ( [ T ] = t )
  ( [ ´ ] = ª )
  ( [ U ] = u )
  ( [ ⁄ ] = ˙ )
  ( [ Ÿ ] = ˘ )
  ( [ ‹ ] = y )
  ( [ V ] = v )
  ( [ W ] = w )
  ( [ X ] = x )
  ( [ Y ] = y )
  ( [ › ] = ˝ )
  ( [ Z ] = z )
  ( [ Æ ] = æ )
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
  (ÃI Ï i Ì)
  (SZ s z)
  (Vowel a · e È i Ì o Û u ˙ ˘ y ˝))
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
  ( [ t ] i Ë t Ï = t )
  ( # a n [ t ] i = t )
  ( # a n t [ i ] Vowel = i # )
  ( p r o [ t ] i v n = t~ )
  ( [ t ] i v n = t )
  ( [ d ] ÃI = d~ )
  ( [ t ] ÃI = t~ )
  ( [ n ] ÃI = n~ )
  ( DTN [ Ï ] = e )
  ( BPV [ Ï ] = j e )
  ( m [ Ï ] = n~ e )
  ;; Special combinations (maybe...)
  ( # [ i ] Vowel = j )
  ( [ i ] Vowel = i j )
  ( [ Ì ] Vowel = i: j )
  ( Vowel [ i ] = j )
  ( Vowel [ y ] = j )
  ( Vowel [ Ì ] = j i: )
  ( Vowel [ ˝ ] = j i: )
  ( [ n n ] ÃI = n~ )
  ( [ n n ] = n )

  ;; Endings (maybe...)
  ( # [ b ] = b )
  ( # [ d z ] = dz )
  ( # [ d æ ] = dz~ )
  ( # [ d ] = d )
  ( # [ g ] = g )
  ( # [ h ] = h )
  ( # [ v ] = v )
  ( # [ w ] = v )
  ( # [ z ] = z )
  ( [ b ] # = p )
  ( [ d ] # = t )
  ( [ d z ] # = c )
  ( [ d æ ] # = c~ )
  ( [ g ] # = k )
  ( [ h ] # = ch )
  ( [ v ] # = f )
  ( [ w ] # = f )
  ( [ z ] # = s )
  
  ;; Two-letter phonems
  ( [ d æ ] = dz~ )
  ( [ d z ] = dz )
  ( [ c h ] = ch )

  ;; Special letters
  ( [ Ï ] = j e )
  ;; Simple letters
  ( [ a ] = a )
  ( [ · ] = a: )
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ Ë ] = c~ )
  ( [ d ] = d )
  ( [ Ô ] = d~ )
  ( [ e ] = e )
  ( [ È ] = e: )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ i ] = i )
  ( [ Ì ] = i: )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ Ú ] = n~ )
  ( [ o ] = o )
  ( [ Û ] = o: )
  ( [ p ] = p )
  ( [ q ] = k v )
  ( [ r ] = r )
  ( [ ¯ ] = r~ )
  ( [ s ] = s )
  ( [ π ] = s~ )
  ( [ t ] = t )
  ( [ ª ] = t~ )
  ( [ u ] = u )
  ( [ ˙ ] = u: )
  ( [ ˘ ] = u: )
  ( [ v ] = v )
  ( [ w ] = v )
  ( [ x ] = k s )
  ( [ y ] = i )
  ( [ ˝ ] = i: )
  ( [ z ] = z )
  ( [ æ ] = z~ )
  ))

(defvar czech-unknown-symbol-word "nezn·m˝")

(defvar czech-lts-extra-rules '())

(define (czech-basic-lts word)
  (lts.apply
   (lts.apply
    (if (lts.in.alphabet word 'czech-normalize)
        word
        czech-unknown-symbol-word)
    'czech-normalize)
   'czech))

(define (czech-syllabify phones)
  (if (null? phones)
      ()
      (let ((syl ()))
        (while (and phones (eq? (phone_feature (car phones) 'vc) '-))
          (set! syl (cons (car phones) syl))
          (set! phones (cdr phones)))
        (while (and phones (eq? (phone_feature (car phones) 'vc) '+))
          (set! syl (cons (car phones) syl))
          (set! phones (cdr phones)))
        (cons (reverse syl) (czech-syllabify phones)))))

(define (czech-syllabify-phstress phones)
  (if (null? phones)
      ()
      (let ((syllables (czech-syllabify phones)))
        (cons (list (car syllables) 1)
              (mapcar (lambda (s) (list s 0)) (cdr syllables))))))

(define (czech-lts word features)
  (list word
        nil
        (czech-syllabify-phstress
          (let ((transformed (czech-basic-lts word))
                (rules czech-lts-extra-rules*))
            (while rules
              (set! transformed (lts.apply transformed (car rules)))
              (set! rules (cdr rules)))
            transformed))))

(lex.set.lts.method 'czech-lts)

;;; Tokenization

(defvar czech-token.unknown_word_name "nezn·mÈ")
(defvar czech-token.separator_word_name "oddÏlovaË") ; our own variable
(defvar czech-token.garbage_word_name "smetÌ")       ; our own variable
(defvar czech-token.whitespace " †\t\n\r")
(defvar czech-token.punctuation "\"'`.,:;!?(){}[]<>")
(defvar czech-token.prepunctuation "\"'`({[<")

;;; Token to words processing

(defvar czech-chars "a-zA-Z·‰ËÔÈÏÌÚÛˆ¯πª˙˘¸˝æ¡ƒ»œ…ÃÕ“”÷ÿ©´⁄Ÿ‹›Æ")
(defvar czech-char-regexp (string-append "[" czech-chars "]"))

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
  (if (czech-item.has_feat token 'numprefix)
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
	 ((string-equal d "3") (list "t¯i"))
	 ((string-equal d "4") (list "Ëty¯i"))
	 ((string-equal d "5") (list "pÏt"))
	 ((string-equal d "6") (list "πest"))
	 ((string-equal d "7") (list "sedm"))
	 ((string-equal d "8") (list "osm"))
	 ((string-equal d "9") (list "devÏt")))))
     ((equal? len 2)
      (if (string-equal (car digits) "1")
	  (let ((d (car (cdr digits))))
	    (cond
	     ((string-equal d "0") (list "deset"))
	     ((string-equal d "1") (list "jeden·ct"))
	     ((string-equal d "2") (list "dvan·ct"))
	     ((string-equal d "3") (list "t¯in·ct"))
	     ((string-equal d "4") (list "Ëtrn·ct"))
	     ((string-equal d "5") (list "patn·ct"))
	     ((string-equal d "6") (list "πestn·ct"))
	     ((string-equal d "7") (list "sedmn·ct"))
	     ((string-equal d "8") (list "osmn·ct"))
	     ((string-equal d "9") (list "devaten·ct"))))
	  (append
	   (let ((d (car digits)))
	     (cond
	      ((string-equal d "0") ())
	      ((string-equal d "2") (list "dvacet"))
	      ((string-equal d "3") (list "t¯icet"))
	      ((string-equal d "4") (list "Ëty¯icet"))
	      ((string-equal d "5") (list "pades·t"))
	      ((string-equal d "6") (list "πedes·t"))
	      ((string-equal d "7") (list "sedmdes·t"))
	      ((string-equal d "8") (list "osmdes·t"))
	      ((string-equal d "9") (list "devades·t"))))
	   (czech-number-from-digits (cdr digits)))))
     ((equal? len 3)
      (append
       (let ((d (car digits)))
	 (cond
	  ((string-equal d "0") ())
	  ((string-equal d "1") (list "sto"))
	  ((string-equal d "2") (list "dvÏ" "stÏ"))
	  ((string-equal d "3") (list "t¯i" "sta"))
	  ((string-equal d "4") (list "Ëty¯i" "sta"))
	  ((string-equal d "5") (list "pÏt" "set"))
	  ((string-equal d "6") (list "πest" "set"))
	  ((string-equal d "7") (list "sedm" "set"))
	  ((string-equal d "8") (list "osm" "set"))
	  ((string-equal d "9") (list "devÏt" "set"))))
       (czech-number-from-digits (cdr digits))))
     ((<= len 12)
      (let ((concatenations '((t "tisÌc" "tisÌce" "tisÌc")
			      (t "milion" "miliony" "milion˘")
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
		(if (car words) "dva" "dvÏ"))
	       ((string-equal (car digits) "3") "t¯i")
	       ((string-equal (car digits) "4") "Ëty¯i"))
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

(define (czech-token_to_words token name)
  (cond
   ;; Spaced numbers
   ((and (or (string-matches name "^[-+]?[1-9][0-9]?[0-9]?$")
             (czech-item.has_feat token 'numprefix))
         (not (czech-item.has_feat token 'punc))
         (item.feat token "n.whitespace" " ")
         (string-matches (item.feat token "n.name") "^[0-9][0-9][0-9]$"))
    (item.set_feat (item.next token) 'numprefix
                   (czech-prepend-numprefix token name))
    nil)
   ;; Ordinal numbers
   ((and (string-matches name "^[0-9]+$")
         (string-equal (item.feat token 'punc) ".")
         (item.next token)
         (not (string-matches (item.feat token "n.whitespace") "  +")))
    (if (not (czech-item.has_feat token 'punctype))
        (item.set_feat token 'punctype 'num))
    (append (czech-number* token name)
            (list ".")))
   ;; Numbers beginning with the zero digit
   ((and (string-matches name "^0[0-9]*$")
         (not (czech-item.has_feat token 'numprefix)))
    (apply append (mapcar czech-number (symbolexplode name))))
   ;; Any other numbers
   ((let ((nname (czech-prepend-numprefix token name)))
      (or (string-matches nname "^[-+]?[0-9]+$")
          (string-matches nname "^[-+]?[0-9]+[.,][0-9]+$")
          (string-matches nname "^[-+]?[0-9]+,-$")))
    (if (not (czech-item.has_feat token 'punctype))
        (item.set_feat token 'punctype 'num))
    (let ((nname (czech-prepend-numprefix token name)))
      (if (and (string-equal (item.feat token "n.name") "KË")
               (string-matches nname "^[-+]?[0-9]+,[-0-9]+$"))
          (append
           (czech-number (string-before nname ","))
           (list "korun")
           (let ((hellers (string-after nname ",")))
             (if (not (string-equal hellers "-"))
                 (append
                  (czech-number hellers)
                  (list "halÈ¯˘")))))
          (czech-number nname))))
   ;; Monetary sign
   ((and (string-equal name "KË")
         (string-matches (item.feat token "p.name") "^[-+]?[0-9]+,[-0-9]+$"))
    nil)
   ;; Acronyms
   ((let ((capitals "^[A-Z¡ƒ»œ…ÃÕ“”÷ÿ©´⁄Ÿ‹›Æ]+$"))
      (and (string-matches name capitals)
           (not (lex.lookup_all name))
           (not (string-matches (item.feat token "p.name") capitals))
           (not (string-matches (item.feat token "p.next") capitals))
           (<= (length name) 3) ; longer pronouncable acronyms are not spelled
           ))
    (lts.apply name 'czech-normalize))
   ;; Abbreviations and other unpronouncable words
   ((and (string-matches
          name
          "^[bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQSTVWXZËÔÚ¯πªæ»œ“ÿ©´Æ][bcdfghjkmnpqstvwxzBCDFGHJKMNPQSTVWXZËÔÚ¯πªæ»œ“ÿ©´Æ]+$")
         (not (lex.lookup_all name)))
    (lts.apply name 'czech-normalize))
   ;; Separators
   ((and (string-matches name (string-append "^[^" czech-chars "0-9]+$"))
         (>= (length name) 4)
         (czech-all-same (symbolexplode name)))
    (list czech-token.separator_word_name))
   ;; Dashes
   ((string-matches name "^-+$")
    (if (item.prev token)
        (item.set_feat (item.prev token) 'punc "-"))
    nil)
   ;; Time (just a few of many possible forms)
   ((and (string-matches name "^[0-9]+:[0-9][0-9]$")
         ;; try to identify ratios -- should be better done in POS tagging
         (not (string-matches (item.feat token "p.name")
                              "^[Pp][Oo][Mm][ÃÏ].*"))
         (not (string-matches (item.feat token "p.name")
                              "^[Pp][Rr][Aa][Vv][Dd][ÃÏ][Pp][Oo][Dd][Oo].*"))
         (not (string-matches (item.feat token "p.name")
                              "^[©π][Aa][Nn][Cc].*")))
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
   ;; Lexicon words
   ((lex.lookup_all name)
    (list name))
   ;; Numeric ranges (might be minus as well, but that's rare)
   ((string-matches name "[0-9]+[.,]*[0-9]*-[0-9]+[.,]*[0-9]*$")
    ;; we don't include signs here not to break phone numbers and such a
    ;; written form is incorrect anyway
    (append
     (czech-token_to_words token (string-append
                                  (substring name 0 1)
                                  (string-before (substring name 1 1000) "-")))
     '(((name "-") (pos range)))
     (czech-token_to_words token (string-after (substring name 1 1000) "-"))))
   ;; Homogenous tokens
   ((string-matches name (string-append "^" czech-char-regexp "+$"))
    (list name))
   ((string-matches name (string-append "^[^" czech-chars "0-9]+$"))
    (if (> (length name) 10)
        (list czech-token.garbage_word_name)
        (symbolexplode name)))
   ;; Hyphens
   ((string-matches name
      (string-append "^" czech-char-regexp "+-[-" czech-chars "]+$"))
    (append
     (czech-token_to_words token (string-before name "-"))
     (czech-token_to_words token (string-after name "-"))))
   ;; TODO: roman numerals
   ;; Heterogenous tokens -- mixed alpha, numeric and non-alphanumeric
   ;; characters
   (t
    (if (not (string-matches name (string-append "^[-" czech-chars "]+$")))
        (item.set_feat token 'punctype nil))
    (apply
     append
     (mapcar (lambda (name) (czech-token_to_words token name))
             (czech-tokenize-on-nonalphas name))))))

;;; Lexicon

(lex.add.entry '("nezn·mÈ" nil (((n e) 1) ((z n a:) 0) ((m e:) 0))))

(lex.add.entry '("ch" nil (((ch a:) 1))))
(lex.add.entry '("a"  sym (((a:) 1))))
(lex.add.entry '("a"  nil (((a) 1))))
(lex.add.entry '("·"  nil (((d l o u) 1) ((h e:) 0) ((a:) 1))))
(lex.add.entry '("b"  nil (((b e:) 1))))
(lex.add.entry '("c"  nil (((c e:) 1))))
(lex.add.entry '("Ë"  nil (((c~ e:) 1))))
(lex.add.entry '("d"  nil (((d e:) 1))))
(lex.add.entry '("Ô"  nil (((d~ e:) 1))))
(lex.add.entry '("e"  nil (((e:) 1))))
(lex.add.entry '("È"  nil (((d l o u) 1) ((h e:) 0) ((e:) 1))))
(lex.add.entry '("Ï"  nil (((i) 1) ((j e) 0))))
(lex.add.entry '("f"  nil (((e f) 1))))
(lex.add.entry '("g"  nil (((g e:) 1))))
(lex.add.entry '("h"  nil (((h a:) 1))))
(lex.add.entry '("Ì"  nil (((d l o u) 1) ((h e:) 0) ((i:) 1))))
(lex.add.entry '("j"  nil (((j e:) 1))))
(lex.add.entry '("k"  sym (((k a:) 1))))
(lex.add.entry '("k"  nil (((k) 0))))
(lex.add.entry '("l"  nil (((e l) 1))))
(lex.add.entry '("m"  nil (((e m) 1))))
(lex.add.entry '("n"  nil (((e n) 1))))
(lex.add.entry '("Ú"  nil (((e n~) 1))))
(lex.add.entry '("o"  sym (((o:) 1))))
(lex.add.entry '("o"  nil (((o) 1))))
(lex.add.entry '("Û"  nil (((d l o u) 1) ((h e:) 0) ((o:) 1))))
(lex.add.entry '("p"  nil (((p e:) 1))))
(lex.add.entry '("q"  nil (((k v e:) 1))))
(lex.add.entry '("r"  nil (((e r) 1))))
(lex.add.entry '("¯"  nil (((e r~) 1))))
(lex.add.entry '("s"  sym (((e s) 1))))
(lex.add.entry '("s"  nil (((s) 0))))
(lex.add.entry '("π"  nil (((e s~) 1))))
(lex.add.entry '("t"  nil (((t e:) 1))))
(lex.add.entry '("ª"  nil (((t~ e:) 1))))
(lex.add.entry '("u"  sym (((u:) 1))))
(lex.add.entry '("u"  nil (((u) 1))))
(lex.add.entry '("˙"  nil (((d l o u) 1) ((h e:) 0) ((u:) 1))))
(lex.add.entry '("˘"  nil (((u:) 1) ((s k r o u) 1) ((z~ k e m) 0))))
(lex.add.entry '("v"  sym (((v e:) 1))))
(lex.add.entry '("v"  nil (((v) 0))))
(lex.add.entry '("w"  nil (((d v o) 1) ((j i) 0) ((t e:) 0) ((v e:) 1))))
(lex.add.entry '("x"  nil (((i k s) 1))))
(lex.add.entry '("y"  nil (((i) 1) ((p s i) 0) ((l o n) 0))))
(lex.add.entry '("˝"  nil (((d l o u) 1) ((h e:) 0) ((i) 1) ((p s i) 0) ((l o n) 0))))
(lex.add.entry '("z"  sym (((z e t) 1))))
(lex.add.entry '("z"  nil (((z) 0))))
(lex.add.entry '("æ"  nil (((z~ e t) 1))))

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

(lex.add.entry '("m/s" nil (((m e t) 1) ((r u:) 0) ((z a) 1) ((s e) 0)
                            ((k u n) 0) ((d u) 0))))
(lex.add.entry '("km/h" nil (((k i) 1) ((l o) 0) ((m e t) 0) ((r u:) 0)
                             ((z a) 1) ((h o) 0) ((d~ i) 0) ((n u) 0))))
(lex.add.entry '("cca" nil (((c i r) 1) ((k a) 0))))
(lex.add.entry '("mm"  nil (((m i) 1) ((l i) 0) ((m e) 0) ((t r u:) 0))))
(lex.add.entry '("cm"  nil (((c e n) 1) ((t i) 0) ((m e) 0) ((t r u:) 0))))
(lex.add.entry '("km"  nil (((k i) 1) ((l o) 0) ((m e) 0) ((t r u:) 0))))

(lex.add.entry '("KË"  nil (((k o) 1) ((r u n) 0))))

(lex.add.entry '("festival" nil (((f e s) 1) ((t i) 0) ((v a l) 0))))
(lex.add.entry '("GNU" nil (((g n u:) 1))))
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
(lex.add.entry '("czech" nil (((c~ e k) 1))))
(lex.add.entry '("softwaru" nil (((s o f t) 1) ((v e:) 0) ((r u) 0))))
(lex.add.entry '("dispozici" nil (((d i s) 1) ((p o) 0) ((z i) 0) ((c i) 0))))
(lex.add.entry '("technicky" nil (((t e ch) 1) ((n i c) 0) ((k i) 0))))
(lex.add.entry '("OS/2" nil (((o: #) 1) ((e s) 1) ((d v a) 1))))

;;; Part of Speech

(defvar czech-guess-pos
  '((prep0 "k" "s" "v" "z")
    (prep "bez" "beze" "bÏhem" "do" "ke" "krom" "kromÏ" "mezi" "mimo"
          "mÌsto" "na" "nad" "nade" "o" "od" "ode" "okolo" "po" "pod" "pode"
          "pro" "proti" "p¯ed" "p¯ede" "p¯es" "p¯eze" "p¯i" "se" "skrz"
          "skrze" "u" "ve" "vyjma" "za" "ze" "zpoza")
    (conj "a" "i" "ani" "nebo" "anebo")
    (particle "aª" "kÈæ" "nechª")
    (misc "aby" "abych" "abys" "abychom" "abyste" "ale" "alespoÚ" "aneb" "ani"
          "aniæ" "anæto" "aspoÚ" "avπak" "aË" "aæ" "aËkoli" "aËkoliv" "buÔ"
          "buÔto" "buÔsi" "by" "byª" "byªsi" "co" "coby" "Ëi" "Ëili" "div"
          "dokdy" "dokonce" "dokud" "dotud" "jak" "jakby" "jakkoli" "jakkoliv"
          "jakmile" "jako" "jakoby" "jakoæ" "jakoæto" "jak˝" "jednak" "jednou"
          "jelikoæ" "jen" "jenom" "jenomæe" "jenæe" "jestli" "jestliæe" "jeπtÏ"
          "jeæto" "jinak" "kam" "kde" "kdeæto" "kdo" "kdy" "kdybych" "kdybys"
          "kdyby" "kdybychom" "kdybyste" "kdyæ" "kolik" "kter˝" "kudy" "kv˘li"
          "leda" "ledaæe" "leË" "mezitÌmco" "mimoto" "naËeæ" "neb" "neboli"
          "neboª" "nejen" "nejenæe" "neæ" "neæli" "ne¯kuli" "nicmÈnÏ" "n˝bræ"
          "odkdy" "odkud" "pak" "pakli" "pakliæe" "podle" "podmÌnky" "pokud"
          "ponÏvadæ" "pop¯ÌpadÏ" "potom" "potud" "potÈ" "proËeæ" "proto"
          "protoæe" "pr·vÏ" "p¯ece" "p¯estoæe" "p¯itom" "respektive" "sic"
          "sice" "sotva" "sotvaæe" "tak" "takov˝" "taktak" "takæe" "takÈ"
          "tedy" "ten" "teprve" "to" "toho" "tolik" "tomu" "totiæ" "tu" "tudÌæ"
          "tÌm" "t¯eba" "t¯ebas" "t¯ebasæe" "t¯ebaæe" "vπak" "vædyª" "zatÌmco"
          "zda" "zdali" "zejmÈna" "zrovna" "zvl·πtÏ" "æe")))

(define (czech-pos utt)
  (mapcar
   (lambda (w)
     (let ((name (item.name w))
           (token (item.root (item.relation w 'Token))))
       (cond
        ((czech-item.has_feat w 'pos)
         nil)
        ((and (czech-item.has_feat token 'punctype)
              (string-matches name (string-append "^[^" czech-chars "0-9]+$")))
         (item.set_feat w "pos" (item.feat token 'punctype)))
        ((member (item.name w) '("\"" "'" "`" "-" "." "," ":" ";" "!" "?"
                                 "(" ")"))
         (item.set_feat w "pos" "punc"))
        ((and (eq? (string-length name) 1)
              (or (czech-item.has_feat token 'punc)
                  (not (item.next token))))
         (item.set_feat w "pos" "sym")))))
   (utt.relation.items utt 'Word))
  utt)

;;; Phrase breaks

(defvar czech-phrase-cart-tree
  ;; SB = (very) short break
  '(;; punctuation
    (lisp_token_end_punc in ("." "?" "!" ":" ";" "-"))
    ((BB))
    ((lisp_token_end_punc in ("," "\"" ")"))
     ((B))
     ;; end of utterance
     ((n.name is 0)
      ((BB))
      ;; list of items separated by commas, finished by a conjunction
      ((n.gpos is conj)
       ((R:Token.root.p.punc is ",")
        ((B))
        ;; not a list, but still a conjunction, possibly starting with a vowel
        ((SB)))
       ;; short breaks before words starting with vowels
       ((n.name matches "[aeiou].*")
        ((SB))
        ;; nothing applies -- no break by default
        ((NB))))))))

;;; Pauses

(define (czech-non-pause-words w n)
  (or (<= n 0)
      (let ((next (item.next w)))
        (or (not next)
            (and (not (string-matches (item.feat next "pbreak") "BB?"))
                 (czech-non-pause-words next (- n 1)))))))

(define (czech-pause-method utt)
  (Classic_Pauses utt)
  (let ((words (utt.relation.items utt 'Word)))
    ;; Handle SB -- Classic_Pauses doesn't know about it
    (mapcar
     (lambda (w)
       (if (string-equal (item.feat w "pbreak") "SB")
           (insert_pause utt w)))
     words)
    ;; Insert pauses into long non-breaking sequences
    (let ((counter 0))
      (mapcar
       (lambda (w)
         (if (string-matches (item.feat w "pbreak") "BB?") ; SBs don't apply
             (set! counter 0)
             (begin
               (set! counter (+ counter 1))
               (if (and (>= counter 8)
                        (czech-non-pause-words w 3))
                   (begin
                     (insert_pause utt w)
                     (set! counter 0))))))
       words)))
  utt)

;;; Accents and intonation

(defvar czech-int-simple-params '((f0_mean 100) (f0_std 20)))

(defvar czech-accent-cart-tree
  '((R:SylStructure.parent.gpos is prep0)
    ((NONE))
    ((p.R:SylStructure.parent.gpos is prep)
     ((NONE))
     ((position_type in (initial single))
      ((Accented))
      ((stress is 1)
       ((Accented))
       ((NONE)))))))

(define (czech-intonation-targets utt syl)
  (let ((start (item.feat syl 'syllable_start))
        (end (item.feat syl 'syllable_end))
        (mean (cadr (assoc 'f0_mean int_general_params)))
        (step (/ (cadr (assoc 'f0_std int_general_params)) 5)))
    (let ((s_int mean)
          (m_int mean)
          (e_int mean))
      ;; Accented syllables
      (if (equal? (item.feat syl "R:Intonation.daughter1.name") "Accented")
          (begin
            (set! s_int (+ s_int step))
            (set! m_int (+ m_int (* 2 step)))))
      ;; First sylable of an utterance or sentence
      (if (or (not (item.prev syl))
              (and (equal? (item.feat syl "syl_in") 0)
                   (equal? (item.feat syl
                                      "p.R:SylStructure.parent.R:Token.n.name")
                           ".")))
          (begin
            (set! s_int (+ s_int (* 2 step)))
            (set! m_int (+ m_int (* 2 step)))))
      ;; End of phrase
      (if (equal? (item.feat syl "syl_out") 0)
          (let ((pun (item.feat syl "R:SylStructure.parent.R:Token.n.name")))
            (cond
             ((equal? pun ".")
              (set! s_int (- s_int (* 1 step)))
              (set! m_int (- m_int (* 4 step)))
              (set! e_int (- e_int (* 6 step))))
             ((equal? pun "!")
              (set! m_int (+ m_int (* 1 step)))
              (set! e_int (+ e_int (* 2 step))))
             ((equal? pun "?")
              (set! s_int (+ s_int (* 1 step)))
              (set! m_int (+ m_int (* 10 step)))
              (set! e_int (+ e_int (* 8 step))))
             (t
              (set! m_int (- m_int (* 1 step)))
              (set! e_int (- e_int (* 2 step)))))))
      ;; Resulting values
      (list
       (list start s_int)
       (list (/ (+ start end) 2.0) m_int)
       (list end e_int)))))

;;; Duration

(defvar czech-phoneme-durations
  '(
    (#   0.15)
    (a   0.09)
    (a:  0.12)
    (b   0.07)
    (c   0.07)
    (c~  0.07)
    (ch  0.07)
    (d   0.07)
    (d~  0.07)
    (e   0.08)
    (e:  0.11)
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

(defvar czech-duration-cart-tree
  '(;; pauses
    (name is "#")
    ((p.R:SylStructure.parent.parent.R:Word.pbreak is "SB")
     ((0.1))
     ((1.0)))
    ;; clause initial
    ((R:SylStructure.parent.R:Syllable.syl_in is 0)
     ((R:SylStructure.parent.stress is 1)
      ((1.3))
      ((1.2)))
     ;; clause final
     ((n.R:SylStructure.parent.R:Syllable.syl_in is 0)
      ((R:SylStructure.parent.stress is 1)
       ((1.3))
       ((1.2)))
      ;; stressed
      ((R:SylStructure.parent.stress is 1)
       ((ph_vc is +)
        ((1.2))
        ((1.0)))
       ;; default
       ((1.0)))))))

;;; Volume

(defvar czech-volume-scale 1.8)
(defvar czech-volume-scale* nil)
  
(define (czech-adjust-volume utt)
  (utt.wave.rescale utt czech-volume-scale*))

;;; Final phoneme translation

(define (czech-phone-adjustment utt)
  (if (and (eq? (Parameter.get 'Language) 'czech)
           czech-phoneset-translation*)
      (mapcar
       (lambda (item)
         (let ((tr (assoc (item.name item) czech-phoneset-translation*)))
           (if tr (item.set_name item (cadr tr)))))
       (utt.relation.items utt 'Segment))))

;;; Finally, the language definition itself

(define (czech-reset-parameters)
  (set! czech-lts-extra-rules* czech-lts-extra-rules)
  (set! czech-int-simple-params* czech-int-simple-params)
  (set! czech-phoneme-durations* czech-phoneme-durations)
  (set! czech-volume-scale* czech-volume-scale)
  (set! czech-phoneset-translation* nil)
  (Parameter.set 'Synth_Method 'UniSyn))

(define (voice-czech-common)
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
  (set! guess_pos czech-guess-pos)
  (Parameter.set 'POS_Method czech-pos)
  ;; Simple phrase break prediction by punctuation
  (set! pos_supported nil)
  (set! phrase_cart_tree czech-phrase-cart-tree)
  (Parameter.set 'Phrase_Method 'cart_tree)
  ;; Pauses
  (Parameter.set 'Pause_Method czech-pause-method)
  ;; Accent prediction and intonation
  (set! int_accent_cart_tree czech-accent-cart-tree)
  (Parameter.set 'Int_Method 'General)
  (set! int_general_params (cons (list 'targ_func czech-intonation-targets)
                                 czech-int-simple-params*))
  (Parameter.set 'Int_Target_Method Int_Targets_General)
  ;; Duration prediction
  (set! duration_cart_tree czech-duration-cart-tree)
  (set! duration_ph_info (mapcar
                          (lambda (spec) (list (car spec) 0.0 (cadr spec)))
                          czech-phoneme-durations*))
  (Parameter.set 'Duration_Method 'Tree_ZScores)
  ;; Postlex rules
  (set! postlex_rules_hooks (list))
  (set! after_analysis_hooks (list czech-phone-adjustment))
  ;; Final voice adjustment
  (set! after_synth_hooks czech-adjust-volume)
  ;; Set current voice
  (set! current-voice 'czech))

(defmac (czech-proclaim-voice form)
  (let ((name (intern (string-append 'czech_ (cadr form))))
        (description (caddr form))
        (body (cdddr form)))
    `(begin
       (define (,(intern (string-append 'voice_ name)))
         (czech-reset-parameters)
         ,@body
         (voice-czech-common)
         (set! current-voice (quote ,name)))
       (proclaim_voice
        (quote ,name)
        (quote ((language czech)
                (dialect nil)
                (gender nil)
                (coding ISO-8859-2)
                (description ,description)))))))

(provide 'czech)
