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

(lts.ruleset
 czech-normalize
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
  (�I � i �)
  (SZ s z)
  (Vowel a � e � i � o � u � � y �))
 (
  ;; Special combinations
  ( [ d ] i SZ m u = d )
  ( [ n ] i SZ m u = n )
  ( [ t ] i SZ m u = t )
  ( [ d ] i s t = d )
  ( [ n ] i s t = n )
  ( [ t ] i s t = t )
  ( [ t ] i c k = t )
  ( [ t ] i � t � = t )
  ( # a n [ t ] i = t )
  ( # a n t [ i ] Vowel = i )
  ( [ t ] i v n = t )
  ( [ d ] �I = d~ )
  ( [ t ] �I = t~ )
  ( [ n ] �I = n~ )
  ( DTN [ � ] = e )
  ( BPV [ � ] = j e )
  ( m [ � ] = n~ e )
  ;; Special combinations (maybe...)
  ( # [ i ] Vowel = j )
  ( [ i ] Vowel = i j )
  ( [ � ] Vowel = i: j )
  ( Vowel [ i ] = j )
  ( Vowel [ y ] = j )
  ( Vowel [ � ] = j i: )
  ( Vowel [ � ] = j i: )
  ( [ n n ] �I = n~ )
  ( [ n n ] = n )

  ;; Endings (maybe...)
  ( # [ b ] = b )
  ( # [ d z ] = dz )
  ( # [ d � ] = dz~ )
  ( # [ d ] = d )
  ( # [ g ] = g )
  ( # [ h ] = h )
  ( # [ v ] = v )
  ( # [ w ] = v )
  ( # [ z ] = z )
  ( [ b ] # = p )
  ( [ d ] # = t )
  ( [ d z ] # = c )
  ( [ d � ] # = c~ )
  ( [ g ] # = k )
  ( [ h ] # = ch )
  ( [ v ] # = f )
  ( [ w ] # = f )
  ( [ z ] # = s )
  
  ;; Two-letter phonems
  ( [ d � ] = dz~ )
  ( [ d z ] = dz )
  ( [ c h ] = ch )

  ;; Special letters
  ( [ � ] = j e )
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

(defvar czech-unknown-symbol-word "nezn�m�")

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

;;; Tokenization

(defvar czech-token.unknown_word_name "nezn�m�")
(defvar czech-token.separator_word_name "odd�lova�") ; our own variable
(defvar czech-token.garbage_word_name "smet�")       ; our own variable
(defvar czech-token.whitespace " �\t\n\r")
(defvar czech-token.punctuation "\"'`.,:;!?(){}[]<>")
(defvar czech-token.prepunctuation "\"'`({[<")

;;; Token to words processing

(defvar czech-chars "a-zA-Z����������������������������ة����ݮ")
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
      (if (and (string-equal (item.feat token "n.name") "K�")
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
           ))
    (lts.apply name 'czech-normalize))
   ;; Abbreviations and other unpronouncable words
   ((and (string-matches
          name
          "^[bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQSTVWXZ����������ة��][bcdfghjkmnpqstvwxzBCDFGHJKMNPQSTVWXZ����������ة��]+$")
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

(require 'czech-lexicon)
(lex.select "czech")
(lex.set.lts.method 'czech-lts)
(lex.add.entry '("nezn�m�" nil (((n e) 1) ((z n a:) 0) ((m e:) 0))))

;;; Part of Speech

(defvar czech-guess-pos
  '((prep0 "k" "s" "v" "z")
    (prep "bez" "beze" "b�hem" "do" "ke" "krom" "krom�" "mezi" "mimo"
          "m�sto" "na" "nad" "nade" "o" "od" "ode" "okolo" "po" "pod" "pode"
          "pro" "proti" "p�ed" "p�ede" "p�es" "p�eze" "p�i" "se" "skrz"
          "skrze" "u" "ve" "vyjma" "za" "ze" "zpoza")
    (conj "a" "i" "ani" "nebo" "anebo")
    (particle "a�" "k�" "nech�")
    (misc "aby" "abych" "abys" "abychom" "abyste" "ale" "alespo�" "aneb" "ani"
          "ani�" "an�to" "aspo�" "av�ak" "a�" "a�" "a�koli" "a�koliv" "bu�"
          "bu�to" "bu�si" "by" "by�" "by�si" "co" "coby" "�i" "�ili" "div"
          "dokdy" "dokonce" "dokud" "dotud" "jak" "jakby" "jakkoli" "jakkoliv"
          "jakmile" "jako" "jakoby" "jako�" "jako�to" "jak�" "jednak" "jednou"
          "jeliko�" "jen" "jenom" "jenom�e" "jen�e" "jestli" "jestli�e" "je�t�"
          "je�to" "jinak" "kam" "kde" "kde�to" "kdo" "kdy" "kdybych" "kdybys"
          "kdyby" "kdybychom" "kdybyste" "kdy�" "kolik" "kter�" "kudy" "kv�li"
          "leda" "leda�e" "le�" "mezit�mco" "mimoto" "na�e�" "neb" "neboli"
          "nebo�" "nejen" "nejen�e" "ne�" "ne�li" "ne�kuli" "nicm�n�" "n�br�"
          "odkdy" "odkud" "pak" "pakli" "pakli�e" "podle" "podm�nky" "pokud"
          "pon�vad�" "pop��pad�" "potom" "potud" "pot�" "pro�e�" "proto"
          "proto�e" "pr�v�" "p�ece" "p�esto�e" "p�itom" "respektive" "sic"
          "sice" "sotva" "sotva�e" "tak" "takov�" "taktak" "tak�e" "tak�"
          "tedy" "ten" "teprve" "to" "toho" "tolik" "tomu" "toti�" "tu" "tud�"
          "t�m" "t�eba" "t�ebas" "t�ebas�e" "t�eba�e" "v�ak" "v�dy�" "zat�mco"
          "zda" "zdali" "zejm�na" "zrovna" "zvl�t�" "�e")))

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
