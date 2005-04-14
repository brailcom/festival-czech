Jak se bal�k pou��v�:

- V�adres��i `festival-czech' prove�te p��kaz

    make

  pro vygenerov�n� lexikonu.

- Je t�eba nastavit cestu k�lexikonu:

    (set! czech-lexicon-file ".../festival-czech/czech-lexicon.out")

  Toto nastaven� mus� b�t provedeno je�t� p�ed nata�en�m �e�tiny popsan�m
  v�n�sleduj�c�m kroku.

- Po startu Festivalu je nutno nat�hnout podporu �e�tiny, nap��klad:

    (set! load-path (cons ".../festival-czech" load-path))
    (require 'czech)

- Chcete-li podpo�it vznik kvalitn�ho svobodn�ho �esk�ho hlasu, vyzkou�ejte
  hlas voice-czech-ph dostupn�
  z�http://www.freebsoft.org/festival-czech-diphone-database.
    
- Proto�e hlas voice-czech-ph je�t� nen� hotov�, je podporov�na i�synt�za
  prost�ednictv�m propriet�rn�ho syntetiz�ru Mbrola.  Chcete-li jej pou��vat,
  mus�te:

  * Nastavit prom�nnou czech-mbrola_database:

      (set! czech-mbrola_database ".../cz2")

  * Nastavit jm�no bin�rky mbrola:

      (set! mbrola_progname ".../mbrola")

  * Nat�hnout soubor czech-mbrola.scm:

      (require 'czech-mbrola)

  * Nastavit �esk� hlas z�Mbrola:

      (voice_czech_mbrola_cz2)

  Pros�m uv�domte si, �e Mbrola nen� svobodn� software a m��ete jej pou��vat
  jen do m�ry svolen� a podpory jeho v�robce.
