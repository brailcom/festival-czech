Jak se bal�k pou��v�:

- V�adres��i `festival-czech' prove�te p��kaz

    make

  pro vygenerov�n� lexikonu.

- Po startu Festivalu je nutno nat�hnout podporu �e�tiny, nap��klad:

    (set! load-path (cons ".../festival-czech" load-path))
    (require 'czech)

- Je t�eba nastavit cestu k�lexikonu:

    (set! czech-lexicon-file ".../festival-czech/czech-lexicon.out")
    
- Bal�k je moment�ln� pou�iteln� pouze se syntetiz�rem Mbrola.  Nen� to stav,
  se kter�m bychom byli spokojeni, a�proto pracujeme na jeho n�prav�.
  S�ohledem na na�e omezen� zdroje v�ak zat�m nen� datab�ze difon� pro
  festivalov� syntetiz�r k�dispozici.

  Chcete-li �e�tinu pou��vat se synt�zou syst�mu Mbrola, mus�te:

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
