Standardn� instalace:

- V�adres��i `festival-czech' prove�te p��kaz

    make

  pro vygenerov�n� lexikonu.

- Prove�te p��kaz

    make install

  Pokud m�te festivalov� *.scm soubory nainstalov�ny jinde ne�
  v�/usr/share/festival, uve�te spr�vn� adres�� jako prom�nnou `festival_path'
  p��kazu `make', nap��klad:

    make install festival_path=/usr/local/festival

- Odinstalaci lze prov�st p��kazem `make uninstall', op�t s�p��padn�m uveden�m
  festival_path.

Pokud nechcete, nemus�te festival-czech instalovat do Festivalu, ale m��ete ho
pou��vat nap��klad p��mo z�instala�n�ho adres��e.  V�takov�m p��pad� je pot�eba
m�sto `make install' ud�lat n�sleduj�c� kroky:

- P�idat adres�� do prom�nn� `load-path' Festivalu, nap��klad:

    (set! load-path (cons ".../festival-czech" load-path))

- Nastavit cestu k�lexikonu:

    (set! czech-lexicon-file ".../festival-czech/czech-lexicon.out")

  Toto nastaven� mus� b�t provedeno je�t� p�ed nata�en�m �e�tiny popsan�m
  v�n�sleduj�c�m kroku.

- Po startu Festivalu nat�hnout podporu �e�tiny, nap��klad:

    (require 'czech)

festival-czech samotn� implementuje pouze pravidla �esk� synt�zy.  Pokud chcete
prov�d�t synt�zu samotnou, pot�ebujete k�n� i��esk� hlas.  Existuje svobodn�
�esk� hlas pro Festival voice-czech-ph dostupn�
z�http://www.freebsoft.org/festival-czech-diphone-database.

Krom� toho je podporov�na i�synt�za prost�ednictv�m propriet�rn�ho syntetiz�ru
Mbrola.  Chcete-li jej pou��vat, mus�te ud�lat n�sleduj�c�:

- Nastavit prom�nnou czech-mbrola_database:

    (set! czech-mbrola_database ".../cz2")

- Nastavit jm�no bin�rky mbrola:

    (set! mbrola_progname ".../mbrola")

- Nat�hnout soubor czech-mbrola.scm:

    (require 'czech-mbrola)

- Nastavit �esk� hlas z�Mbrola:

    (voice_czech_mbrola_cz2)

Pros�m uv�domte si, �e Mbrola nen� svobodn� software a m��ete jej pou��vat jen
do m�ry svolen� a podpory jeho v�robce.
