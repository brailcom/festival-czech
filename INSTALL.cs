Jak se bal�k pou��v�:

- Po startu Festivalu je nutno nat�hnout podporu �e�tiny, nap��klad:

    (load ".../czech.scm")

- Chcete-li �e�tinu pou��vat se standardn� festivalovou synt�zou (UniSyn),
  pou�ijte jako vzor pro definici hlasu soubor czech-unisyn.scm.

    (set! czech-index_file ".../festival/voice/czech.index")

  Bohu�el toho �asu neexistuj� ��dn� svobodn� vzorky pro Festival, mus�te m�t
  n�jak� z�vlastn�ch zdroj�.  M�te-li z�jem o�spolupr�ci na vytvo�en�
  svobodn�ch �esk�ch vzork� pro Festival, pros�me, kontaktujte n�s!

- Chcete-li �e�tinu pou��vat se synt�zou syst�mu Mbrola, mus�te nav�c:

  * Nastavit prom�nnou czech-mbrola_database:

      (set! czech-mbrola_database ".../cz2")

  * Nastavit jm�no bin�rky mbrola:

      (set! mbrola_progname ".../mbrola")

  * Nat�hnout soubor czech-mbrola.scm:

      (load ".../czech-mbrola.scm")

  * Nastavit �esk� hlas z�Mbrola:

      (voice_czech_mbrola_cz2)

  Pros�m uv�domte si, �e Mbrola nen� svobodn� software a m��ete jej pou��vat
  jen do m�ry svolen� a podpory jeho v�robce.
