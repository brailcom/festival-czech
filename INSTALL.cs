Jak se balík pou¾ívá:

- Po startu Festivalu je nutno natáhnout podporu èe¹tiny, napøíklad:

    (set! load-path (cons ".../festival-czech" load-path))
    (require 'czech)

- Balík je momentálnì pou¾itelný pouze se syntetizérem Mbrola.  Není to stav,
  se kterým bychom byli spokojeni, a proto pracujeme na jeho nápravì.
  S ohledem na na¹e omezené zdroje v¹ak zatím není databáze difonù pro
  festivalový syntetizér k dispozici.

  Chcete-li èe¹tinu pou¾ívat se syntézou systému Mbrola, musíte:

  * Nastavit promìnnou czech-mbrola_database:

      (set! czech-mbrola_database ".../cz2")

  * Nastavit jméno binárky mbrola:

      (set! mbrola_progname ".../mbrola")

  * Natáhnout soubor czech-mbrola.scm:

      (require 'czech-mbrola)

  * Nastavit èeský hlas z Mbrola:

      (voice_czech_mbrola_cz2)

  Prosím uvìdomte si, ¾e Mbrola není svobodný software a mù¾ete jej pou¾ívat
  jen do míry svolení a podpory jeho výrobce.
