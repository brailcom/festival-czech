Jak se balík pou¾ívá:

- Po startu Festivalu je nutno natáhnout podporu èe¹tiny, napøíklad:

    (load ".../czech.scm")

- Chcete-li èe¹tinu pou¾ívat se standardní festivalovou syntézou (UniSyn),
  nastavte je¹tì pøed nata¾ením souboru czech.scm promìnnou czech-index_file na
  cestu k pou¾itým vzorkùm:

    (set! czech-index_file ".../festival/voice/czech.index")

  Bohu¾el toho èasu neexistují ¾ádné svobodné vzorky pro Festival, musíte mít
  nìjaké z vlastních zdrojù.  Máte-li zájem o spolupráci na vytvoøení
  svobodných èeských vzorkù pro Festival, prosíme, kontaktujte nás!

- Chcete-li èe¹tinu pou¾ívat se syntézou systému Mbrola, musíte navíc:

  * Nastavit promìnnou czech-mbrola_database:

      (set! czech-mbrola_database ".../cz2")

  * Nastavit jméno binárky mbrola:

      (set! mbrola_progname ".../mbrola")

  * Natáhnout soubor czech-mbrola.scm:

      (load ".../czech-mbrola.scm")

  * Nastavit èeskou syntézu na Mbrola:

      (set! czech-description czech-mbrola-description)

  Prosím uvìdomte si, ¾e Mbrola není svobodný software a mù¾ete jej pou¾ívat
  jen do míry svolení a podpory jeho výrobce.

- Nyní je ji¾ mo¾no zapnout èe¹tinu:

    (voice_czech)
