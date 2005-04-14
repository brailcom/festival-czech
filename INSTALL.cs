Jak se balík pou¾ívá:

- V adresáøi `festival-czech' proveïte pøíkaz

    make

  pro vygenerování lexikonu.

- Je tøeba nastavit cestu k lexikonu:

    (set! czech-lexicon-file ".../festival-czech/czech-lexicon.out")

  Toto nastavení musí být provedeno je¹tì pøed nata¾ením èe¹tiny popsaném
  v následujícím kroku.

- Po startu Festivalu je nutno natáhnout podporu èe¹tiny, napøíklad:

    (set! load-path (cons ".../festival-czech" load-path))
    (require 'czech)

- Chcete-li podpoøit vznik kvalitního svobodného èeského hlasu, vyzkou¹ejte
  hlas voice-czech-ph dostupný
  z http://www.freebsoft.org/festival-czech-diphone-database.
    
- Proto¾e hlas voice-czech-ph je¹tì není hotový, je podporována i syntéza
  prostøednictvím proprietárního syntetizéru Mbrola.  Chcete-li jej pou¾ívat,
  musíte:

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
