Standardní instalace:

- V adresáøi `festival-czech' proveïte pøíkaz

    make

  pro vygenerování lexikonu.

- Proveïte pøíkaz

    make install

  Pokud máte festivalové *.scm soubory nainstalovány jinde ne¾
  v /usr/share/festival, uveïte správný adresáø jako promìnnou `festival_path'
  pøíkazu `make', napøíklad:

    make install festival_path=/usr/local/festival

- Odinstalaci lze provést pøíkazem `make uninstall', opìt s pøípadným uvedením
  festival_path.

Pokud nechcete, nemusíte festival-czech instalovat do Festivalu, ale mù¾ete ho
pou¾ívat napøíklad pøímo z instalaèního adresáøe.  V takovém pøípadì je potøeba
místo `make install' udìlat následující kroky:

- Pøidat adresáø do promìnné `load-path' Festivalu, napøíklad:

    (set! load-path (cons ".../festival-czech" load-path))

- Nastavit cestu k lexikonu:

    (set! czech-lexicon-file ".../festival-czech/czech-lexicon.out")

  Toto nastavení musí být provedeno je¹tì pøed nata¾ením èe¹tiny popsaném
  v následujícím kroku.

- Po startu Festivalu natáhnout podporu èe¹tiny, napøíklad:

    (require 'czech)

festival-czech samotné implementuje pouze pravidla èeské syntézy.  Pokud chcete
provádìt syntézu samotnou, potøebujete k ní i èeský hlas.  Existuje svobodný
èeský hlas pro Festival voice-czech-ph dostupný
z http://www.freebsoft.org/festival-czech-diphone-database.

Kromì toho je podporována i syntéza prostøednictvím proprietárního syntetizéru
Mbrola.  Chcete-li jej pou¾ívat, musíte udìlat následující:

- Nastavit promìnnou czech-mbrola_database:

    (set! czech-mbrola_database ".../cz2")

- Nastavit jméno binárky mbrola:

    (set! mbrola_progname ".../mbrola")

- Natáhnout soubor czech-mbrola.scm:

    (require 'czech-mbrola)

- Nastavit èeský hlas z Mbrola:

    (voice_czech_mbrola_cz2)

Prosím uvìdomte si, ¾e Mbrola není svobodný software a mù¾ete jej pou¾ívat jen
do míry svolení a podpory jeho výrobce.
