Popis postupù èeské syntézy pou¾itých ve festival-czech
=======================================================

* Základní jednotky øeèi

Základní jednotky øeèi byly definovány dle difonové sady vypracované
Mgr. Pavlem Machaèem.  Tento materiál je dostupný v balíku voice-czech-ph.

** Fonémy

Fonémová sada je definovaná standardním zpùsobem v czech.scm.  Vlastnosti
fonémù definované v czech.scm byly definovány dle potøeb èeské syntézy, bez
pøímé návaznosti na konkrétní jazykové nebo fonetické poznatky.  Rùzné fonémy
pøitom mohou mít zcela shodné vlastnosti, nebylo snahou uèinit fonémy svými
vlastnostmi unikátní.

** Difony

Pou¾íváme difonovou sadu z vý¹e zmínìného materiálu Mgr. Pavla Machaèe.  Ta
v rozumné míøe aproximuje mno¾inu dvojic hlásek vyskytujících se v èe¹tinì.

** Pøidávání segmentù

Èást procesu stanovení výsledné sekvence difonù se provádí a¾ pøed samotnou
syntézou, v czech-after-analysis-hooks.  Do sekvence segmentù se zde pøidávají
umìlé segmenty.  Cílem tìchto akcí je dosa¾ení lep¹ího výsledného zvuku a
pøípadnì té¾ konverze fonémù pro syntetizéry pou¾ívající jinou fonémovou sadu.
Proto¾e se pøidávání segmentù provádí a¾ tìsnì pøed syntézou, není nutno je
nijak zohledòovat v urèování prozodie.  Propagace prozodických informací do
pøidaných segmentù je zaji¹tìna automaticky.

** Slabiky

Ve festival-czech je za slabiku pova¾ováno celé mluvené slovo.  Prvním dùvodem
je, ¾e je to výhodné pøi urèování prozodie.  Druhým dùvodem je, ¾e rozklad slov
na slabiky je v èe¹tinì obtí¾nì urèitelný.  Pøi zkoumání slabik je relevantní
pouze jejich poèet, který je dán výskytem samohlásek a slabikotvorných
souhlásek.  Na to není rozklad na slabiky zapotøebí.


* Výslovnost jednotlivých slov

Urèování výslovnosti jednotlivých slov je øe¹eno primárnì LTS pravidly `czech'.
Korekce fonetické formy, ji¾ nezávislé na pravopisných pravidlech, se provádí
ve funkci czech-adjust-segments.  Tato funkce se volá a¾ po zpracování pauz,
proto¾e pauzy jsou pro urèení výsledné fonetické podoby podstatný.

Proto¾e souèasný èeský pravopis je pøíli¹ nepravidelný, je nutno si pøi pøevodu
psané formy na fonetickou vypomáhat lexikonem.  Lexikon je pojatý jako seznam
pøesnì tìch slov, která nejsou a ani rozumnì být nemohou pokryta LTS pravidly
nebo pro která by nesprávnì probìhl pøevod z tokenu na slovo (napøíklad
v pøípadì slova _pst_, které by bylo hláskováno).

** Jak postupovat pøi nalezení nesprávné výslovnosti slova

Pokud je slovo obsa¾eno v lexikonu, pøíslu¹ný záznam se jednodu¹e opraví.
Pokud slovo není obsa¾eno v lexikonu, mìlo by být v první øadì posouzeno, zda
pøíslu¹ný problém není ¹ir¹ího rázu a nestálo by za to jej o¹etøit pøidáním LTS
pravidla.

Jedná-li se spí¹e o výjimku, kterou nemá smysl zohledòovat v LTS pravidlech,
pøidá se do lexikonu.  Jde-li o slovo ohebné, mìly by být pøidány v¹echny jeho
tvary, jsou-li vyslovovány chybnì té¾.  Je dobré zamyslet se i nad formami
vytvoøenými pøidáním nebo odebráním pøedpon.

Pokud dojde na pøidání nového LTS pravidla, je zapotøebí provìøit, jaké zmìny
zpùsobí.  Optimální by bylo nechat vygenerovat výslovnost v¹ech èeských slov
pøed zmìnou LTS pravidel a po ní a následnì srovnat jejich diff výstupy.  To je
v¹ak na bì¾ných strojích pomìrnì zdlouhavá zále¾itost a je tedy praktiètìj¹í ji
aplikovat pouze na mno¾inu slov, které mohou být pøíslu¹ným pravidlem dotèeny.
Je-li nové LTS pravidlo øádnì ovìøeno, mù¾e být pøidáno.  Volitelnì lze
provìøit slova v lexikonu, zda se výslovnost nìkterých z nich neshoduje
s výsledkem po aplikaci nových LTS pravidel, a taková slova z lexikonu vyøadit.

** Tokeny, které se expandují na více slov

Je-li zápis urèitého slova expandován na vícero slov, je nutno jeho expanzi
definovat v promìnné czech-multiword-abbrevs.  Taková slova nepatøí do lexikonu
-- lexikon definuje jen výslovnost jednotlivých slov.

** Pravidla pro pøepis z textu do fonetické podoby

Pøi pøepisu textu do fonetické podoby je v lexikonu dovoleno pou¾ívat ve¹keré
fonémy definované v èeské fonémové sadì.  Pøitom musí být dodr¾ena následující
pravidla.

*** Konverze znìlých souhlásek na neznìlé a naopak

Neprovádí se konverze znìlých souhlásek na neznìlé a naopak, kde to není
vylo¾enì nutné.  Napøíklad správný (z hlediska festival-czech) fonetický pøepis
slova _ovce_ je (o v c e), nikoliv (o f c e).  Výslovnost difonu v-c se toti¾
jednak v tomto slovì podstatnì neli¹í od výslovnosti tohoto difonu v jiných
slovech a jednak se s opravdovou znìlou výslovností tohoto difonu v èe¹tinì
nesetkáme (ani na hranicích slov a slovech pøejatých).  Jeho správná výslovnost
tedy bude zaji¹tìna v difonové databázi nebo jejích konverzních pravidlech.

Naproti správný pøepis slova _magnetismus_ je (m a g n e t i z m u s), proto¾e
v nìm se skuteènì jedná o pozmìnìnou výslovnost slova pøejatého a v koncovce je
èeský difon z-m, nikoliv èeský difon s-m.

Toto pravidlo nedává z fonetického hlediska smysl.  Jeho cílem je v¹ak
zjednodu¹ení tvorby polo¾ek lexikonu (pøispìvatel se mù¾e více dr¾et psané
formy a nemusí se zabývat speciálními pøípady) a zamezení lidové tvoøivosti
v nejednoznaèných pøípadech.

*** Vkládání rázù

Je nutno nezapomínat na vlo¾ení rázù mezi samohláskami.  Správný pøepis slova
_neefektivní_ je (n e _ e f e k t i v n~ i:).  Naopak správný pøepis slova
_poet_ je (p o e t).

*** Dvojhlásky

Dvojhlásky se reprezentují odpovídající dvojicí hlásek.  Speciálním pøípadem
jsou dvojhlásky obsahující `i' nebo `i:'.  Ve skuteènosti se nejedná
o dvojhlásky, vyslovuje se toti¾ mezi nimi hláska `j'.  Správný pøepis slova
_poezie_ tedy je (p o e z i j e).


* Prozodie

Prozodie ve festival-czech zahrnuje intonaci, délku, pauzy a pøízvuk.  Tyto
parametry jsou konstruovány pøevá¾nì na základì pravidel publikovaných
v [palková:04].

Vìtné úseky a pøízvukové takty jsou konstruovány ve fázi Word, jako zvlá¹tní
relace IntUnit a StressUnit.

** Intonace

Pravidla pro intonaci byla pøevzata z [palková:04].  Konkrétní intonaèní
tabulka kadencí byla pøevzata z [palková-ptáèek:97].

V publikovaných prozodických pravidlech jsou urèité nejasnosti, které byly
rozøe¹eny následujícím zpùsobem:

- Pro ¹esti a víceslabièné pøízvukové takty se v pozici F pou¾ívá intonaèní
  køivka ze skupiny A (v [palková-ptáèek:97] pøíslu¹né pravidlo pro pozici F
  chybí).

- Pro pozici F se volí intonaèní køivka ze skupiny A (v [palková:04] není
  specifikováno).

- Pro pozici F-1 se vybírá pouze z intonaèních køivek povolených pro skupinu F
  (pro jistotu -- v [palková:04] není specifikováno, k èemu by s ohledem na
  pøedchozí úpravu mìly být F-køivky skupiny B).

Nedoøe¹ena je detekce doplòkových tázacích vìt ve funkci czech-yes-no-question,
kde chybí dostateènì obsáhlý seznam tázacích zájmen, èíslovek a pøíslovcí.
Mìlo by být mo¾né jej získat s pomocí èeského ispellu.
  
** Délka

Délka se stanovuje na základì poètu slabik v pøízvukovém taktu dle tabulky
z [palková:04].

** Pauzy

Pou¾íváme pauzy tøí délek: nejdel¹í (BB), støední (B) a krátká (SB).
Umis»ování pauz se definuje v promìnné czech-phrase-cart-tree a funkci
czech-adjust-phrase-breaks.  Pou¾itá metoda urèování pauz vychází
z [palková-ptáèek:66], není v¹ak pøesnou implementací tìchto pravidel.

** Pøízvuk

Dle [palková:04], sekce 1.2db), není v syntéze èe¹tiny ¾ádoucí pøízvuk
explicitnì generovat zmìnou dynamiky.  Pøízvuk je modelován intonaèní køivkou.


* Odkazy

[palková:04]
  Zdena Palková: Soubor fonetických pravidel jako podklad pro prozodický
  komponent automatické syntézy TTS v èe¹tinì

[palková-ptáèek:97]
  Zdena Palková, Miroslav Ptáèek: Modelling Prosody in TTS Diphone Synthesis in
  Czech; Forum Phoneticum 63, Frankfurt am Main 1997

[palková-ptáèek:66]
  Zdena Palková, Miroslav Ptáèek: TTS Issues: Prosody modifications in Text;
  in Speech processing, 6th Czech-German Workshop, Prag 1966, R. Vích
  (ed.) pp.32-34
  + interní materiál Z. Palková, Závìreèná zpráva grantu GAÈR 405/96/0301


-- Milan Zamazal

Local variables:
mode: outline
end:
