Popis postupù èeské syntézy pou¾itých ve festival-czech
=======================================================

* Základní jednotky øeèi

Základní jednotky øeèi byly definovány dle [palková-ptáèek:94], sekce 2.

©ir¹í seznam jednotek øeèi pro èe¹tinu lze nalézt v [horák:02].

** Fonémy

K fonémùm z [palková-ptáèek:94] byly do èeské fonémové sady navíc pøidány
fonémy odpovídající dlouhým samohláskám á, é, ó, ú.  Dále byl je¹tì pøidán
foném pauzy a foném pro nárazový pøechod mezi samohláskami (napøíklad ve slovì
"neefektivní" mezi dvìma `e').

Vlastnosti fonémù ve fonémové sadì byly definovány dle potøeb èeské syntézy,
bez pøímé návaznosti na konkrétní jazykové nebo fonetické poznatky.  Rùzné
fonémy pøitom mohou mít zcela shodné vlastnosti, nebylo snahou uèinit fonémy
svými vlastnostmi unikátní.

** Difony

UniSyn syntéza Festivalu pracuje nejlépe s difony, jiné pøístupy by byly
komplikované.  Proto je ¾ádoucí, aby relace segmentù byla sestavena ji¾
s ohledem na budoucí páry fonémù odpovídající stanovené mno¾inì difonù.  Ve
festival-czech tak skuteènì èiníme.

Kompletní mno¾ina difonù festival-czech se skládá z následujících skupin:

- Spojení samohláska-souhláska: a-b a-c a-ch a-c~ a-d a-d~ a-dz a-dz~ a-f a-g
  a-h a-j a-k a-l a-m a-n a-n~ a-p a-r a-r~ a-s a-s~ a-t a-t~ a-v a-z a-z~ au-b
  au-c au-ch au-c~ au-d au-d~ au-dz au-dz~ au-f au-g au-h au-j au-k au-l au-m
  au-n au-n~ au-p au-r au-r~ au-s au-s~ au-t au-t~ au-v au-z au-z~ e-b e-c e-ch
  e-c~ e-d e-d~ e-dz e-dz~ e-f e-g e-h e-j e-k e-l e-m e-n e-n~ e-p e-r e-r~
  e-s e-s~ e-t e-t~ e-v e-z e-z~ eu-b eu-c eu-ch eu-c~ eu-d eu-d~ eu-dz eu-dz~
  eu-f eu-g eu-h eu-j eu-k eu-l eu-m eu-n eu-n~ eu-p eu-r eu-r~ eu-s eu-s~ eu-t
  eu-t~ eu-v eu-z eu-z~ i-b i-c i-ch i-c~ i-d i-d~ i-dz i-dz~ i-f i-g i-h i-j
  i-k i-l i-m i-n i-n~ i-p i-r i-r~ i-s i-s~ i-t i-t~ i-v i-z i-z~ i:-b i:-c
  i:-ch i:-c~ i:-d i:-d~ i:-dz i:-dz~ i:-f i:-g i:-h i:-j i:-k i:-l i:-m i:-n
  i:-n~ i:-p i:-r i:-r~ i:-s i:-s~ i:-t i:-t~ i:-v i:-z i:-z~ o-b o-c o-ch o-c~
  o-d o-d~ o-dz o-dz~ o-f o-g o-h o-j o-k o-l o-m o-n o-n~ o-p o-r o-r~ o-s
  o-s~ o-t o-t~ o-v o-z o-z~ ou-b ou-c ou-ch ou-c~ ou-d ou-d~ ou-dz ou-dz~ ou-f
  ou-g ou-h ou-j ou-k ou-l ou-m ou-n ou-n~ ou-p ou-r ou-r~ ou-s ou-s~ ou-t
  ou-t~ ou-v ou-z ou-z~ u-b u-c u-ch u-c~ u-d u-d~ u-dz u-dz~ u-f u-g u-h u-j
  u-k u-l u-m u-n u-n~ u-p u-r u-r~ u-s u-s~ u-t u-t~ u-v u-z u-z~

- Spojení souhláska-samohláska: b-a c-a ch-a c~-a d-a d~-a dz-a dz~-a f-a g-a
  h-a j-a k-a l-a m-a n-a n~-a p-a r-a r~-a s-a s~-a t-a t~-a v-a z-a z~-a b-au
  c-au ch-au c~-au d-au d~-au dz-au dz~-au f-au g-au h-au j-au k-au l-au m-au
  n-au n~-au p-au r-au r~-au s-au s~-au t-au t~-au v-au z-au z~-au b-e c-e ch-e
  c~-e d-e d~-e dz-e dz~-e f-e g-e h-e j-e k-e l-e m-e n-e n~-e p-e r-e r~-e
  s-e s~-e t-e t~-e v-e z-e z~-e b-eu c-eu ch-eu c~-eu d-eu d~-eu dz-eu dz~-eu
  f-eu g-eu h-eu j-eu k-eu l-eu m-eu n-eu n~-eu p-eu r-eu r~-eu s-eu s~-eu t-eu
  t~-eu v-eu z-eu z~-eu b-i c-i ch-i c~-i d-i d~-i dz-i dz~-i f-i g-i h-i j-i
  k-i l-i m-i n-i n~-i p-i r-i r~-i s-i s~-i t-i t~-i v-i z-i z~-i b-i: c-i:
  ch-i: c~-i: d-i: d~-i: dz-i: dz~-i: f-i: g-i: h-i: j-i: k-i: l-i: m-i: n-i:
  n~-i: p-i: r-i: r~-i: s-i: s~-i: t-i: t~-i: v-i: z-i: z~-i: b-o c-o ch-o c~-o
  d-o d~-o dz-o dz~-o f-o g-o h-o j-o k-o l-o m-o n-o n~-o p-o r-o r~-o s-o
  s~-o t-o t~-o v-o z-o z~-o b-ou c-ou ch-ou c~-ou d-ou d~-ou dz-ou dz~-ou f-ou
  g-ou h-ou j-ou k-ou l-ou m-ou n-ou n~-ou p-ou r-ou r~-ou s-ou s~-ou t-ou
  t~-ou v-ou z-ou z~-ou b-u c-u ch-u c~-u d-u d~-u dz-u dz~-u f-u g-u h-u j-u
  k-u l-u m-u n-u n~-u p-u r-u r~-u s-u s~-u t-u t~-u v-u z-u z~-u

- Spojení souhláska-neutrální foném a neutrální foném-souhláska, tato spojení
  se týkají jenom vybraných souhlásek, mezi nì¾ se neutrální foném vkládá: b-0
  d-0 d~-0 g-0 j-0 k-0 l-0 m-0 n-0 n~-0 p-0 r-0 t-0 t~-0 0-b 0-d 0-d~ 0-g 0-j
  0-k 0-l 0-m 0-n 0-n~ 0-p 0-r 0-t 0-t~

- Spojení souhláska-souhláska, nejsou zde kombinace souhlásek, mezi nì¾ se
  vkládá neutrální foném: b-c b-ch b-c~ b-dz b-dz~ b-f b-h b-l b-r~ b-s~ b-v
  b-z b-z~ d-c d-ch d-c~ d-dz d-dz~ d-f d-h d-l d-r~ d-s~ d-v d-z d-z~ d~-c
  d~-ch d~-c~ d~-dz d~-dz~ d~-f d~-h d~-l d~-r~ d~-s~ d~-v d~-z d~-z~ g-c g-ch
  g-c~ g-dz g-dz~ g-f g-h g-l g-r~ g-s~ g-v g-z g-z~ j-c j-ch j-c~ j-dz j-dz~
  j-f j-h j-l j-r~ j-s~ j-v j-z j-z~ k-c k-ch k-c~ k-dz k-dz~ k-f k-h k-l k-r~
  k-s~ k-v k-z k-z~ s-c s-ch s-c~ s-dz s-dz~ s-f s-h s-l s-r~ s-s~ s-v s-z s-z~
  m-c m-ch m-c~ m-dz m-dz~ m-f m-h m-l m-r~ m-s~ m-v m-z m-z~ n-c n-ch n-c~
  n-dz n-dz~ n-f n-h n-l n-r~ n-s~ n-v n-z n-z~ n~-c n~-ch n~-c~ n~-dz n~-dz~
  n~-f n~-h n~-l n~-r~ n~-s~ n~-v n~-z n~-z~ p-c p-ch p-c~ p-dz p-dz~ p-f p-h
  p-l p-r~ p-s~ p-v p-z p-z~ r-c r-ch r-c~ r-dz r-dz~ r-f r-h r-l r-r~ r-s~ r-v
  r-z r-z~ t-c t-ch t-c~ t-dz t-dz~ t-f t-h t-l t-r~ t-s~ t-v t-z t-z~ t~-c
  t~-ch t~-c~ t~-dz t~-dz~ t~-f t~-h t~-l t~-r~ t~-s~ t~-v t~-z t~-z~ c-b ch-b
  c~-b dz-b dz~-b f-b h-b l-b r~-b s~-b v-b z-b z~-b c-d ch-d c~-d dz-d dz~-d
  f-d h-d l-d r~-d s~-d v-d z-d z~-d c-d~ ch-d~ c~-d~ dz-d~ dz~-d~ f-d~ h-d~
  l-d~ r~-d~ s~-d~ v-d~ z-d~ z~-d~ c-g ch-g c~-g dz-g dz~-g f-g h-g l-g r~-g
  s~-g v-g z-g z~-g c-j ch-j c~-j dz-j dz~-j f-j h-j l-j r~-j s~-j v-j z-j z~-j
  c-k ch-k c~-k dz-k dz~-k f-k h-k l-k r~-k s~-k v-k z-k z~-k c-s ch-s c~-s
  dz-s dz~-s f-s h-s l-s r~-s s~-s v-s z-s z~-s c-m ch-m c~-m dz-m dz~-m f-m
  h-m l-m r~-m s~-m v-m z-m z~-m c-n ch-n c~-n dz-n dz~-n f-n h-n l-n r~-n s~-n
  v-n z-n z~-n c-n~ ch-n~ c~-n~ dz-n~ dz~-n~ f-n~ h-n~ l-n~ r~-n~ s~-n~ v-n~
  z-n~ z~-n~ c-p ch-p c~-p dz-p dz~-p f-p h-p l-p r~-p s~-p v-p z-p z~-p c-r
  ch-r c~-r dz-r dz~-r f-r h-r l-r r~-r s~-r v-r z-r z~-r c-t ch-t c~-t dz-t
  dz~-t f-t h-t l-t r~-t s~-t v-t z-t z~-t c-t~ ch-t~ c~-t~ dz-t~ dz~-t~ f-t~
  h-t~ l-t~ r~-t~ s~-t~ v-t~ z-t~ z~-t~ c-c ch-c c~-c dz-c dz~-c f-c h-c l-c
  r~-c s~-c v-c z-c z~-c c-ch ch-ch c~-ch dz-ch dz~-ch f-ch h-ch l-ch r~-ch
  s~-ch v-ch z-ch z~-ch c-c~ ch-c~ c~-c~ dz-c~ dz~-c~ f-c~ h-c~ l-c~ r~-c~
  s~-c~ v-c~ z-c~ z~-c~ c-dz ch-dz c~-dz dz-dz dz~-dz f-dz h-dz l-dz r~-dz
  s~-dz v-dz z-dz z~-dz c-dz~ ch-dz~ c~-dz~ dz-dz~ dz~-dz~ f-dz~ h-dz~ l-dz~
  r~-dz~ s~-dz~ v-dz~ z-dz~ z~-dz~ c-f ch-f c~-f dz-f dz~-f f-f h-f l-f r~-f
  s~-f v-f z-f z~-f c-h ch-h c~-h dz-h dz~-h f-h h-h l-h r~-h s~-h v-h z-h z~-h
  c-l ch-l c~-l dz-l dz~-l f-l h-l l-l r~-l s~-l v-l z-l z~-l c-r~ ch-r~ c~-r~
  dz-r~ dz~-r~ f-r~ h-r~ l-r~ r~-r~ s~-r~ v-r~ z-r~ z~-r~ c-s~ ch-s~ c~-s~
  dz-s~ dz~-s~ f-s~ h-s~ l-s~ r~-s~ s~-s~ v-s~ z-s~ z~-s~ c-v ch-v c~-v dz-v
  dz~-v f-v h-v l-v r~-v s~-v v-v z-v z~-v c-z ch-z c~-z dz-z dz~-z f-z h-z l-z
  r~-z s~-z v-z z-z z~-z c-z~ ch-z~ c~-z~ dz-z~ dz~-z~ f-z~ h-z~ l-z~ r~-z~
  s~-z~ v-z~ z-z~ z~-z~

- Spojení pauza-hláska a hláska-pauza: #-a #-au #-e #-eu #-i #-i: #-o #-ou #-u
  #-b #-c #-ch #-c~ #-d #-d~ #-dz #-dz~ #-f #-g #-h #-j #-k #-l #-m #-n #-n~
  #-p #-r #-r~ #-s #-s~ #-t #-t~ #-v #-z #-z~ a-# au-# e-# eu-# i-# i:-# o-#
  ou-# u-# b-# c-# ch-# c~-# d-# d~-# dz-# dz~-# f-# g-# h-# j-# k-# l-# m-#
  n-# n~-# p-# r-# r~-# s-# s~-# t-# t~-# v-# z-# z~-#

- Spojení dvou shodných souhlásek, toto spojení reprezentuje støedovou
  samohlásku, nikoliv pravý difon: a-a e-e i-i i:-i: o-o u-u

- Spojení samohláska-dvojhláska a dvojhláska-samohláska, jedná se o podobný
  trik jako v pøedchozí skupinì, tak¾e se napojují jen shodné samohlásky: a-au
  e-eu o-ou au-u eu-u ou-u

- Spojení samohláska-samohláska, která nespadají do pøedchozích dvou skupin,
  vyluèujeme té¾ samohlásky `i' a `i:', u kterých v takových kombinacích
  doplòujeme `j': a-e a-o e-a e-o o-a o-e u-a u-e u-o

Kompletní seznam zde definovaných èeských difonù je v souboru czech-diphones.

Po difonové databázi se nepo¾aduje, aby obsahovala kompletní seznam difonù.
Øada difonù ze skupiny souhláska-souhláska se v èe¹tinì mù¾e vyskytnout jenom
na hranicích slov, kde je lze nahradit skupinou souhláska-#-souhláska, kde # je
velmi krátká pauza.  Nìkteré dal¹í difony z této skupiny jsou si natolik
podobné, ¾e je lze reprezentovat jediným difonem, typicky nìkteré kombinace
znìlých a neznìlých souhlásek.  Ka¾dopádnì platí, ¾e èím více difonù, tím ménì
obezlièek a následných ztrát kvality; nedojde-li ov¹em pøi vìt¹ím mno¾ství
difonù ke ztrátì kvality vlivem únavy mluvèího.

** Pøidávání segmentù

Èást procesu stanovení výsledné sekvence difonù se provádí a¾ pøed samotnou
syntézou, v czech-after-analysis-hooks.  Do sekvence segmentù se zde pøidávají
umìlé segmenty.  Cílem tìchto akcí je vygenerování správné cílové sekvence
difonù, která nemusí nutnì odpovídat pøirozenému fonetickému zápisu
prostøednictvím jednotlivých fonémù.  Proto¾e se pøidávání segmentù provádí a¾
tìsnì pøed syntézou, není nutno je nijak zohledòovat v urèování prozodie.
Propagace prozodických informací do pøidaných segmentù je zaji¹tìna
automaticky.

** Slabiky

Ve festival-czech je za slabiku pova¾ováno celé mluvené slovo.  Prvním dùvodem
je, ¾e je to výhodné pøi urèování prozodie.  Druhým dùvodem je, ¾e rozklad slov
na slabiky je v èe¹tinì obtí¾nì urèitelný.  Pøi zkoumání slabik je relevantní
pouze jejich poèet, který je dán výskytem samohlásek a slabikotvorných
souhlásek.  Na to není rozklad na slabiky zapotøebí.


* Výslovnost jednotlivých slov

Urèování výslovnosti jednotlivých slov je øe¹eno primárnì LTS pravidly `czech'.
Proto¾e je v¹ak pravopis v souèasné èe¹tinì pøíli¹ nepravidelný, je nutno si
vypomáhat lexikonem.  Lexikon je pojatý jako seznam pøesnì tìch slov, která
nejsou a ani rozumnì být nemohou pokryta LTS pravidly.

Pro doplòování LTS pravidel a lexikonu je nezbytný seznam v¹ech èeských slov a
mù¾e být u¾iteèná funkce dump-pronounciation.

** Vygenerování seznamu v¹ech èeských slov

Vygenerování seznamu v¹ech èeských slov vèetnì jejich tvarù lze provést
s pomocí èeského ispellu (viz [koláø]).  Seznam slov v základním tvaru je
vygenerován ze zdrojových souborù èeského ispellu a nachází se v souboru
czech-words.  Seznam v¹ech tvarù tìchto slov není pro svou rozsáhlost
distribuován.  Máte-li instalován èeský ispell, mù¾ete tento seznam vygenerovat
pøíkazem `make czech-words-all'.

** Funkce dump-pronounciation

Funkce dump-pronounciation, obsa¾ená v pøilo¾eném souboru
dump-pronounciation.scm, slou¾í k pøevodu psané formy slova na výslovnost.
Lze ji typicky vyu¾ít pro testování LTS pravidel a usnadnìní pøidávání
nových slov do lexikonu.  Funkce pou¾ívá výhradnì LTS pravidla, nepou¾ívá
lexikon.

Funkce dump-pronunciation má dva argumenty.  Prvním je jméno souboru
obsahujícího slova, která mají být pøevedena, druhým je jméno souboru, do
kterého má být zapsán výstup.  Ve vstupním souboru je oèekáváno právì jedno
slovo na ka¾dém øádku souboru.  Doporuèuje se, aby slova byla uzavøena
v uvozovkách, jinak pøi vìt¹ím mno¾ství slov narazíte na výkonnostní potí¾e
Festivalu.

** Jak postupovat pøi nalezení nesprávné výslovnosti slova

Pokud je slovo obsa¾eno v lexikonu, pøíslu¹ný záznam se jednodu¹e opraví.
Pokud slovo není obsa¾eno v lexikonu, mìlo by být v první øadì posouzeno, zda
pøíslu¹ný problém není ¹ir¹ího rázu a nestálo by za to jej o¹etøit pøidáním LTS
pravidla.

Jedná-li se spí¹e o výjimku, kterou nemá smysl zohledòovat v LTS pravidlech,
pøidá se do lexikonu.  Jde-li o slovo ohebné, mìly by být pøidány v¹echny jeho
tvary, jsou-li vyslovovány chybnì té¾.  Je dobré zamyslet se i nad formami
vytvoøenými pøidáním nebo odebráním pøedpon.  Pøi generování v¹ech tvarù lze
pro usnadnìní práce vyu¾ít èeský ispell (viz [koláø]) a funkci
dump-pronounciation.scm.  Pøidáváte-li do lexikonu slovo, jeho¾ základní tvar
není uveden v souboru czech-words, pøidejte jej tam (a pokud mo¾no jej té¾
oznamte autorovi èeského ispellu).  Zachovejte pøitom abecední poøadí slov
v souboru (pøi nastavení locales na cs_CZ.iso8859-2).

Pokud dojde na pøidání nového LTS pravidla, je zapotøebí provìøit, jaké zmìny
zpùsobí.  Optimální by bylo nechat vygenerovat výslovnost v¹ech èeských slov
pøed zmìnou LTS pravidel a po ní a následnì srovnat jejich diff výstupy.  To je
v¹ak na bì¾ných strojích pomìrnì zdlouhavá zále¾itost a je tedy praktiètìj¹í ji
aplikovat pouze na mno¾inu slov, které mohou být pøíslu¹ným pravidlem dotèeny.
Je-li nové LTS pravidlo øádnì ovìøeno, mù¾e být pøidáno.  Volitelnì lze
provìøit slova v lexikonu, zda se výslovnost nìkterých z nich neshoduje
s výsledkem po aplikaci nových LTS pravidel, a taková slova z lexikonu vyøadit.


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

Pou¾íváme pauzy tøí délek: nejdel¹í (BB), støední (B) a krátká (SB).  Dlouhá
pauza se vkládá na koncích vìt, støední pauza mezi vìty v souvìtí a v seznamech
slov a krátká pauza pøed spojky bez èárky.  Detekce souvìtí a seznamu slov
v místech, kde se nenachází èárka, je provádìno triviální (a samozøejmì v mnoha
pøípadech chybující) heuristikou.  Umis»ování tìchto pauz se definuje
v promìnné czech-phrase-cart-tree.

Pou¾itá metoda je na¹ím vlastním výtvorem a neopírá se o ¾ádné fonetické
poznatky.

** Pøízvuk

Dle [palková:04], sekce 1.2db), není v syntéze èe¹tiny ¾ádoucí pøízvuk
explicitnì generovat zmìnou dynamiky.  Pøízvuk je modelován intonaèní køivkou.


* Odkazy

[palková:04]
  Zdena Palková: Soubor fonetických pravidel jako podklad pro prozodický
  komponent automatické syntézy TTS v èe¹tinì
  <<doplnit pøesnou citaci>>

[palková-ptáèek:97]
  Zdena Palková, Miroslav Ptáèek: Modelling Prosody in TTS Diphone Synthesis in
  Czech; Forum Phoneticum 63, Frankfurt am Main 1997

[palková-ptáèek:94]
  Zdena Palková, Miroslav Ptáèek: Ein Beitrag zur Intonation in der
  Diphonsynthese; Phonetica Pragensia VIII, Univerzita Karlova Praha 1994

[horák:02]
  Petr Horák: Modelování suprasegmentálních rysù mluvené èe¹tiny pomocí
  lineární predikce; dizertaèní práce; ÈVUT, Fakulta elektrotechnická,
  Praha 2002

[koláø]
  Petr Koláø: ispell-czech -- èeská kontrola pravopisu
  viz ftp://ftp.vslib.cz/pub/unix/ispell/


-- Milan Zamazal <pdm@freebsoft.org>
