Struèný popis pøevodu textu do zvukové podoby ve Festivalu
==========================================================

Celý proces øeèové syntézy Festivalu je velmi podrobnì popsán v dokumentu
Building Synthetic Voices, který je k mání na www.festvox.org a lze jej
doporuèit ka¾dému, kdo chce festivalové mechanismy pochopit do dostateèné
hloubky.  Dal¹í informace jsou pak k dispozici v manuálu Festivalu, ty jsou
v¹ak v urèitých smìrech neúplné a bez vý¹e uvedeného dokumentu se pøi
serióznìj¹í práci na novém festivalovém jazyce a/nebo hlasu nelze obejít.

Tento dokument je struèným popisem syntézy a dostupných nástrojù pro ty, kdo
potøebují získat základní orientaci v postupu festivalové syntéz a pøitom
nemají èas anebo zájem se vìnovat zdlouhavému a podrobnému studiu vý¹e
uvedených dokumentù.  Nejedná se o *u¾ivatelský* úvod do Festivalu, znalost
u¾ivatelské práce s Festivalem je pøedpokládána.


* Základní principy Festivalu

Festival je systém modulární a celý proces se skládá z provedení libovolné
sekvence modulù.  Ka¾dý modul má svùj úèely a pro nìkteré èásti zpracování si
lze vybrat z více alternativních modulù.  Lze té¾ pou¾ít libovolné moduly
vlastní.

Moduly se obvykle pí¹ou ve schemovém programovacím jazyce Festivalu SIOD.
Tento jazyk je popsán v manuálu Festivalu.


* Fáze zpracování

Zpracování textu se skládá z následujících fází, definovaných v synthesis.scm:

(defUttType Text
  (Initialize utt)
  (Text utt)
  (Token_POS utt)
  (Token utt)
  (POS utt)
  (Phrasify utt)
  (Word utt)
  (Pauses utt)
  (Intonation utt)
  (PostLex utt)
  (Duration utt)
  (Int_Targets utt)
  (Wave_Synth utt)
  )

V¹e se toèí okolo tzv. utterance, které pøedstavuje jakousi jednotku, více èi
ménì anotovanou, kterou lze postupným doplòováním anotací (*features*) dostat
a¾ do stavu, kdy je schopna odeslání na zvukový výstup.  Ka¾dá z vý¹e uvedených
fází je volání funkce na utterance `utt'.


* Práce s utterance

Potøebujete-li provést syntézu urèitého textu, vytvoøíte si pøíslu¹né utterance
napøíklad následujícím zpùsobem:

  (Utterance Text "nìjaký text")

Funkce vrátí nezpracované utterance, které lze nechat plnì zanalyzovat voláním
funkce `utt.synth'.  Pozor, ve festivalovém øádkovém nelze zadávat 8-bitové
znaky, v pøípadì potøeby je nutno jít pøes *.scm soubor.

Utterance se skládá z relations, jejich¾ jména lze zjistit pomocí

  (utt.relationnames utterance)

Obsah (seznam polo¾ek, items) ¾ádaného relation se vytáhne pomocí

  (utt.relation.items utterance 'relation-name)

Základními vlastnostmi polo¾ky jsou její jméno a rysy:

  (item.name item)

Mnohé polo¾ky se objevují na více místech, napøíklad konkrétní Word mù¾e mít
pøiøazeno Phrase, takové Phrase je pak dostupno jako rys Word.  Rysy polo¾ky
lze zjistit pomocí

  (item.features item)

Lze si vyzkou¹et:

  (mapcar item.features (utt.relation.items utterance 'Word))

Pro vypsání relace je pøehlednìj¹í výstup z

  (utt.relation.print utterance 'Word)

Lze té¾ pou¾ít

  (utt.relation_tree utt 'SylStructure)

Funkce vrací kompletnìj¹í informaci o dané relaci, utt.relation.print mù¾e
nìkterá data zamlèet.

Obèas nará¾íme na pojem globálního parametru.  Hodnotu globálního parametru lze
získat voláním

  (Param.get 'jméno-parametru)


* Struèný popis fází zpracování

** Initialize

Jen vytvoøí prázdné utterance.

** Text

Provede rozdìlení textu na tokens.  Funkce `Text' je napsána v C++, vyu¾ívá
v¹ak promìnné `token.*' definované v token.scm.

Definovaná interpunkèní znaménka jsou oddìlena od výsledných tokenù a jsou
dostupná jako jejich features.

** Token_POS

Provádí, je-li tøeba, kontextové oznaèkování tokenu pro rozli¹ení rùzného
významu shodných tokenù.  Tato fáze slou¾í pro urèení správné výslovnosti slov
ve fázi bezprostøednì následující, pozdìj¹í fáze POS má úèel jiný.

** Token

Pøevádí tokeny na slova.  Pøevod lze provést definicí funkce token_to_words.
Pøi konverzi tokenu je mo¾no se podívat na okolní tokeny (nebo lépe vyu¾ívat
kontextových informací získaných ve fázi Token_POS), tak¾e je mo¾no provádìt
i sofistikovanìj¹í operace ne¾ pouhé mapování jediného tokenu na jedno nebo
více slov.

** POS (POS == Part of Speech)

Provádí tagování konkrétních slov dle jejich pozice v øeèi.  Nepovinná èást.
Parametrizovatelné pomocí nìkolika promìnných.  Pro èe¹tinu pou¾íváme vlastní
postup.  Tato fáze, na rozdíl od fáze Token_POS, urèuje význam jednotlivých
slov ji¾ nikoliv pro urèení výslovnosti slov, nýbr¾ pro urèení jejich role ve
vztahu k následnému urèení prozodie, tj. pomlk, délek, pøízvuku a intonace.

** Phrasify

Identifikace pauz (¾ádná, normální, dlouhá) mezi slovy.  Mezi vìt¹inu slov se
nevkládá ¾ádná pauza, vkládá se za interpunkci, nìkdy krátká (po èárce), nìkdy
dlouhá (konec vìty).  Tato funkce sama o sobì pauzy nevkládá, to se dìje a¾ na
základì jí generovaných informací v Pauses.

Pou¾íváme jednodu¹¹í, nestatistickou, metodu `cart_tree', pøièem¾ zohledòujeme
v první øadì interpunkci.

** Word

Provádí pøevod slov na fonémy a slabiky v implicitním festivalovém formátu.
Definováno v lexicon.scm, není-li globálním parametrem Word_Method øeèeno
jinak, volá se C++ funkce Classic_Word.

Celý process je pomìrnì podrobnì popsán v dokumentaci, sekce Lexicons.
Definovaný postup pro èe¹tinu funguje zhruba následujícím zpùsobem:

- Výjimky jsou definovány v lexikonu, viz czech-lexicon.scm.

- Jinak se uplatòuje funkce czech-lts.

- czech-lts nejprve pøevede text na fonémy a pak volá obvyklou festivalovou
  funkci czech-syllabify-phstress pro jejich sestavení do slabik.  Slabiky jsou
  identifikovány v C++ funkcí `syl_breakable' podle samohlásek (které jsou
  definovány ve phone set).  Proto¾e anglická funkce na èe¹tinu nefunguje
  dobøe, pou¾ívám místo lex.syllabify.phstress uvedenou funkci vlastní.

** Pauses

Vkládá pauzy.  Pro tuto akci se pou¾ívá funkce definovaná globálním parametrem
Pause_Method.  Implicitní je funkce Classic_Pauses, definovaná v pauses.scm.
Funkce dìlá v zásadì to, ¾e vlo¾í poèáteèní pauzu a pak vlo¾í pauzy dle
informací vytvoøených ve fázi Phrasify.  Navíc vyøadí z utterance slova
oznaèená jako interpunkce.

Vkládání úvodní pauzy lze odru¹it pøedefinováním funkce insert_initial_space,
nemìlo by se to v¹ak dít v jiných ne¾ odùvodnìných pøípadech, jinak tím utrpí
výsledná kvalita syntézy.

** Intonation

Generuje akcenty pro intonaci (první fáze zpracování intonace), ale nevytváøí
je¹tì intonaci samotnou (druhá fáze zpracování intonace), to dìlá a¾
Int_Targets.  Pou¾ívá se funkce definovaná globálním parametrem Int_Method.

** PostLex

V této fázi lze aplikovat jakékoliv dodateèné transformace nad utterance,
zaøazené v seznamu postlex_rules_hooks.

** Duration

Urèí délky trvání jednotlivých fonémù syntetizovaného textu.

** Int_Targets

Urèí parametry intonaèní køivky, obvykle po slabikách.

** Wave_Synth

Samotné sestavení zvuku na základì ji¾ v¹ech dostupných anotací.  Funkce, která
se pro syntézu zavolá, je definována globálním parametrem `Synth_Method'.
Funkce Wave_Synth je ve Scheme a nachází se v synthesis.scm.


* Praktické poznámky.

Docstringy umí vypsat funkce `doc'.

Readline umí doplòovat jména funkcí, co¾ je èasto u¾iteèné.


* Pøíklady

** Angliètina

Analýza textu "Hello, world! How are you?":

festival> (set! utt (Utterance Text "Hello, world!  How are you?"))
#<Utterance 0x40757ec8>

... nutno dopsat ...

