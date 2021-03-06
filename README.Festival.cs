Stru�n� popis p�evodu textu do zvukov� podoby ve Festivalu
==========================================================

Cel� proces �e�ov� synt�zy Festivalu je velmi podrobn� pops�n v�dokumentu
Building Synthetic Voices, kter� je k�m�n� na www.festvox.org a lze jej
doporu�it ka�d�mu, kdo chce festivalov� mechanismy pochopit do dostate�n�
hloubky.  Dal�� informace jsou pak k�dispozici v�manu�lu Festivalu, ty jsou
v�ak v�ur�it�ch sm�rech ne�pln� a bez v��e uveden�ho dokumentu se p�i
seri�zn�j�� pr�ci na nov�m festivalov�m jazyce a/nebo hlasu nelze obej�t.

Tento dokument je stru�n�m popisem synt�zy a dostupn�ch n�stroj� pro ty, kdo
pot�ebuj� z�skat z�kladn� orientaci v�procesu festivalov� synt�zy a p�itom
nemaj� �as anebo z�jem se v�novat zdlouhav�mu a podrobn�mu studiu v��e
uveden�ch dokument�.  Nejedn� se o�*u�ivatelsk�* �vod do Festivalu, znalost
u�ivatelsk� pr�ce s�Festivalem je p�edpokl�d�na.

Konkr�tn� postupy pou�it� pro �e�tinu jsou pops�ny ve zdrojov�m k�du
festival-czech (technick� informace) a v�souboru README.Czech.cs (jazykov�
informace).


* Z�kladn� principy Festivalu

Festival je syst�m modul�rn� a cel� proces se skl�d� z�proveden� libovoln�
sekvence modul�.  Ka�d� modul m� sv�j ��el a pro n�kter� ��sti zpracov�n� si
lze vybrat z�v�ce alternativn�ch modul�.  Lze t� pou��t libovoln� moduly
vlastn�.

Moduly se obvykle p��ou ve schemov�m programovac�m jazyce Festivalu zvan�m
SIOD.  Tento jazyk je pops�n v�manu�lu Festivalu.


* F�ze zpracov�n�

Zpracov�n� textu se standardn� skl�d� z�n�sleduj�c�ch f�z�, definovan�ch
v�synthesis.scm:

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

V�e se to�� okolo tzv. utterance, kter� p�edstavuje jakousi jednotku, v�ce �i
m�n� anotovanou, kterou lze postupn�m dopl�ov�n�m anotac� (*features*) dostat
a� do stavu, kdy je schopna odesl�n� na zvukov� v�stup.  Ka�d� z�v��e uveden�ch
f�z� je vol�n� funkce na utterance `utt'.


* Pr�ce s�utterance

Pot�ebujete-li prov�st synt�zu ur�it�ho textu, vytvo��te si p��slu�n� utterance
nap��klad n�sleduj�c�m zp�sobem:

  (Utterance Text "n�jak� text")

Funkce vr�t� nezpracovan� utterance, kter� lze nechat pln� zanalyzovat vol�n�m
funkce `utt.synth'.  Pozor, ve festivalov�m ��dkov�m rozhran� nelze zad�vat
8-bitov� znaky, v�p��pad� pot�eby je nutno synt�zu prov�st p�es soubor.

Utterance se skl�d� z�tzv. relac�, jejich� jm�na lze zjistit pomoc�

  (utt.relationnames utterance)

Obsah (seznam polo�ek, items) ��dan� relace se vyt�hne pomoc�

  (utt.relation.items utterance 'relation-name)

Mnoh� polo�ky se vyskytuj� ve v�ce relac�ch.  Relace m� obecn� podobu stromu a
prost�ednictv�m polo�ek mohou b�t tyto stromy vz�jemn� prov�z�ny.  Relace
polo�ky lze zjistit pomoc�

  (item.relations item)

Z�kladn�mi vlastnostmi polo�ky jsou jej� jm�no a rysy:

  (item.name item)

Rysy polo�ky lze zjistit pomoc�

  (item.features item)

Lze si vyzkou�et:

  (mapcar item.features (utt.relation.items utterance 'Word))

Pro vyps�n� relace je p�ehledn�j�� v�stup z

  (utt.relation.print utterance 'Word)

Lze t� pou��t

  (utt.relation_tree utt 'SylStructure)

Funkce utt.relation_tree vrac� kompletn�j�� informaci o�dan� relaci, obsahuje
cel� strom dat obsa�en�ch v�relaci, zat�mco utt.relation.print vypisuje pouze
prvky nejvy��� �rovn�.

Ob�as nar��me na pojem glob�ln�ho parametru.  Hodnotu glob�ln�ho parametru lze
z�skat vol�n�m

  (Param.get 'jm�no-parametru)


* Stru�n� popis f�z� zpracov�n�

** Initialize

Jen vytvo�� pr�zdn� utterance.

** Text

Provede rozd�len� textu na tokeny.  Funkce `Text' je naps�na v�C++, vyu��v�
v�ak prom�nn� `token.*' definovan� v�token.scm.

Definovan� interpunk�n� znam�nka jsou odd�lena od v�sledn�ch token� a jsou
dostupn� jako jejich features.

** Token_POS

Prov�d�, je-li t�eba, kontextov� ozna�kov�n� tokenu pro rozli�en� r�zn�ho
v�znamu shodn�ch token�.  Tato f�ze slou�� pro ur�en� spr�vn� v�slovnosti slov
ve f�zi bezprost�edn� n�sleduj�c�, pozd�j�� f�ze POS m� ��el jin�.

** Token

P�ev�d� tokeny na slova.  P�evod lze prov�st definic� funkce token_to_words.
P�i konverzi tokenu je mo�no se pod�vat na okoln� tokeny (nebo l�pe vyu��vat
kontextov�ch informac� z�skan�ch ve f�zi Token_POS), tak�e je mo�no prov�d�t
i�sofistikovan�j�� operace ne� pouh� mapov�n� jedin�ho tokenu na jedno nebo
v�ce slov.

** POS (POS == Part of Speech)

Prov�d� tagov�n� konkr�tn�ch slov dle jejich pozice v��e�i.  Nepovinn� ��st.
Parametrizovateln� pomoc� n�kolika prom�nn�ch.  Tato f�ze, na rozd�l od f�ze
Token_POS, ur�uje v�znam jednotliv�ch slov ji� nikoliv pro ur�en� v�slovnosti
slov, n�br� pro ur�en� jejich role ve vztahu k�n�sledn�mu ur�en� prozodie,
tj. pauz, d�lek, p��zvuku a intonace.

** Phrasify

Identifikace pauz (��dn�, norm�ln�, dlouh�) mezi slovy.  Mezi v�t�inu slov se
nevkl�d� ��dn� pauza, vkl�d� se za interpunkci, n�kdy kr�tk� (po ��rce), n�kdy
dlouh� (konec v�ty).  Tato funkce sama o�sob� pauzy nevkl�d�, to se d�je a� na
z�klad� j� generovan�ch informac� v�Pauses.

** Word

Prov�d� p�evod slov na fon�my a slabiky v�implicitn�m festivalov�m form�tu.
Definov�no v�lexicon.scm, nen�-li glob�ln�m parametrem Word_Method �e�eno
jinak, vol� se C++ funkce Classic_Word.

Cel� process je pom�rn� podrobn� pops�n v�dokumentaci, sekce Lexicons.

** Pauses

Vkl�d� pauzy.  Pro tuto akci se pou��v� funkce definovan� glob�ln�m parametrem
Pause_Method.  Implicitn� je funkce Classic_Pauses, definovan� v�pauses.scm.
Funkce d�l� v�z�sad� to, �e vlo�� po��te�n� pauzu a pak vlo�� pauzy dle
informac� vytvo�en�ch ve f�zi Phrasify.  Nav�c vy�ad� z�utterance slova
ozna�en� jako interpunkce.

Vkl�d�n� �vodn� pauzy lze odru�it p�edefinov�n�m funkce insert_initial_space,
nem�lo by se to v�ak d�t v�jin�ch ne� od�vodn�n�ch p��padech, jinak t�m utrp�
v�sledn� kvalita synt�zy.

** Intonation

Generuje akcenty pro intonaci (prvn� f�ze zpracov�n� intonace), ale nevytv���
je�t� intonaci samotnou (druh� f�ze zpracov�n� intonace), to d�l� a�
Int_Targets.  Pou��v� se funkce definovan� glob�ln�m parametrem Int_Method.

** PostLex

V�t�to f�zi lze aplikovat jak�koliv dodate�n� transformace nad utterance,
za�azen� v�seznamu postlex_rules_hooks.

** Duration

Ur�� d�lky trv�n� jednotliv�ch segment� (fon�m�) syntetizovan�ho textu.

** Int_Targets

Ur�� parametry intona�n� k�ivky, obvykle po slabik�ch.

** Wave_Synth

Samotn� sestaven� zvuku na z�klad� ji� v�ech dostupn�ch anotac�.  Funkce, kter�
se pro synt�zu zavol�, je definov�na glob�ln�m parametrem `Synth_Method'.
Kostra funkce Wave_Synth je naps�na ve Scheme a nach�z� se v�synthesis.scm.


* Praktick� pozn�mky.

Docstringy um� vypsat funkce `doc'.

Readline um� dopl�ovat jm�na funkc�, co� je �asto u�ite�n�.


-- Milan Zamazal
