Popis postup� �esk� synt�zy pou�it�ch ve festival-czech
=======================================================

* Z�kladn� jednotky �e�i

Z�kladn� jednotky �e�i byly definov�ny dle difonov� sady vypracovan�
Mgr. Pavlem Macha�em.  Tento materi�l je dostupn� v�bal�ku voice-czech-ph.

** Fon�my

Fon�mov� sada je definovan� standardn�m zp�sobem v�czech.scm.  Vlastnosti
fon�m� definovan� v�czech.scm byly definov�ny dle pot�eb �esk� synt�zy, bez
p��m� n�vaznosti na konkr�tn� jazykov� nebo fonetick� poznatky.  R�zn� fon�my
p�itom mohou m�t zcela shodn� vlastnosti, nebylo snahou u�init fon�my sv�mi
vlastnostmi unik�tn�.

** Difony

Pou��v�me difonovou sadu z�v��e zm�n�n�ho materi�lu Mgr. Pavla Macha�e.  Ta
v�rozumn� m��e aproximuje mno�inu dvojic hl�sek vyskytuj�c�ch se v��e�tin�.

** P�id�v�n� segment�

��st procesu stanoven� v�sledn� sekvence difon� se prov�d� a� p�ed samotnou
synt�zou, v�czech-after-analysis-hooks.  Do sekvence segment� se zde p�id�vaj�
um�l� segmenty.  C�lem t�chto akc� je dosa�en� lep��ho v�sledn�ho zvuku a
p��padn� t� konverze fon�m� pro syntetiz�ry pou��vaj�c� jinou fon�movou sadu.
Proto�e se p�id�v�n� segment� prov�d� a� t�sn� p�ed synt�zou, nen� nutno je
nijak zohled�ovat v�ur�ov�n� prozodie.  Propagace prozodick�ch informac� do
p�idan�ch segment� je zaji�t�na automaticky.

** Slabiky

Ve festival-czech je za slabiku pova�ov�no cel� mluven� slovo.  Prvn�m d�vodem
je, �e je to v�hodn� p�i ur�ov�n� prozodie.  Druh�m d�vodem je, �e rozklad slov
na slabiky je v��e�tin� obt�n� ur�iteln�.  P�i zkoum�n� slabik je relevantn�
pouze jejich po�et, kter� je d�n v�skytem samohl�sek a slabikotvorn�ch
souhl�sek.  Na to nen� rozklad na slabiky zapot�eb�.


* V�slovnost jednotliv�ch slov

Ur�ov�n� v�slovnosti jednotliv�ch slov je �e�eno prim�rn� LTS pravidly `czech'.
Korekce fonetick� formy, ji� nez�visl� na pravopisn�ch pravidlech, se prov�d�
ve funkci czech-adjust-segments.  Tato funkce se vol� a� po zpracov�n� pauz,
proto�e pauzy jsou pro ur�en� v�sledn� fonetick� podoby podstatn�.

Proto�e sou�asn� �esk� pravopis je p��li� nepravideln�, je nutno si p�i p�evodu
psan� formy na fonetickou vypom�hat lexikonem.  Lexikon je pojat� jako seznam
p�esn� t�ch slov, kter� nejsou a ani rozumn� b�t nemohou pokryta LTS pravidly
nebo pro kter� by nespr�vn� prob�hl p�evod z�tokenu na slovo (nap��klad
v�p��pad� slova _pst_, kter� by bylo hl�skov�no).

** Jak postupovat p�i nalezen� nespr�vn� v�slovnosti slova

Pokud je slovo obsa�eno v�lexikonu, p��slu�n� z�znam se jednodu�e oprav�.
Pokud slovo nen� obsa�eno v�lexikonu, m�lo by b�t v�prvn� �ad� posouzeno, zda
p��slu�n� probl�m nen� �ir��ho r�zu a nest�lo by za to jej o�et�it p�id�n�m LTS
pravidla.

Jedn�-li se sp�e o�v�jimku, kterou nem� smysl zohled�ovat v�LTS pravidlech,
p�id� se do lexikonu.  Jde-li o�slovo ohebn�, m�ly by b�t p�id�ny v�echny jeho
tvary, jsou-li vyslovov�ny chybn� t�.  Je dobr� zamyslet se i�nad formami
vytvo�en�mi p�id�n�m nebo odebr�n�m p�edpon.

Pokud dojde na p�id�n� nov�ho LTS pravidla, je zapot�eb� prov��it, jak� zm�ny
zp�sob�.  Optim�ln� by bylo nechat vygenerovat v�slovnost v�ech �esk�ch slov
p�ed zm�nou LTS pravidel a po n� a n�sledn� srovnat jejich diff v�stupy.  To je
v�ak na b�n�ch stroj�ch pom�rn� zdlouhav� z�le�itost a je tedy prakti�t�j�� ji
aplikovat pouze na mno�inu slov, kter� mohou b�t p��slu�n�m pravidlem dot�eny.
Je-li nov� LTS pravidlo ��dn� ov��eno, m��e b�t p�id�no.  Voliteln� lze
prov��it slova v�lexikonu, zda se v�slovnost n�kter�ch z�nich neshoduje
s�v�sledkem po aplikaci nov�ch LTS pravidel, a�takov� slova z�lexikonu vy�adit.

** Tokeny, kter� se expanduj� na v�ce slov

Je-li z�pis ur�it�ho slova expandov�n na v�cero slov, je nutno jeho expanzi
definovat v�prom�nn� czech-multiword-abbrevs.  Takov� slova nepat�� do lexikonu
-- lexikon definuje jen v�slovnost jednotliv�ch slov.

** Pravidla pro p�epis z�textu do fonetick� podoby

P�i p�episu textu do fonetick� podoby je v�lexikonu dovoleno pou��vat ve�ker�
fon�my definovan� v��esk� fon�mov� sad�.  P�itom mus� b�t dodr�ena n�sleduj�c�
pravidla.

*** Konverze zn�l�ch souhl�sek na nezn�l� a naopak

Neprov�d� se konverze zn�l�ch souhl�sek na nezn�l� a naopak, kde to nen�
vylo�en� nutn�.  Nap��klad spr�vn� (z�hlediska festival-czech) fonetick� p�epis
slova _ovce_ je (o v c e), nikoliv (o f c e).  V�slovnost difonu v-c se toti�
jednak v�tomto slov� podstatn� neli�� od v�slovnosti tohoto difonu v�jin�ch
slovech a jednak se s�opravdovou zn�lou v�slovnost� tohoto difonu v��e�tin�
nesetk�me (ani na hranic�ch slov a slovech p�ejat�ch).  Jeho spr�vn� v�slovnost
tedy bude zaji�t�na v�difonov� datab�zi nebo jej�ch konverzn�ch pravidlech.

Naproti spr�vn� p�epis slova _magnetismus_ je (m a g n e t i z m u s), proto�e
v�n�m se skute�n� jedn� o�pozm�n�nou v�slovnost slova p�ejat�ho a v�koncovce je
�esk� difon z-m, nikoliv �esk� difon s-m.

Toto pravidlo ned�v� z�fonetick�ho hlediska smysl.  Jeho c�lem je v�ak
zjednodu�en� tvorby polo�ek lexikonu (p�isp�vatel se m��e v�ce dr�et psan�
formy a nemus� se zab�vat speci�ln�mi p��pady) a zamezen� lidov� tvo�ivosti
v�nejednozna�n�ch p��padech.

*** Vkl�d�n� r�z�

Je nutno nezapom�nat na vlo�en� r�z� mezi samohl�skami.  Spr�vn� p�epis slova
_neefektivn�_ je (n e _ e f e k t i v n~ i:).  Naopak spr�vn� p�epis slova
_poet_ je (p o e t).

*** Dvojhl�sky

Dvojhl�sky se reprezentuj� odpov�daj�c� dvojic� hl�sek.  Speci�ln�m p��padem
jsou dvojhl�sky obsahuj�c� `i' nebo `i:'.  Ve skute�nosti se nejedn�
o�dvojhl�sky, vyslovuje se toti� mezi nimi hl�ska `j'.  Spr�vn� p�epis slova
_poezie_ tedy je (p o e z i j e).


* Prozodie

Prozodie ve festival-czech zahrnuje intonaci, d�lku, pauzy a p��zvuk.  Tyto
parametry jsou konstruov�ny p�ev�n� na z�klad� pravidel publikovan�ch
v�[palkov�:04].

V�tn� �seky a p��zvukov� takty jsou konstruov�ny ve f�zi Word, jako zvl�tn�
relace IntUnit a StressUnit.

** Intonace

Pravidla pro intonaci byla p�evzata z�[palkov�:04].  Konkr�tn� intona�n�
tabulka kadenc� byla p�evzata z�[palkov�-pt��ek:97].

V�publikovan�ch prozodick�ch pravidlech jsou ur�it� nejasnosti, kter� byly
roz�e�eny n�sleduj�c�m zp�sobem:

- Pro �esti a v�ceslabi�n� p��zvukov� takty se v�pozici F pou��v� intona�n�
  k�ivka ze skupiny A (v�[palkov�-pt��ek:97] p��slu�n� pravidlo pro pozici F
  chyb�).

- Pro pozici F se vol� intona�n� k�ivka ze skupiny A (v�[palkov�:04] nen�
  specifikov�no).

- Pro pozici F-1 se vyb�r� pouze z�intona�n�ch k�ivek povolen�ch pro skupinu F
  (pro jistotu -- v�[palkov�:04] nen� specifikov�no, k��emu by s�ohledem na
  p�edchoz� �pravu m�ly b�t F-k�ivky skupiny B).

Nedo�e�ena je detekce dopl�kov�ch t�zac�ch v�t ve funkci czech-yes-no-question,
kde chyb� dostate�n� obs�hl� seznam t�zac�ch z�jmen, ��slovek a p��slovc�.
M�lo by b�t mo�n� jej z�skat s�pomoc� �esk�ho ispellu.
  
** D�lka

D�lka se stanovuje na z�klad� po�tu slabik v�p��zvukov�m taktu dle tabulky
z�[palkov�:04].

** Pauzy

Pou��v�me pauzy t�� d�lek: nejdel�� (BB), st�edn� (B) a kr�tk� (SB).
Umis�ov�n� pauz se definuje v�prom�nn� czech-phrase-cart-tree a funkci
czech-adjust-phrase-breaks.  Pou�it� metoda ur�ov�n� pauz vych�z�
z�[palkov�-pt��ek:66], nen� v�ak p�esnou implementac� t�chto pravidel.

** P��zvuk

Dle [palkov�:04], sekce 1.2db), nen� v�synt�ze �e�tiny ��douc� p��zvuk
explicitn� generovat zm�nou dynamiky.  P��zvuk je modelov�n intona�n� k�ivkou.


* Odkazy

[palkov�:04]
  Zdena Palkov�: Soubor fonetick�ch pravidel jako podklad pro prozodick�
  komponent automatick� synt�zy TTS v��e�tin�

[palkov�-pt��ek:97]
  Zdena Palkov�, Miroslav Pt��ek: Modelling Prosody in TTS Diphone Synthesis in
  Czech; Forum Phoneticum 63, Frankfurt am Main 1997

[palkov�-pt��ek:66]
  Zdena Palkov�, Miroslav Pt��ek: TTS Issues: Prosody modifications in Text;
  in Speech processing, 6th Czech-German Workshop, Prag 1966, R. V�ch
  (ed.) pp.32-34
  + intern� materi�l Z. Palkov�, Z�v�re�n� zpr�va grantu GA�R 405/96/0301


-- Milan Zamazal

Local variables:
mode: outline
end:
