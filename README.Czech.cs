Popis postup� �esk� synt�zy pou�it�ch ve festival-czech
=======================================================

* Z�kladn� jednotky �e�i

Z�kladn� jednotky �e�i byly definov�ny dle [palkov�-pt��ek:94], sekce�2.

�ir�� seznam jednotek �e�i pro �e�tinu lze nal�zt v�[hor�k:02].

** Fon�my

K�fon�m�m z�[palkov�-pt��ek:94] byly do �esk� fon�mov� sady nav�c p�id�ny
fon�my odpov�daj�c� dlouh�m samohl�sk�m �, �, �, �.  Nen�-li v�konkr�tn�m
�esk�m hlase stanoveno jinak, jsou tyto pou��v�ny pouze b�hem stanoven�
prozodie, p�ed fin�ln� synt�zou zvuku jsou p�evedeny na jejich kr�tk�
ekvivalenty.  D�le byl je�t� p�id�n fon�m pauzy a fon�m pro n�razov� p�echod
mezi samohl�skami (nap��klad ve slov� "neefektivn�" mezi dv�ma `e').

K�vlastnostem fon�m� byla p�id�na d�lka samohl�sek dle anglick�ho vzoru.  R�zn�
fon�my mohou m�t zcela shodn� vlastnosti, nebylo snahou u�init fon�my sv�mi
vlastnostmi unik�tn�.

** Difony

UniSyn synt�za Festivalu pracuje nejl�pe s�difony, jin� p��stupy by byly
komplikovan�.  Proto je ��douc�, aby relace segment� byla sestavena ji�
s�ohledem na budouc� p�ry fon�m� odpov�daj�c� stanoven� mno�in� difon�.  Ve
festival-czech tak skute�n� �in�me.

Kompletn� mno�ina difon� festival-czech se skl�d� z�n�sleduj�c�ch skupin:

- Spojen� samohl�ska-souhl�ska: a-b a-c a-ch a-c~ a-d a-d~ a-dz a-dz~ a-f a-g
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

- Spojen� souhl�ska-samohl�ska: b-a c-a ch-a c~-a d-a d~-a dz-a dz~-a f-a g-a
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

- Spojen� souhl�ska-neutr�ln� fon�m a neutr�ln� fon�m-souhl�ska, tato spojen�
  se t�kaj� jenom vybran�ch souhl�sek, mezi n� se neutr�ln� fon�m vkl�d�: b-0
  d-0 d~-0 g-0 j-0 k-0 l-0 m-0 n-0 n~-0 p-0 r-0 t-0 t~-0 0-b 0-d 0-d~ 0-g 0-j
  0-k 0-l 0-m 0-n 0-n~ 0-p 0-r 0-t 0-t~

- Spojen� souhl�ska-souhl�ska, nejsou zde kombinace souhl�sek, mezi n� se
  vkl�d� neutr�ln� fon�m: b-c b-ch b-c~ b-dz b-dz~ b-f b-h b-l b-r~ b-s~ b-v
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

- Spojen� pauza-hl�ska a hl�ska-pauza: #-a #-au #-e #-eu #-i #-i: #-o #-ou #-u
  #-b #-c #-ch #-c~ #-d #-d~ #-dz #-dz~ #-f #-g #-h #-j #-k #-l #-m #-n #-n~
  #-p #-r #-r~ #-s #-s~ #-t #-t~ #-v #-z #-z~ a-# au-# e-# eu-# i-# i:-# o-#
  ou-# u-# b-# c-# ch-# c~-# d-# d~-# dz-# dz~-# f-# g-# h-# j-# k-# l-# m-#
  n-# n~-# p-# r-# r~-# s-# s~-# t-# t~-# v-# z-# z~-#

- Spojen� dvou shodn�ch souhl�sek, toto spojen� reprezentuje st�edovou
  samohl�sku, nikoliv prav� difon: a-a e-e i-i i:-i: o-o u-u

- Spojen� samohl�ska-dvojhl�ska a dvojhl�ska-samohl�ska, jedn� se o�podobn�
  trik jako v�p�edchoz� skupin�, tak�e se napojuj� jen shodn� samohl�sky: a-au
  e-eu o-ou au-u eu-u ou-u

- Spojen� samohl�ska-samohl�ska, kter� nespadaj� do p�edchoz�ch dvou skupin,
  vylu�ujeme t� samohl�sky `i' a `i:', u�kter�ch v�takov�ch kombinac�ch
  dopl�ujeme `j': a-e a-o e-a e-o o-a o-e u-a u-e u-o

Kompletn� seznam zde definovan�ch �esk�ch difon� je v�souboru czech-diphones.

Po difonov� datab�zi se nepo�aduje, aby obsahovala kompletn� seznam difon�.
�ada difon� ze skupiny souhl�ska-souhl�ska se v��e�tin� m��e vyskytnout jenom
na hranic�ch slov, kde je lze nahradit skupinou souhl�ska-#-souhl�ska, kde # je
velmi kr�tk� pauza.  N�kter� dal�� difony z�t�to skupiny jsou si natolik
podobn�, �e je lze reprezentovat jedin�m difonem, typicky n�kter� kombinace
zn�l�ch a nezn�l�ch souhl�sek.  Ka�dop�dn� plat�, �e ��m v�ce difon�, t�m m�n�
obezli�ek a n�sledn�ch ztr�t kvality; nedojde-li ov�em p�i v�t��m mno�stv�
difon� ke ztr�t� kvality vlivem �navy mluv��ho.

** P�id�v�n� segment�

V�lexikonu a LTS pravidlech se neprov�d� zdvojov�n� souhl�sek pro generov�n�
samohl�skov�ch fon�m� ani vkl�d�n� neutr�ln� hl�sky.  Jedn� se toti� o�intern�
trik, kter�m by tv�rci lexik�ln�ho analyz�toru nem�li b�t obt�ov�ni.  Vlo�en�
p��slu�n�ch segment� se prov�d� automaticky v�after_analysis_hooks.

** Slabiky

Ve festival-czech je za slabiku pova�ov�no cel� mluven� slovo.  �e�� se tak,
pon�kud o�kliv�m trikem, p��padn� rozlo�en� psan�ch slov na v�ce mluven�ch slov
(viz nap��klad zkratka `km/h' nebo ��slovky).  T�m p�dem slova relace Word
odpov�daj� slov�m psan� formy, zat�mco slabiky relace Syllable odpov�daj�
slov�m skute�n�m, jak je zapot�eb� je m�t p�i ur�ov�n� prozodie.

Skute�n� rozklad slov na slabiky se neprov�d�, proto�e je v��e�tin� obt��n�
ur�iteln�.  P�i zkoum�n� slabik je relevantn� pouze jejich po�et, kter� je d�n
v�skytem samohl�sek a slabikotvorn�ch souhl�sek.  Na to nen� rozklad na slabiky
zapot�eb�.


* Prozodie

Prozodie ve festival-czech zahrnuje intonaci, d�lku, pauzy a p��zvuk.  Tyto
parametry jsou konstruov�ny na z�klad� pravidel publikovan�ch v�[palkov�:04].

V�tn� �seky a p��zvukov� takty jsou konstruov�ny ve f�zi Word, jako zvl�tn�
relace IntUnit a StressUnit.

** Intonace

Pravidla pro intonaci byla p�evzata z�[palkov�:04].  Konkr�tn� intona�n�
tabulka kadenc� byla p�evzata z�[palkov�-pt��ek:97].

V�publikovan�ch prozodick�ch pravidlech jsou ur�it� nejasnosti, kter� byly
roz�e�eny n�sleduj�c�m zp�sobem:

- Pro �esti a v�ceslabi�n� p��zvukov� takty se v�pozici F pou��v� pravidlo ze
  skupiny A (v�[palkov�-pt��ek:97] p��slu�n� pravidlo pro pozici F chyb�).

- Pro pozici F se n�hodn� vyb�r� ze skupin A, B (v�[palkov�:04] nen�
  specifikov�no).

Nedo�e�ena je detekce dopl�kov�ch t�zac�ch v�t ve funkci czech-yes-no-question,
kde chyb� dostate�n� obs�hl� seznam t�zac�ch z�jmen, ��slovek a p��slovc�.
  
** D�lka

D�lka se stanovuje na z�klad� po�tu slabik v�p��zvukov�m taktu dle tabulky
z�[palkov�:04].

** Pauzy

Pou��v�me pauzy t�� d�lek: nejdel�� (BB), st�edn� (B) a kr�tk� (SB).  Dlouh�
pauza se vkl�d� na konc�ch v�t, st�edn� pauza mezi souv�t� a v�seznamech slov a
kr�tk� pauza p�ed spojky bez ��rky.  Detekce souv�t� a seznamu slov v�m�stech,
kde se nenach�z� ��rka, je prov�d�no trivi�ln� (a�samoz�ejm� v�mnoha p��padech
chybuj�c�) heuristikou.  Umis�ov�n� t�chto pauz se definuje v�prom�nn�
czech-phrase-cart-tree.

** P��zvuk

Dle [palkov�:04], sekce 1.2db), nen� v�synt�ze �e�tiny ��douc� p��zvuk
explicitn� generovat zm�nou dynamiky.  P��zvuk je modelov�n zm�nami intonace.


* Odkazy

[palkov�:04]
  Zdena Palkov�: Soubor fonetick�ch pravidel jako podklad pro prozodick�
  komponent automatick� synt�zy TTS v��e�tin�
  <<doplnit p�esnou citaci>>

[palkov�-pt��ek:97]
  Zdena Palkov�, Miroslav Pt��ek: Modelling Prosody in TTS Diphone Synthesis in
  Czech; Forum Phoneticum 63, Frankfurt am Main 1997

[palkov�-pt��ek:94]
  Zdena Palkov�, Miroslav Pt��ek: Ein Beitrag zur Intonation in der
  Diphonsynthese; Phonetica Pragensia VIII, Univerzita Karlova Praha 1994

[hor�k:02]
  Petr Hor�k: Modelov�n� suprasegment�ln�ch rys� mluven� �e�tiny pomoc�
  line�rn� predikce; dizerta�n� pr�ce; �VUT, Fakulta elektrotechnick�,
  Praha�2002


-- Milan Zamazal <pdm@freebsoft.org>