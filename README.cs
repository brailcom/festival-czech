festival-czech -- podpora �e�tiny pro Festival
==============================================

C�lem festival-czech je poskytnout kvalitn� kompletn� svobodnou �eskou �e�ovou
synt�zu.  Je vyu�ito syst�mu Festival, kter� nab�z� solidn� svobodn� framework
pro tvorbu �e�ov�ch synt�z a disponuje kvalitn� svobodnou anglickou �e�ovou
synt�zou.

�e�ov� synt�za je d�le�it�m prvkem svobodn�ch opera�n�ch syst�m�.  Krom� jin�ho
tvo�� z�sadn� komponenty u�ivatelsk�ho rozhran� pro zrakov� posti�en�
u�ivatele.  Sou�asn� stav svobodn� �esk� �e�ov� synt�zy je neuspokojiv�,
neexistuje ��dn� dostate�n� kvalitn� kompletn� syst�m.  Proto byla zah�jena
pr�ce na festival-czech, kde je snahou dos�hnout s�vyu�it�m solidn� festivalov�
infrastruktury zapln�n� t�to mezery.  Projekt t� m��e v�budoucnu slou�it jako
z�klad a zdroj nezbytn�ho festivalov�ho know-how pro dal�� jazyky a p�isp�t tak
k�vytvo�en� v�cejazy�n�ho svobodn�ho �e�ov�ho syntetiz�ru.

festival-czech �e�� dv� �lohy:

1. P�evod textu do fonetick�ho popisu, nez�visl� na modulu prov�d�j�c�m
   kone�nou synt�zu zvuku.

2. Difonovou datab�zi pro festivalov� syntetiz�r.  Tato ��st se nach�z�
   samostatn� v�podprojektu voice-czech-ph.

Stru�n� n�vod k�pou�it� naleznete v�souboru INSTALL.cs.

Soubor czech-words je seznam �esk�ch slov v�z�kladn�ch tvarech spolu s�pravidly
pro ispell.  Seznam slov byl po��zen z�dat velmi kvalitn� ispellov� �e�tiny
distribuovan� na ftp://ftp.vslib.cz/pub/unix/ispell/czech pod GPL (d�ky Petru
Kol��ovi za tuto �ctyhodnou aktivitu!).  Ze stejn�ho zdroje byl p�evzat soubor
`munch'.

festival-czech je nyn� ve stavu rozpracovanosti.  M�me funk�n� jazykov� modul
pro �e�tinu a funk�n� �esk� difonov� hlas.  Jazykov� modul se skl�d� z�n�kolika
��st�: fon�mov� a difonov� sada (z�v�t�� ��sti hotovo), z�kladn� pravidla pro
p�evod psan�ho textu do hl�skov� podoby (ta mus� b�t je�t� dopln�na a lad�na),
v�slovnostn� slovn�k (jeho obsah je zat�m zcela minim�ln�, je v�ak definov�n
framework pro jeho tvorbu), minim�ln� jazykov� anal�za (�irok� pole pro budouc�
vylep�ov�n� interpretace psan�ho textu), prozodick� pravidla (ta jsou v�cem�n�
kompletn�, ale mohou v�nich b�t je�t� chyby).  Difonov� hlas, vytvo�en� v�r�mci
podprojektu voice-czech-ph, je nyn� v�provozuschopn�m stavu, ale zat�m
neprob�hlo lad�n� kvality jeho v�stupu.

V�sledek projektu z�vis� na dostupn�ch zdroj�ch, na jednu stranu m� potenci�l
dos�hnout pom�rn� slu�n�ho v�sledku, na druhou stranu v�ak m��e zaj�t na �byt�.
Proto je v�t�na ka�d� pomoc -- jazykov�dn�, program�torsk�, finan�n� nebo
jak�koliv jin�.

Projekt �esk� festivalov� synt�zy je realizov�n spole�nost� Brailcom, o.p.s.
Finan�n� na n�j p�isp�l Nada�n� fond �esk�ho rozhlasu.  Odbornou pomoc poskytl
Fonetick� �stav Filozofick� fakulty Univerzity Karlovy v��ele
s�prof. PhDr. Zdenou Palkovou, CSc.

S�p��padn�mi dotazy, n�m�ty a nab�dkami pomoci se lze obracet na moji
e-mailovou adresu.

-- Milan Zamazal <pdm@freebsoft.org>
