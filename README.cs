festival-czech -- podpora èe¹tiny pro Festival
==============================================

Cílem festival-czech je poskytnout kvalitní kompletní svobodnou èeskou øeèovou
syntézu.  Je vyu¾ito systému Festival, který nabízí solidní svobodný framework
pro tvorbu øeèových syntéz a disponuje kvalitní svobodnou anglickou øeèovou
syntézou.

Øeèová syntéza je dùle¾itým prvkem svobodných operaèních systémù.  Kromì jiného
tvoøí zásadní komponenty u¾ivatelského rozhraní pro zrakovì posti¾ené
u¾ivatele.  Souèasný stav svobodné èeské øeèové syntézy je neuspokojivý,
neexistuje ¾ádný dostateènì kvalitní kompletní systém.  Proto byla zahájena
práce na festival-czech, kde je snahou dosáhnout s vyu¾itím solidní festivalové
infrastruktury zaplnìní této mezery.  Projekt té¾ mù¾e v budoucnu slou¾it jako
základ a zdroj nezbytného festivalového know-how pro dal¹í jazyky a pøispìt tak
k vytvoøení vícejazyèného svobodného øeèového syntetizéru.

festival-czech øe¹í dvì úlohy:

1. Pøevod textu do fonetického popisu, nezávislý na modulu provádìjícím
   koneènou syntézu zvuku.

2. Difonovou databázi pro festivalový syntetizér.

Struèný návod k pou¾ití naleznete v souboru INSTALL.cs.

Soubor czech-words je seznam èeských slov v základních tvarech.  Soubor
czech-words-all je (prozatím nekompletní) seznam èeských slov vèetnì jejich
skloòovaných a èasovaných tvarù.  Seznamy slov byly poøízeny z dat a s pomocí
nástrojù velmi kvalitní ispellové èe¹tiny distribuované na
ftp://ftp.vslib.cz/pub/unix/ispell/czech pod GPL.

Momentální stav festival-czech je experimentální.  Pøevod textu do fonetického
popisu má zatím vytvoøeny pouze technické základy a musí být naplnìn jazykovými
pravidly a odladìn.  V oblasti difonové databáze se pracuje na postupech
zpracování namluvených vzorkù, hotová databáze zatím není k dispozici.
Výsledek projektu závisí na dostupných zdrojích, na jednu stranu má potenciál
dosáhnout pomìrnì slu¹ného výsledku, na druhou stranu v¹ak mù¾e zajít na úbytì.
Proto je vítána ka¾dá pomoc -- jazykovìdná, programátorská, finanèní nebo
jakákoliv jiná.

S pøípadnými dotazy, námìty a pomocí se lze obracet na moji e-mailovou adresu.

-- Milan Zamazal <pdm@freebsoft.org>
