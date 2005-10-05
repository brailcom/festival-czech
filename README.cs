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

2. Difonovou databázi pro festivalový syntetizér.  Tato èást se nachází
   samostatnì v podprojektu voice-czech-ph.

Struèný návod k pou¾ití naleznete v souboru INSTALL.cs.

festival-czech je nyní ve stavu rozpracovanosti.  Máme funkèní jazykový modul
pro èe¹tinu a funkèní èeský difonový hlas.  Jazykový modul se skládá z nìkolika
èástí: fonémová a difonová sada (z vìt¹í èásti hotovo), základní pravidla pro
pøevod psaného textu do hláskové podoby (ta musí být je¹tì doplnìna a ladìna),
výslovnostní slovník (jeho obsah je zatím zcela minimální), minimální jazyková
analýza (¹iroké pole pro budoucí vylep¹ování interpretace psaného textu),
prozodická pravidla (ta jsou víceménì kompletní, ale mohou v nich být je¹tì
chyby).  Difonový hlas, vytvoøený v rámci podprojektu voice-czech-ph, je nyní
v provozuschopném stavu, ale zatím neprobìhlo ladìní kvality jeho výstupu.

Výsledek projektu závisí na dostupných zdrojích, na jednu stranu má potenciál
dosáhnout pomìrnì slu¹ného výsledku, na druhou stranu v¹ak mù¾e zajít na úbytì.
Proto je vítána ka¾dá pomoc -- jazykovìdná, programátorská, finanèní nebo
jakákoliv jiná.

Projekt èeské festivalové syntézy je realizován spoleèností Brailcom, o.p.s.
Finanènì na nìj pøispìly Nadaèní fond Èeského rozhlasu, spoleènost Seznam.cz a
Evropská komise (v rámci programu Leonardo da Vinci).
Odbornou pomoc poskytl Fonetický ústav Filozofické fakulty Univerzity Karlovy
v èele s prof. PhDr. Zdenou Palkovou, CSc.

S pøípadnými dotazy, námìty a nabídkami pomoci se lze obracet na adresu
festival-czech@lists.freebsoft.org .

-- Milan Zamazal
