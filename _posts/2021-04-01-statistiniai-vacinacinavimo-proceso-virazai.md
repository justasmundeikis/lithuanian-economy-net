---
title: Statistiniai vakcinavimo proceso viražai
date: 2021-04-01 08:00:00 +03:00
author: Justas Mundeikis
layout: post
comments: true
citation: true
image:  /assets/2021/04/01/ban.png
thumbnail: /assets/2021/04/01/thumb.ban.png
categories:
  - COVID19
tags:
  - statistika
---

**Šiame blogpost'e apie mistines sukauptas ir šaldytuvuose laikomas vakcinas ir Statistikos departamento klaidinantį duomenų valdymo procesą.**<!--more-->

# Intro

Vakar įvyko Seimo Sveikatos reikalų komiteto posėdis, kuriame SAM atsakinėjo į Seimo narių bei soc. partnerių klausimus, aptarinėjo aktualiausias vakcinavimo, žaliojo paso ir t.t. naujienas.

Delfi cituoja po posėdžio pasisakiusius Seimo narius [delfi.lt](https://www.delfi.lt/news/daily/lithuania/zemaitaitis-sokiruojantis-dalykas-yra-susijes-su-zaliuoju-pasu.d?id=86836351). Man užkliuvo buvusio SAM ministro A.Verygos pasisakymas:

>Buvęs sveikatos apsaugos ministras Aurelijus Veryga patikino, kad šiuo metu reikėtų lankstesnių sprendimų, nes **„vakcina šaldytuve imuniteto niekam nesukurs“**.

Tad kilo natūralūs klausimas - o tai kiek šaldytuvuose guli tų vakcinų? Turime rimtą problemą ar politinį pasibadymą? Ir pasirodo atsakyti į šį klausimą pasidarė ne taip paprasta, kaip aš tikėjausi.

Vakar atsidarius Statistikos departamento [COVID19 švieslentę](https://experience.arcgis.com/experience/cab84dcfe0464c2a8050a78f817924ca/page/page_3/) buvo galima išvysti tokią statistiką:

![](/assets/2021/04/01/screen_1.png)

Matome, jog:

* už laikotarpį iki 2021-03-30
* į Lietuvą pristatyta buvo 642 365 vakcinos dozių
* iš jų 565 765 paskirstyta į vakcinavimo centrus (arba 88% gautų vakcinų)
* o panauda skiepams tik 495 383 (77% gautų, 88% paskirstytų vakcinų)
* visgi **"šaldytuvuose"** laikoma **146 982** vakcinų dozių (642 365 - 495 383)

Jeigu tiek vakcinų iš tiesų laikoma šaldytuvuose, tada suprantama A.Veryga būtų teisus ir šią problemą reiktų spręsti ir kuo skubiau.

# Statistikos departamento nesuvaldomi procesai

Visgi "šaldytuvų" esama dviejų tipo. Vakcinos gali gulėti "ministerijos šaldytuvuose", tai tos vakcinos, kurios yra gautos, bet dar nepaskirstytos ir vakcinos gali gulėti "vakcinavimo centrų šaldytuvuose", tai tos vakcinos, kurios yra paskirstytos į vakcinavimo centrus, bet dar nepanaudotos skiepijant.

Deja Statistikos departamentas (su visa pagarba po truputi jų atveriamiems duomenims), dar nėra atvėręs pakankamai duomenų. Tad tako rankutėmis suskaičiuoti duomenis, esančius stulpelinėse diagramose "Gauta vakcinų dozių" ir "Paskirstyta vakcinų dozių". Ir o šokas, **SKAIČIAI NEATITINKA**!!!

Suskaičiavus rankutėmis "Paskirstyta vakcinų dozių" suma atitinka nurodytas, o  "Gauta vakcinų dozių" ne. "Gauta vakcinų dozių" iš stulpelinės diagramos suma siekia 577 565, o ne 642 365! Taigi nepaskirstytų vakcinų skaičius siekia ne 76 600 (642 365-565 795), o tik 11 800 (577 567-565 795) dozių.

Parašiau el. laišką į Statistikos departamentą, dėl galimo neatitikimo / klaidos ir gavau šiandien ryte tokį atsakymą:

>...nėra jokios paslapties, panikos, dramos ar klaidos (bent jau aš to nevadinčiau klaida). Šįryt SAM keliom minutėm anksčiau nei reikėjo į sistemą suvedė naujų ką tik atvykusių vakcinų atvykimą į sandėlį. Kadangi suvedė prieš mums išpublikuojant vakarykščius duomenis – taip ir gavosi, kad atrodo, jog vakcinos atvyko jau vakar ryte, o ne šiandien ryte. SAM tiesiog turėjo mažiau uoliai suvedinėti skaičius. Jei būtų suvedę 09:30 – apie šias naujas dozes, atvykusias šįryt, švieslentė paskelbtų tinkamu metu - rytoj ryte.

Kitaip tariant, SAM suvedinėja suminius skaičius, o Statistikos departamentas, duomenis stulpinėse diagramose. Tai jog publikuojami neatitinkantys skaičiai - mehhh... anokia čia problema...  Rimtai????

Mieli statistikai, taip mes Jus labai mėgstame, BET, Statistikos departamentas yra padarytas COVID duomenų "Gate keeper'iu", po to, kai NVSC suskaičiavo/nesuskaičiavo ir t.t. Būkite malonūs, susitvarkykite savo duomenų valdymo procesus ir užtikrinkite duomenų integralumą. Negali būti jog tame pačiame puslapyje esančiuose dviejuose laukeliuose skiriasi duomenys. Ir niekam nesvarbu, SAM paskubėjo ar ne. Jūs esate atsakingi už duomenų kokybę. Kitaip Jūs prisidedate prie to, jog būtų pradėta manipuliuoti skaičių neatitikimais.

O dabar atgal prie temos...

# Vakcinos "šaldytuvuose"

Šiandien skaičiau jau sutvarkyti ir galime pasižiūrėti į bendrą vaizdą.

Pirmame grafike nurodomi akumuliuoti gautų (akum_gauta_viso), akumuliuoti paskirstytų vakcinų (akum_paskirstyta_viso) ir akumuliuoti skiepams panaudotų vakcinų (akum_vakcinuota_viso) įverčiai.
Labai gerai matosi, jog "ministerijos šaldytuvai" nėra pildomi ir gautos vakcinos yra nedelsiant paskirstomos į vakcinavimo centrus. Tai parodo ir (ministerijos_šaldytuve) linija. Visgi matome, jog vakcinavimas pačiuose vakcinavimo centruose atsilieka, ypač nuo vasario vidurio. Nors neryškiai, bet pastebimas vak_centrų_šaldytuvų rodiklio pakilimas.

![](/assets/2021/04/01/vac_img_1.png)

Siekiant nuodugniau pažiūrėt į vakcinacijos centruose esančių vakcinų likučius, padariau žemiau esantį grafiką. Jame labai gerai matomi vakcinų likučiai.

Pfizer BioNTech vakcinos yra gaunamos ir iškart pradedama jomis vakcinuoti, todėl likučiai sparčiai krenta (žalia linija), su Moderna situacija panaši -  užsilaiko ji keliomis dienomis ilgiau, bet galbūt tai labiau susiję su kitokiu vakcinavimo procesu? Bet rimtos problemos nematyti.

Problema akivaizdiai yra AstraZeneca vakcina. Vasario pradžioje matome, jog vakcinų likučiai liko pakilę, t.y. nebuvo panaudotos visos gautos vakcinos. Vasario viduryje, **naujos** gaunamos vakcinos tarsi ir buvo suvakcinuojamos, nes kiek pakildavo, tiek bemaž ir nusileisdavo, bet tas bazinis ~10 000 vakcinų likutis, kažkur vis dar tebebuvo laikomas. Daug klausimų kylą, kur ir kodėl jos liko nepanaudotos?

Kovo 10 SAM paskelbė stabdanti vakcinavimą AstraZeneca vakcina (juodas brūkšnys), kaip tik po naujos partijos vakcinų gavimo, nemalonus sutapimas. Visgi pradėjus vėl vakcinuoti, matome, kad AstrZeneca likučiai pradėjo sparčiai leistis, bet vis tik lieka pakibę ties 17 000 vakcinų.

![](/assets/2021/04/01/vac_img_2.png)

# Išvados

Turimi duomenys rodo, jog iš tiesų turime problemą su AstraZeneca panaudojimu. Deja nėra pakankamai **viešai prieinamų** duomenų, jog identifikuoti, kur šie likučiai susikaupę. Ar tai, kur jie susikaupę, koreliuoja su su kokiomis nors savivaldybių savybėmis - atsakyti neturint duomenų neįmanoma. Tačiau akivaizdu ir tai, jog Lietuvoje teks dėti milžiniškas pastangas žmonėms paaiškinti, jog ši vakcina yra saugi ir niekas neturėtų bijoti ja vakcinuotis.

Tačiau tuo pat metu turimi duomenys aiškiai parodo, kad mušti būgnais ir aiškinti apie SAM neveiklumą, bent jau šiuo metu, prielaidų taip pat nėra. Vakcinos iš ministerijos yra paskirstomos greitai, kelių dienų bėgyje, o atvykusios į vakcinavimo centrus, jos sparčiai sunaudojamos. Tikėtis, kad gautos vakcinos bus sunaudotos 100 proc. tą pačią dieną yra nerealu ir nereikia turėti fantastinių lūkesčių.


# Palinkėjimai

Palinkėjimas Statistikos departamentui - sureguliuoti duomenų pateikimo ir viešinimo procesus bei atverti kuo daugiau duomenų visuomenei.
Palinkėjimas SAM - pradėti visuotinius, bet deja, renginius Lietuvos miesteliuose ir kaimeliuose tam, jog šviesti visuomenę. Nes bendras nepasitikėjimas ir nenoras vakcinuoti panašu, jog bus didesnis nei tikėtasi.

Ach, ir mažiau klausyt ekonomistų :D
