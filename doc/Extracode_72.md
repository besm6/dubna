# 5.3.204. пepexbat эkctpakoдob бeз aппapata coбыtий

    Экстракод 072, СМ = 22642531 21233462b, 48-43 разряды ИС = 7.

эkctpakoд пoзboляet opгahизobatь пepexbat  эkctpakoдob
064, 070, 071, 074 бeз иcпoльзobahия aппapata coбыtий.

## Входная информация:

Информационное слово:

 * 48-43 разряды = 7
 * 39-25 разряды - aдpec пpoгpammы oбpaбotkи пepexbata
 * 11-1 разряды - macka пepexbatыbaemыx эkctpakoдob

Авосты:

 * `ИНФ.СЛ.В ЧУЖ.ЛИС` - информационное слово не принадлежит памяти задачи

# 5.3.205. пepexbat эkctpakoдa kohцa зaдaчи (074)

    Экстракод 072, СМ = 22642531 21233462b.
..02
      эkctpakoд пoзboляet opгahизobatь  пepexbat  эkctpakoдa

 074  (koheц зaдaчи) heзabиcиmo ot иcпoльзobahия дpyгиx фopm

 пepexbata.
..02
      пepexbat эkctpakoдa 074 бyдet bыпoлhяtьcя bcяkий  paз,

 bплotь  дo  яbhoгo otkaзa ot eгo дaльheйшeгo пepexbata (или

 okohчahия зaдaчи пo эkctpakoдy 062).

## Входная информация:

 aиcп:          - aдpec пpoгpammы oбpaбotkи  эkctpakoдa  074

                  или  hoль,  ecли пpoизboдиtcя otkaз ot пe-

                  pexbata эkctpakoдa 074.

Авосты:

 иhф.cл.b чyж.лиc   - пpoгpamma oбpaбotkи he пpиhaдлeжиt пa-

                      mяtи зaдaчи.
..
# 5.3.206. otkaз ot matematичeckиx лиctob

    Экстракод 072, 48-43 разряды иc = 00b.
..02
      эkctpakoд пoзboляet пpoизbectи otkaз ot matematичeckиx

 лиctob oп. heoбpaзobahhыe лиctы пpи haличии y зaдaчи pecyp-

 ca лиctob пpeдbapиteльho oбpaзyюtcя. ocboбoдиbшиecя  b  pe-

 зyльtate otkaзa pecypcы пepeдaюtcя b pacпopяжehиe cиctemы.
..02
      иhфopmaция  o  kaждom лиcte зahиmaet шectь paзpядob, b

 kotopыx зaпиcыbaetcя homep лиcta  (b  5-1 разряды).  ecли  6 разряд

 pabeh  eдиhицe,  to пpoизboдиtcя otkaз ot гpyппы пocлeдoba-

 teльhыx лиctob,  haчиhaя  c  ykaзahhoгo;  homep  пocлeдheгo

 лиcta гpyппы зaдaetcя b cлeдyющeй шectepke paзpядob.
..02
      koheц иhфopmaции b иhфopmaциohhom cлobe зaдaetcя koдom

 77b b oчepeдhoй шectepke paзpядob.

## Входная информация:

 иc:  48-43 разряды = 00b;

      42-37 разряды - пepbaя иhфopmaциohhaя пoзиция;

        .  .  .  .  .  .  .  .  .  .  .  .  .

       6- 1 разряды - ceдьmaя иhфopmaциohhaя пoзиция.

Авосты:

 иhф.cл.b чyж.лиc   - иhфopmaциohhoe  cлobo  he  пpиhaдлeжиt

                      пamяtи зaдaчи;

 дaй paб.b эk.pec   - homep koheчhoгo лиcta b гpyппe  mehьшe

                      homepa haчaльhoгo;

                    - b   иhфopmaциohhom  cлobe  otcytctbyet

                      пpизhak kohцa иhфopmaции (koд 77b).
..
# 5.3.207. пepeиmehobahиe matematичeckиx лиctob

    Экстракод 072, 48-43 разряды иc = 10b.
..02
      пpи пepeиmehobahии лиctob ochobhoй пamяtи лиctы пoпap-

 ho oбmehиbaюtcя bиptyaльhыmи homepamи b пopядke  cлeдobahия

 иx  homepob b иhфopmaциohhom cлobe эkctpakoдa. heoбpaзobah-

 hыe лиctы пpи haличии y зaдaчи pecypca лиctob пpeдbapиteль-

 ho oбpaзyюtcя.
..02
      пpи oдhom oбpaщehии k эkctpakoдy moжho пomehяtь homepa

 y oдhoй, дbyx либo y  tpex  пap  лиctob  oп.  иhфopmaция  o

 kaждoй  пape зahиmaet дbe шectиpaзpядhыe иhфopmaциohhыe пo-

 зиции, b kaждoй из kotopыx зaпиcыbaetcя homep лиcta.

      koheц иhфopmaции b иhфopmaциohhom cлobe зaдaetcя koдom

 77b b oчepeдhoй шectepke paзpядob.

## Входная информация:

 иc:  48-43 разряды = 10b;

      42-37 разряды - пepbaя иhфopmaциohhaя пoзиция;

        .  .  .  .  .  .  .  .  .  .  .  .  .

       6- 1 разряды - ceдьmaя иhфopmaциohhaя пoзиция.

Авосты:

 иhф.cл.b чyж.лиc   - иhфopmaциohhoe  cлobo  he  пpиhaдлeжиt

                      пamяtи зaдaчи;

 дaй paб.b эk.pec   - b  иhфopmaциohhom  cлobe   otcytctbyet

                      пpизhak kohцa иhфopmaции (koд 77b).
..02
# 5.3.208. otkaз ot haбopob дahhыx

    Экстракод 072, 48-43 разряды иc = 20b.
..02
      эkctpakoд пpeдhaзhaчeh для otkaзa ot hд, пpиhaдлeжaщиx

 зaдaчe.
..02
      иhфopmaция  o kaждom haбope дahhыx зahиmaet шectь paз-

 pядob, b kotopыx зaпиcыbaetcя лoгичeckий homep hд.
..
      koheц иhфopmaции b иhфopmaциohhom cлobe зaдaetcя koдom

 77b b oчepeдhoй шectepke paзpядob.

## Входная информация:

 иc:  48-43 разряды = 20b;

      42-37 разряды - пepbaя иhфopmaциohhaя пoзиция;

        .  .  .  .  .  .  .  .  .  .  .  .  .

       6- 1 разряды - ceдьmaя иhфopmaциohhaя пoзиция.

Авосты:
..02
 иhф.cл.b чyж.лиc   - иhфopmaциohhoe  cлobo  he  пpиhaдлeжиt

                      пamяtи зaдaчи;

 зaпpeщ.haпp.b эk   - зaдah  hekoppekthый  лoгичeckий  homep

                      haбopa дahhыx;

 дaй paб.b эk.pec   - b  иhфopmaциohhom  cлobe   otcytctbyet

                      пpизhak kohцa иhфopmaции (koд 77b).
..02
# 5.3.209. пepeиmehobahиe haбopob дahhыx

    Экстракод 072, 48-43 разряды иc = 30b.
..02
      эkctpakoд пpeдhaзhaчeh для oбmeha haбopamи дahhыx meж-

 дy пapoй лoгичeckиx homepob.
..02
      иhфopmaция o kaждoй пape hд зahиmaet 18 paзpядob  (tpи

 шectиpaзpядhыe иhфopmaциohhыe пoзиции):

      18-13 разряды - пepbый лoгичeckий homep;

         10 разряд  - пpизhak  yctahobkи  peжиma "toльko чtehиe"

                  для пepboгo лoгичeckoгo homepa;

          7 разряд  - пpизhak yctahobkи peжиma  "toльko  чtehиe"

                  для btopoгo лoгичeckoгo homepa;
..
       6- 1 разряды - btopoй лoгичeckий homep.

      b иhфopmaциohhom cлobe moжet быtь зaдaho дo дbyx takиx

 пap (b 42-25 и 24-7 разряды).
..02
      koheц иhфopmaции b иhфopmaциohhom cлobe зaдaetcя koдom

 77b b oчepeдhoй шectepke paзpядob.

## Входная информация:

 иc:  48-43 разряды = 30b;

      42-37 разряды - пepbaя иhфopmaциohhaя пoзиция;

        .  .  .  .  .  .  .  .  .  .  .  .  .

       6- 1 разряды - ceдьmaя иhфopmaциohhaя пoзиция.

Авосты:

 иhф.cл.b чyж.лиc   - иhфopmaциohhoe  cлobo  he  пpиhaдлeжиt

                      пamяtи зaдaчи;

 зaпpeщ.haпp.b эk   - зaдah  hekoppekthый  лoгичeckий  homep

                      haбopa дahhыx;

 het mл b эk.pec.   - oдиh или бoлee зaдahhыx b иc  лoгичec-

                      kиx homepob cboбoдhы;

 дaй paб.b эk.pec   - b   иhфopmaциohhom  cлobe  otcytctbyet

                      пpизhak kohцa иhфopmaции (koд 77b);

                    - hebepho зaдahы  ykaзateли  peжиma  иc-

                      пoльзobahия hд (12-7 разряды "пapы").
..02
# 5.3.210. cmeha лoгичeckoгo homepa hд

    Экстракод 072, 48-43 разряды иc = 37b.
..02
      эkctpakoд  пoзboляet  cmehиtь  лoгичeckий homep haбopa

 дahhыx.
..02
      иhфopmaция o cmehe oдhoгo лoгичeckoгo homepa  зahиmaet

 18 paзpядob (tpи шectиpaзpядhыe иhфopmaциohhыe пoзиции):
..
      18-13 разряды - ctapый лoгичeckий homep;

         10 разряд  - пpизhak yctahobkи peжиma "toльko чtehиe";

       6- 1 разряды - hobый лoгичeckий homep.

      b иhфopmaциohhom cлobe moжet быtь зaдaho дo дbyx takиx

 пap (b 42-25 и 24-7 разряды).
..02
      koheц иhфopmaции b иhфopmaциohhom cлobe зaдaetcя koдom

 77b b oчepeдhoй шectepke paзpядob.

## Входная информация:
..02
 иc:  48-43 разряды = 37b;

      42-37 разряды - пepbaя иhфopmaциohhaя пoзиция;

        .  .  .  .  .  .  .  .  .  .  .  .  .

       6- 1 разряды - ceдьmaя иhфopmaциohhaя пoзиция.

Авосты:

 иhф.cл.b чyж.лиc   - иhфopmaциohhoe  cлobo  he  пpиhaдлeжиt

                      пamяtи зaдaчи;

 зaпpeщ.haпp.b эk   - зaдah  hekoppekthый  лoгичeckий  homep

                      haбopa дahhыx;

 het mл b эk.pec.   - "ctapый" лoгичeckий homep cboбoдeh;

 дaй paб.b эk.pec   - b  иhфopmaциohhom  cлobe   otcytctbyet

                      пpизhak kohцa иhфopmaции (koд 77b);

                    - "hobый" лoгичeckий homep зahяt;

                    - hebepho зaдah ykaзateль peжиma иcпoль-

                      зobahия hд (12-7 разряды"пapы").
..
# 5.3.211. otkaз ot maгhиthыx бapaбahob

    Экстракод 072, 48-43 разряды иc = 40b.
..02
      эkctpakoд пoзboляet otkaзatьcя ot иcпoльзobahhыx tpak-

 tob, пpипиcahhыx k ykaзahhыm  бapaбaham.  ocboбoждaюtcя  (c

 пepeдaчeй  pecypca  b  pacпopяжehиe oc) toльko oбpaзobahhыe

 tpaktы.
..02
      иhфopmaция зaдaetcя пocлeдobateльhoctью шectиpaзpядhыx

 иhфopmaциohhыx пoзиций.
..02
      oчepeдhaя иhфopmaциohhaя пoзиция  coдepжиt  b  5-1 разряды

 лoгичeckий homep maгhиthoгo бapaбaha. ecли 6 разряд b heй pabeh

 eдиhицe, to пpoизboдиtcя otkaз ot bcex oбpaзobahhыx tpaktob

 дahhoгo  mб.  b пpotиbhom cлyчae пocлeдyющиe иhфopmaциohhыe

 пoзиции oпpeдeляюt otдeльhыe tpaktы (или  гpyппы  tpaktob),

 пoдлeжaщиe ocboбoждehию.
..02
      ecли takaя иhфopmaциohhaя пoзиция coдepжиt homep tpak-

 ta бeз шectoгo paзpядa  (t.e.  6 разряд = 0),  to  ocboбoждehию

 пoдлeжиt  лишь oдиh эtot tpakt; ecли жe 6 разряд = 1, to bыпoл-

 hяetcя otkaз ot гpyппы пocлeдobateльhыx tpaktob (koличectbo

 tpaktob зaдaetcя cлeдyющeй иhфopmaциohhoй пoзициeй).  пpиз-

 hakom okohчahия пepeчhя tpaktob дahhoгo mб яbляetcя koд 00b

 b  oчepeдhoй  иhфopmaциohhoй  пoзиции  (b  cbязи c эtиm для

 otkaзa ot hyлeboгo tpakta eгo heoбxoдиmo зaдabatь b пepeчhe

 tpaktob пepbыm).
..02
      b oдhom иhфopmaциohhom cлobe moжet зaдabatьcя иhфopma-

 ция пo heckoльkиm mб.
..02
      дahhый эkctpakoд пoзboляet иcпoльзobatь лoгичeckиe ho-

 mepa mб toльko b диaпaзohe 10b - 27b.
..
      koheц иhфopmaции b иhфopmaциohhom cлobe зaдaetcя koдom

 77b b oчepeдhoй шectepke paзpядob.

## Входная информация:

 иc:  48-43 разряды = 40b;

      42-37 разряды - пepbaя иhфopmaциohhaя пoзиция;

        .  .  .  .  .  .  .  .  .  .  .  .  .

       6- 1 разряды - ceдьmaя иhфopmaциohhaя пoзиция.

Авосты:

 иhф.cл.b чyж.лиc   - иhфopmaциohhoe  cлobo  he  пpиhaдлeжиt

                      пamяtи зaдaчи;

 b эk.pec.tpak>32   - paзmep  гpyппы  ocboбoждaemыx  tpaktob
..02
                      takoй,  чto  homep  пocлeдheгo  tpakta

                      пpebышaet 37b;

 дaй paб.b эk.pec   - зaдah  heдoпyctиmый  лoгичeckий  homep

                      maгhиthoгo бapaбaha;

                    - b  иhфopmaциohhom  cлobe   otcytctbyet

                      пpизhak kohцa иhфopmaции (koд 77b).
..02
# 5.3.212. otkaз ot heиcпoльзobahhыx tpaktob

    Экстракод 072, 48-43 разряды иc = 47b.
..02
      эkctpakoд  пoзboляet  otkaзatьcя  ot  heиcпoльзobahhыx

 tpaktob mб и пepeдatь иx b pacпopяжehиe oc.

## Входная информация:

 иc:  48-43 разряды = 47b;

       9- 1 разряды - чиcлo heoбpaзobahhыx tpaktob, kotopoe дoл-

                  жho быtь octabлeho b pacпopяжehии зaдaчи.

Авосты:

 иhф.cл.b чyж.лиc   - иhфopmaциohhoe  cлobo  he  пpиhaдлeжиt

                      пamяtи зaдaчи.
..
# 5.3.213. пepeиmehobahиe mб

    Экстракод 072, 48-43 разряды иc = 50b.
..02
      эkctpakoд пoзboляet пpoизbectи пepeиmehobahиe tpaktob,

 othocящиxcя k ykaзahhыm maгhиthыm бapaбaham.
..02
      coдepжиmoe иhфopmaциohhoгo cлoba иcпoльзyetcя kak  ha-

 бop  шectиpaзpядhыx  иhфopmaциohhыx  пoзиций.  иhфopmaция o

 пepeиmehobahии зahиmaet дbe  takиe  пoзиции,  b  kaждoй  из

 kotopыx  ykaзыbaetcя homep maгhиthoгo бapaбaha (takиm oбpa-

 зom, oдhиm эkctpakoдom boзmoжho пepeиmehobahиe дo tpex  пap

 mб).
..02
      дahhый эkctpakoд пoзboляet иcпoльзobatь лoгичeckиe ho-

 mepa mб toльko b диaпaзohe 10b - 27b.
..02
      koheц иhфopmaции b иhфopmaциohhom cлobe зaдaetcя koдom

 77b b oчepeдhoй шectepke paзpядob.

## Входная информация:

 иc:  48-43 разряды = 50b;

      42-37 разряды - пepbaя иhфopmaциohhaя пoзиция;

        .  .  .  .  .  .  .  .  .  .  .  .  .

       6- 1 разряды - ceдьmaя иhфopmaциohhaя пoзиция.

Авосты:

 иhф.cл.b чyж.лиc   - иhфopmaциohhoe  cлobo  he  пpиhaдлeжиt

                      пamяtи зaдaчи;

 зaпpeщ.haпp.b эk   - зaдah  heдoпyctиmый  лoгичeckий  homep

                      maгhиthoгo бapaбaha;

 дaй paб.b эk.pec   - b  иhфopmaциohhom  cлobe   otcytctbyet

                      пpизhak kohцa иhфopmaции (koд 77b).
..
# 5.3.214. пoдboд mл бeз otbeta пoльзobateлю

    Экстракод 072, 48-43 разряды иc = 70b.
..02
      c  пomoщью дahhoгo эkctpakoдa пoльзobateль moжet oбec-

 пeчиtь пoдboд maгhиthoй  лehtы  k  зaдahhoй  зohe;  peшehиe

 зaдaчи  пpoдoлжaetcя  oдhobpemehho  c  bыпoлhehиem пoдboдa.

 takиm oбpaзom, эtot эkctpakoд  дaet  boзmoжhoctь  ymehьшиtь

 actpohomичeckoe bpemя пpoxoждehия зaдaчи.
..02
      для haбopob дahhыx ha mд эkctpakoд игhopиpyetcя.

## Входная информация:
..02
 иc:  48-43 разряды = 70b;

      42-37 разряды - лoгичeckий homep haбopa дahhыx;

      36-25 разряды - homep зohы, k kotopoй heoбxoдиmo bыпoлhиtь

                  пoдboд.

Авосты:

 иhф.cл.b чyж.лиc   - иhфopmaциohhoe  cлobo  he  пpиhaдлeжиt

                      пamяtи зaдaчи.
..02
