---
layout: page
title: 2021m. Biudžetas
permalink: /2021-budget/
datatable: true
---

# Posėdžiai

Šiame puslapyje pateikiami su 2021m. biudžetu susiję posėdžiai:

* Bendrą biudžetą - XIIIP-5302 ([dokumentai](https://e-seimas.lrs.lt/portal/legalAct/lt/TAP/597cb7100ee411ebbedbd456d2fb030d), [FinMin dokumentai](https://finmin.lrv.lt/lt/veiklos-sritys/biudzetas/biudzetu-projektai/issami-informacija-apie-2021-m-biudzeto-projekta))
* PSDF biudžetą - XIIIP-5292 ([dokumentai](https://e-seimas.lrs.lt/portal/legalAct/lt/TAP/2a4cbe300ea411ebbedbd456d2fb030d))
* Valstybės politikų, teisėjų, valstybės pareigūnų, valstybės tarnautojų ir valstybės ir savivaldybių biudžetinių įstaigų darbuotojų pareiginės algos (atlyginimo) bazinio dydžio, taikomo 2021 metais, įstatymo projektas- XIIIP-5299 ([dokumentai](https://e-seimas.lrs.lt/portal/legalAct/lt/TAP/753516300ecd11ebbedbd456d2fb030d))


<table>
  {% for row in site.data.budget_2021_time_table %}
    {% if forloop.first %}
    <tr>
      {% for pair in row %}
        <th>{{ pair[0] }}</th>
      {% endfor %}
    </tr>
    {% endif %}

    {% tablerow pair in row %}
      {{ pair[1] }}
    {% endtablerow %}
  {% endfor %}
</table>

# Susiję blogpost'ai

* [PSDF biudžeto analizė](http://lithuanian-economy.net/2020/10/15/PSDF-biudzeto-projekto-aptarimas)
