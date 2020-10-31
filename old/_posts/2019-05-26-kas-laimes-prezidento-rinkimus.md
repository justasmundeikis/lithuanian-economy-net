---
id: 1992
title: Kas laimės Prezidento rinkimus?
date: 2019-05-26T17:16:04+03:00
author: Justas Mundeikis
layout: post
guid: http://lithuanian-economy.net/?p=1992
permalink: /2019/05/26/kas-laimes-prezidento-rinkimus/
classic-editor-remember:
  - block-editor
image: /wp-content/uploads/2019/05/winner.jpeg
categories:
  - Economics
---
<p>Padarykime prielaidą, jog šiandien tiek I.Šimonytė, tiek G.Nausėda surinks tiek pat balsų (tarpusavio santykyje), kaip ir per pirmą turą, t.y. nei vienas negaus balsų iš kitų partijų, arba jeigu gaus, tai nepakeis judviejų gautų balsų tarpusavio santykio.<br />Įvertinus aktyvumą iki 18 val, ir truputi paskaičiavus.... tiesa, dabartiniame skaičiavime trūksta aktyvumo rodiklių iš užsienyje esančių konsulatų.... tačiau gaunamas toks galimas rinkimų rezultatas:<!--more--></p>
<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align: right; font-weight: bold; color: white !important; background-color: #808080 !important;">NAUSĖDA</th>
<th style="text-align: right; font-weight: bold; color: white !important; background-color: #808080 !important;">ŠIMONYTĖ</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: right; font-weight: bold; color: black !important; background-color: #d3d3d3 !important;">49.6</td>
<td style="text-align: right; font-weight: bold; color: black !important; background-color: #d3d3d3 !important;">50.4</td>
</tr>
</tbody>
</table>
<p>Žemiau: R kodas naudojamas skaičiavimui</p>

<!-- wp:code -->
<pre class="wp-block-code"><code>#*******************************************************************************
# Paketų instaliavimas ir loadinimas ####
#*******************************************************************************
# apima dplyr, tidyr, ggplot...
if(!require(tidyverse)) install.packages("tidyverse"); 
suppressMessages(require(tidyverse))
if(!require(knitr)) install.packages("knitr")
suppressMessages(require(knitr))

#*******************************************************************************
# Duomenų iš VRK parsiutnimas ####
#*******************************************************************************
url &lt;- "http://vrk.lt/statiniai/puslapiai/rinkimai/904/1/1546/rezultatai/VN_904_1_1546.csv" 
# guess_encoding(url)
R1 &lt;- read.csv(url, stringsAsFactors = FALSE, header = TRUE, sep="\t", fileEncoding = "UTF-16LE")

url &lt;- "http://vrk.lt/statiniai/puslapiai/rinkimai/904/2/aktyvumas/AK_904_2.csv"
# guess_encoding(url)
R2 &lt;- read.delim(url, stringsAsFactors = FALSE, header = TRUE, dec=",")
                  
#*******************************************************************************
# Duomenų apdorojimas 
#*******************************************************************************
R1 &lt;- R1 %>% filter(PAVARDE %in% c("NAUSĖDA", "ŠIMONYTĖ"))%>%
    select(RPL_UNIKALUS_NUMERIS, PAVARDE, BALSU_VISO)%>%
    spread(PAVARDE, BALSU_VISO)%>%
    rename(R1_NAUSĖDA=NAUSĖDA,
           R1_ŠIMONYTĖ=ŠIMONYTĖ)

R2 &lt;- data.frame(RPL_UNIKALUS_NUMERIS=R2$RPL_UNIKALUS_NUMERIS,
                 
                    R2_BALSU_SK=R2$is.viso,
                    R2_svoris=(R2$is.viso/R2$RINKEJU_SKAICIUS))%>%
    full_join(., R1, by="RPL_UNIKALUS_NUMERIS")%>% 
    mutate(R2_NAUS=R2_svoris * R1_NAUSĖDA,
           R2_ŠIMO=R2_svoris * R1_ŠIMONYTĖ)%>%
    na.omit()

#*******************************************************************************
# Rezultatai
#*******************************************************************************
BALSAI &lt;- data.frame(NAUSĖDA=round(sum(R2[,6])/sum(R2[,6:7])*100,1),
                     ŠIMONYTĖ=round(sum(R2[,7])/sum(R2[,6:7])*100,1))
kable(BALSAI)
</code></pre>
<!-- /wp:code -->