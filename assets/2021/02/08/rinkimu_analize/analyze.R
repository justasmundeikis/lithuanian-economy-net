library(tidyverse)
library(knitr)

# https://www.vrk.lt/atviri-duomenys-2016-seimo
# https://www.vrk.lt/2020-seimo/rezultatai

dg2016 <- read.csv("2016_DG.csv", sep = ";", fileEncoding = "ISO-8859-13")
dg2020 <- read.csv("2020_DG.csv", sep="\t")
part_2020 <- read.csv("rink_2020_part.csv", sep = ";", fileEncoding = "ISO-8859-13")[,-1]
colnames(part_2020) <- c("APYGARDOS_NR","APYGARDOS_PAVADINIMAS","RPL_UNIKALUS_NUMERIS","APYLINKES_NR","APYLINKES_PAVADINIMAS","SAV","VISO_RINKEJU","VISO_BALSAVO","RINK_24_V","RINK_24_M","BALS_24_V","BALS_24_M","RINK_25_29_V","RINK_25_29_M","BALS_25_29_V","BALS_25_29_M","RINK_30_34_V","RINK_30_34_M","BALS_30_34_V","BALS_30_34_M","RINK_35_44_V","RINK_35_44_M","BALS_35_44_V","BALS_35_44_M","RINK_45_54_V","RINK_45_54_M","BALS_45_54_V","BALS_45_54_M","RINK_55_64_V","RINK_55_64_M","BALS_55_64_V","BALS_55_64_M","RINK_65_74_V","RINK_65_74_M","BALS_65_74_V","BALS_65_74_M","RINK_75_V","RINK_75_M","BALS_75_V","BALS_75_M")

xx <- dg2016%>%
  filter(SAV!="",
         SARASO_PAVADINIMAS %in% c("Lietuvos socialdemokratų partija",
                                   "Lietuvos socialdemokratų darbo partija",
                                   "Lietuvos Respublikos liberalų sąjūdis",
                                   "Laisvės partija"))%>%
  select(RPL_UNIKALUS_NUMERIS, APYLINKES_PAVADINIMAS,SAV, SARASO_PAVADINIMAS, BALSU_VISO)%>%
  mutate(SARASO_PAVADINIMAS=gsub("Lietuvos socialdemokratų partija", "LEFT",SARASO_PAVADINIMAS),
         SARASO_PAVADINIMAS=gsub("Lietuvos socialdemokratų darbo partija", "LEFT",SARASO_PAVADINIMAS),
         SARASO_PAVADINIMAS=gsub("Lietuvos Respublikos liberalų sąjūdis", "LIB",SARASO_PAVADINIMAS),
         SARASO_PAVADINIMAS=gsub("Laisvės partija", "LIB",SARASO_PAVADINIMAS))%>%
  mutate(APYLINKES_PAVADINIMAS=gsub("–","-",APYLINKES_PAVADINIMAS))%>%
  group_by(RPL_UNIKALUS_NUMERIS, APYLINKES_PAVADINIMAS, SAV, SARASO_PAVADINIMAS)%>%
  summarise_all(sum)%>%
  spread(SARASO_PAVADINIMAS,BALSU_VISO)%>%
  rename(LEFT_2016=LEFT,
         LIB_2016=LIB)



yy <- dg2020%>%
  filter(SAV!="",
         SARASO_PAVADINIMAS %in% c("Lietuvos socialdemokratų partija",
                                   "Lietuvos socialdemokratų darbo partija",
                                   "Lietuvos Respublikos liberalų sąjūdis",
                                   "Laisvės partija"))%>%
  select(RPL_UNIKALUS_NUMERIS, APYLINKES_PAVADINIMAS,SAV, SARASO_PAVADINIMAS, BALSU_VISO)%>%
  mutate(SARASO_PAVADINIMAS=gsub("Lietuvos socialdemokratų partija", "LEFT",SARASO_PAVADINIMAS),
         SARASO_PAVADINIMAS=gsub("Lietuvos socialdemokratų darbo partija", "LEFT",SARASO_PAVADINIMAS),
         SARASO_PAVADINIMAS=gsub("Lietuvos Respublikos liberalų sąjūdis", "LIB",SARASO_PAVADINIMAS),
         SARASO_PAVADINIMAS=gsub("Laisvės partija", "LIB",SARASO_PAVADINIMAS))%>%
  mutate(APYLINKES_PAVADINIMAS=gsub("–","-",APYLINKES_PAVADINIMAS))%>%
  group_by(RPL_UNIKALUS_NUMERIS, APYLINKES_PAVADINIMAS,SAV,SARASO_PAVADINIMAS)%>%
  summarise_all(sum)%>%
  spread(SARASO_PAVADINIMAS,BALSU_VISO)%>%
  rename(LEFT_2020=LEFT,
         LIB_2020=LIB)

aa <- dg2016%>%
  filter(SAV!="",
         SARASO_PAVADINIMAS %in% c("Lietuvos socialdemokratų partija"))%>%
  select(RPL_UNIKALUS_NUMERIS,VISO_DALYVAVO)%>%
  rename(VISO_DALYVAVO_2016=VISO_DALYVAVO)

bb <- dg2020%>%
  filter(SAV!="",
         SARASO_PAVADINIMAS %in% c("Lietuvos socialdemokratų partija"))%>%
  select(RPL_UNIKALUS_NUMERIS,VISO_DALYVAVO)%>%
  rename(VISO_DALYVAVO_2020=VISO_DALYVAVO)

################################################################################
#### Rezultatas rinkimų apylinkėse
################################################################################
mm <- full_join(xx,yy, by=c("RPL_UNIKALUS_NUMERIS",
                            "APYLINKES_PAVADINIMAS",
                            "SAV"))%>%
  ## isvalomos nepersidengencios apylinkes
  na.omit()%>%
  left_join(.,aa, by="RPL_UNIKALUS_NUMERIS")%>%
  left_join(.,bb, by="RPL_UNIKALUS_NUMERIS")%>%
  group_by(RPL_UNIKALUS_NUMERIS,APYLINKES_PAVADINIMAS, SAV)%>%
  summarise_all(sum, na.rm=TRUE)%>%
  mutate(LEFT_2016_p=LEFT_2016/VISO_DALYVAVO_2016*100,
         LIB_2016_p=LIB_2016/VISO_DALYVAVO_2016*100,
         LEFT_2020_p=LEFT_2020/VISO_DALYVAVO_2020*100,
         LIB_2020_p=LIB_2020/VISO_DALYVAVO_2020*100)%>%
  mutate(Skirstymas=ifelse(SAV%in% c("Vilniaus miesto savivaldybė",
                                       "Vilniaus rajono savivaldybė",
                                       "Kauno miesto savivaldybė",      
                                       "Kauno rajono savivaldybė",
                                       "Klaipėdos miesto savivaldybė",
                                       "Klaipėdos rajono savivaldybė",
                                       "Šiaulių miesto savivaldybė",
                                       "Šiaulių rajono savivaldybė" ), 
                           "Didmiestis",
                           "Ne"))


png("./rinkimu_balsu_santykis.png", width = 10, height = 6, units = 'in', res = 200)
ggplot(mm, aes(LEFT_2016_p,LEFT_2020_p, col=Skirstymas))+
  geom_point(aes(size = VISO_DALYVAVO_2020^2),alpha=0.5)+
  scale_size(name   = "Rinkeju sk.",
             breaks = fivenum(mm$VISO_DALYVAVO_2020)^2,
             labels = fivenum(mm$VISO_DALYVAVO_2020))+
  scale_shape(solid = F)+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5)+
  labs(title="LSDP (+LSDDP) rezultatas rinkimų apylinkėse 2016m. ir 2020m.",
       x="2016m. procentas",
       y="2020m. procentas")+
  theme(legend.position = "bottom")
dev.off()


################################################################################
#### Rezultatas savivaldybėse
################################################################################

nn <- mm %>%
  ungroup()%>%
  select(2:8)%>%
  group_by(SAV)%>%
  summarise_all(sum)%>%
  mutate(LEFT_2016_p=LEFT_2016/VISO_DALYVAVO_2016*100,
         LIB_2016_p=LIB_2016/VISO_DALYVAVO_2016*100,
         LEFT_2020_p=LEFT_2020/VISO_DALYVAVO_2020*100,
         LIB_2020_p=LIB_2020/VISO_DALYVAVO_2020*100)%>%
  mutate(Skirstymas=ifelse(SAV%in% c("Vilniaus miesto savivaldybė",
                                     "Vilniaus rajono savivaldybė",
                                     "Kauno miesto savivaldybė",      
                                     "Kauno rajono savivaldybė",
                                     "Klaipėdos miesto savivaldybė",
                                     "Klaipėdos rajono savivaldybė",
                                     "Šiaulių miesto savivaldybė",
                                     "Šiaulių rajono savivaldybė" ), 
                           "Didmiestis",
                           "Ne"))%>%
  mutate(LEFT_diff=LEFT_2020_p-LEFT_2016_p,
         LIB_diff=LIB_2020_p-LIB_2016_p)

png("./rinkimu_balsu_pokytis.png", width = 8, height = 10, units = 'in', res = 200)
ggplot(nn, aes(reorder(SAV, LEFT_diff),LEFT_diff, fill=Skirstymas))+
  geom_bar(stat="identity")+ 
  coord_flip()+
  labs(title="LSDP+LSDDP santykinis balsų pokytis",
       x="Savivaldybė",
       y="Skirtumas proc. punktai")+
  theme(legend.position = "")
dev.off()

################################################################################
#### Didmiesčiai vs ne didmiesčiai - balsų skaičius
################################################################################
kable(mm%>%
  ungroup()%>%
  select(Skirstymas,VISO_DALYVAVO_2016,VISO_DALYVAVO_2020)%>%
  group_by(Skirstymas)%>%
  summarise_all(sum)
)


################################################################################
#### LEFT VS LIB savivaldybės
################################################################################

hh <- nn%>%select(1,13,14)%>%
  mutate(left_rank=rank(nn$LEFT_diff))%>%
  gather(Kintamasis,values,2:3)

png("./rinkimu_balsai_lib_left_sav.png", width = 8, height = 10, units = 'in', res = 200)
ggplot(hh, aes(reorder(SAV,left_rank), values, fill=Kintamasis))+
  geom_bar(stat="identity",
           position = "stack")+
  coord_flip()+
  labs(title="Gautų balsų pokytis, LEFT vs LIB, 2016m. vs 2020m.",
       x="Savivaldybė",
       y="Skirtumas proc. punktai")+
  theme(legend.position = "bottom")
dev.off()



################################################################################
#### LEFT VS LIB amžiaus kategorijos
################################################################################

jj <- part_2020%>%
  filter(SAV!="")%>%
  select(3,7:40)%>%
  mutate(AKT_24	 =100*(BALS_24_V+BALS_24_M)/(RINK_24_V+RINK_24_M),
         AKT_25_29=100*(BALS_25_29_V+BALS_25_29_M)/(RINK_25_29_V+RINK_25_29_M),
         AKT_30_34=100*(BALS_30_34_V+BALS_30_34_M)/(RINK_30_34_V+RINK_30_34_M),
         AKT_35_44=100*(BALS_35_44_V+BALS_35_44_M)/(RINK_35_44_V+RINK_35_44_M),
         AKT_45_54=100*(BALS_45_54_V+BALS_45_54_M)/(RINK_45_54_V+RINK_45_54_M),
         AKT_55_64=100*(BALS_55_64_V+BALS_55_64_M)/(RINK_55_64_V+RINK_55_64_M),
         AKT_65_74=100*(BALS_65_74_V+BALS_65_74_M)/(RINK_65_74_V+RINK_65_74_M),
         AKT_75   =100*(BALS_75_V+BALS_75_M)/(RINK_75_V+RINK_75_M))%>%
  select(1,36:43)

ll <- left_join(mm, jj,by=("RPL_UNIKALUS_NUMERIS"))


png("./lib_aktyvumas_balsai.png", width = 10, height = 6, units = 'in', res = 200)
ggplot(ll, aes(AKT_25_29, LIB_2020_p, weight=VISO_DALYVAVO_2020))+
  geom_point(aes(size = VISO_DALYVAVO_2020^2, col=Skirstymas),alpha=0.5)+
  scale_size(name   = "Rinkeju sk.",
             breaks = fivenum(mm$VISO_DALYVAVO_2020)^2,
             labels = fivenum(mm$VISO_DALYVAVO_2020))+
  scale_shape(solid = F)+
  geom_smooth(method = "loess", se=FALSE)+
  labs(title="LIB rezultatas rinkimų apylinkėse 2020m. ir rinkėjų aktyvumas",
       x="Aktyvumas 25-29m. kategorijoje",
       y="Gautų balsų proc.")+
  theme(legend.position = "bottom")
dev.off()



png("./left_aktyvumas_balsai.png", width = 10, height = 6, units = 'in', res = 200)
ggplot(ll, aes(AKT_25_29, LEFT_2020_p, weight=VISO_DALYVAVO_2020))+
  geom_point(aes(size = VISO_DALYVAVO_2020^2, col=Skirstymas),alpha=0.5)+
  scale_size(name   = "Rinkeju sk.",
             breaks = fivenum(mm$VISO_DALYVAVO_2020)^2,
             labels = fivenum(mm$VISO_DALYVAVO_2020))+
  scale_shape(solid = F)+
  geom_smooth(method = "loess", se=FALSE)+
  labs(title="LEFT rezultatas rinkimų apylinkėse 2020m. ir rinkėjų aktyvumas",
       x="Aktyvumas 25-29m. kategorijoje",
       y="Gautų balsų proc.")+
  theme(legend.position = "bottom")
dev.off()

