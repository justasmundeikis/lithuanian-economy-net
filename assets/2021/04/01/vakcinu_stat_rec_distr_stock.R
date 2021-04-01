library(tidyverse)
library(jsonlite)

received <- read.csv("./rec.csv")
distributed <- read.csv("./distr.csv")
data_lt_vak <- readLines("https://opendata.arcgis.com/datasets/ffb0a5bfa58847f79bf2bc544980f4b6_0.geojson")
data_lt_vak <- fromJSON(data_lt_vak, flatten = TRUE)
data_lt_vak <- as.data.frame(data_lt_vak$features)%>%
  select(4:(ncol(.)-1))
names(data_lt_vak)<- gsub("\\properties.", "", names(data_lt_vak))


received <- received %>%
  spread(code, count)%>%
  mutate(date=as.Date(date))

distributed <- distributed%>%
  spread(code, count)%>%
  mutate(date=as.Date(date))

vac <- data.frame(mapply(c,data_lt_vak[,c(6,8)], data_lt_vak[,c(7,9)],SIMPLIFY=FALSE))%>%
  na.omit()
colnames(vac) <- c("date", "name")
vac$name <- gsub("Pfizer-BioNTech", "vac_P", vac$name )
vac$name <- gsub("Moderna", "vac_M", vac$name )
vac$name <- gsub("AstraZeneca", "vac_A", vac$name )

vac <- vac %>%
  mutate(date=as.Date(date))

vac <- vac %>%
  group_by(date, name)%>%
  summarise(n=n())%>%
  spread(name, n, fill = 0)


date <- data.frame(date=seq.Date(from = as.Date("2020-12-20"),
                                 to=as.Date("2021-04-01"), 
                                 by="day"))

df <- left_join(date, received, by="date" )%>%
  left_join(., distributed, by="date")%>%
  left_join(., vac, by="date")


df[is.na(df)] <- 0


df <- df%>%
  mutate(stock_A=cumsum(rec_A)-cumsum(distr_A),
         stock_M=cumsum(rec_M)-cumsum(distr_M),
         stock_P=cumsum(rec_P)-cumsum(distr_P),
         
         likutis_AstraZeneca=cumsum(distr_A)-cumsum(vac_A),
         likutis_Moderna=cumsum(distr_M)-cumsum(vac_M),
         likutis_Pfizer_BioNTech=cumsum(distr_P)-cumsum(vac_P),
         
         akum_gauta_viso=cumsum(rec_A + rec_M + rec_P),
         akum_paskirstyta_viso=cumsum(distr_A + distr_M + distr_P),
         akum_vakcinuota_viso=cumsum(vac_A + vac_M + vac_P),
         
         ministerijos_šaldytuve=stock_A + stock_P + stock_M,
         vak_centrų_šaldytuve=akum_gauta_viso - akum_vakcinuota_viso)


dfw <- df%>%
  select(date, akum_gauta_viso, akum_paskirstyta_viso, akum_vakcinuota_viso, ministerijos_šaldytuve, vak_centrų_šaldytuve)%>%
  gather(var, values, 2:6)%>%
  mutate(values=values/1000)

png("./vac_img_1.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(dfw,aes(date, values, col=var))+
  geom_line()+
  scale_x_date(breaks="7 day", date_labels = "%Y-%m-%d")+
  scale_y_continuous(breaks = seq(from=0, to=700, by=100))+
  scale_color_brewer(palette="Set1")+
  labs(title="Gautos ir paskirstytos vakcinos, skiepai ir sandėliavimas",
       subtitle="Šaltinis: LSD, skaičiavimai: Lithuanian-Economy.net",
       x="Laikotarpis", 
       y="Skaičius, tūkst.")+
  theme(legend.title=element_blank(),
        legend.position='bottom',
        axis.text.x=element_text(angle=45, hjust=1))+
  guides(col = guide_legend(ncol = 3, byrow = TRUE))
dev.off()


df_z <- df%>%
  select(1,14,15,16)%>%
  gather(var,values, 2:4)%>%
  mutate(values=values/1000)

png("./vac_img_2.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(df_z, aes(date, values, col=var))+
  geom_line(size=1)+
  scale_x_date(breaks="7 day", date_labels = "%Y-%m-%d")+
  scale_y_continuous(breaks = seq(from=0, to=100, by=10))+
  geom_vline(xintercept = as.Date("2021-03-10"), size=1)+
  scale_color_brewer(palette="Set1")+
  labs(title="Sukauptos vakcinos paskirstymo centruose",
       subtitle="Šaltinis: LSD, skaičiavimai: Lithuanian-Economy.net",
       x="Laikotarpis", 
       y="Skaičius, tūkst.")+
  theme(legend.title=element_blank(),
        legend.position='bottom',
        axis.text.x=element_text(angle=45, hjust=1))
dev.off()

