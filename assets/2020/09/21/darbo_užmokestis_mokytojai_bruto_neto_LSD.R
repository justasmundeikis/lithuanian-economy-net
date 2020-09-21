library(tidyverse)
library(rsdmx)
library(lubridate)

# S3R0050 - Average earnings (monthly)	
#	Occupation (teachers, doctors) | Type 
S3R0050_M3060320_1<- readSDMX(providerId = "LSD", resource = "data", flowRef = "S3R0050_M3060320_1", dsd = TRUE)
S3R0050_M3060320_1<- as.data.frame(S3R0050_M3060320_1 , labels = TRUE)

# S3R0050 - Average earnings (monthly)	
# Administrative territory | Sector | Type 
S3R0050_M3060322<- readSDMX(providerId = "LSD", resource = "data", flowRef = "S3R0050_M3060322", dsd = TRUE)
S3R0050_M3060322<- as.data.frame(S3R0050_M3060322 , labels = TRUE)

df_1 <-  S3R0050_M3060320_1 %>% 
  mutate(LAIKOTARPIS=parse_date_time(LAIKOTARPIS, "y q")) %>%
  filter(darboM3060321 %in% c("bruto", "neto"),
         EVRKM3060213 == "8021e") %>%
  select(EVRKM3060213_label.lt,darboM3060321_label.lt, LAIKOTARPIS, obsValue)

df_a <- S3R0050_M3060320_1 %>% 
    mutate(LAIKOTARPIS=parse_date_time(LAIKOTARPIS, "y q")) %>%
    filter(darboM3060321 %in% c("bruto"),
           EVRKM3060213 == "8021e",
           LAIKOTARPIS>="2007-01-01") %>%
    select(LAIKOTARPIS, obsValue)%>%
    rename(mok_alg=obsValue)

df_b <- S3R0050_M3060322 %>%
    mutate(LAIKOTARPIS=parse_date_time(LAIKOTARPIS, "y q")) %>%
    filter(darboM3060321 %in% c("bruto"),
           savivaldybesRegdb == "00",
           Ekon_sektoriusM3061118=="0ex",
           Lytis==0,
           LAIKOTARPIS>="2007-01-01")%>%
    select(LAIKOTARPIS,obsValue)%>%
    rename(vdu=obsValue)

df_2 <- merge(df_a, df_b)%>%
  mutate(obsValue=round(mok_alg/vdu*100,1))%>%
  mutate(sant_raida=round((obsValue/lag(obsValue,4)-1)*100,1))



vdu_actual <- df_3$vdu[df_3$LAIKOTARPIS==max(df_3$LAIKOTARPIS)]
vdu_koef <- 130

png("mokytojų_bruto_neto_LSD.png", width = 9, height = 6, units = 'in', res = 200)
ggplot(data = df, aes(x=LAIKOTARPIS, y=obsValue, color=darboM3060321_label.lt)) + 
  geom_line(size=1.1)+
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(limits=c(0,2000), breaks = seq(0,3300, by=250))+
  geom_hline(yintercept=vdu_actual*vdu_koef/100, linetype="dashed", color = "red", size=2)+
  labs(title="Mokytojų (viešojo sektoriaus) bruto  ir neto darbo užmokestis, 1.3 VDU riba",
       subtitle="Šaltinis: LSD (S3R0050_M3060320_1). Skaičiavimai: Lithuanian-Economy.net" ,
       y="Euro",
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank(),plot.title = element_text(hjust = 0))
dev.off()

png("mokytojų_bruto_neto_LSD_rel.png", width = 9, height = 6, units = 'in', res = 200)
ggplot(data = df_2, aes(x=LAIKOTARPIS, y=obsValue)) + 
  geom_line(size=1.1)+
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200, by=25))+
  geom_hline(yintercept=vdu_koef, linetype="dashed", color = "red", size=2)+
  labs(title="Santykinis mokytojų darbo užmokestis % VDU (Šalies ūkio be individualiųjų įmonių)",
       subtitle="Šaltinis: LSD (S3R0050_M3060320_1; S3R0050_M3060315_1). Skaičiavimai: Lithuanian-Economy.net" ,
       y="VDU",
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank(),plot.title = element_text(hjust = 0))
dev.off()

png("mokytojų_sant_raida.png", width = 9, height = 6, units = 'in', res = 200)
ggplot(data = df_2%>%filter(year(LAIKOTARPIS)>=2016), aes(x=LAIKOTARPIS, y=sant_raida)) + 
  geom_line(size=1.1)+
  geom_point()+
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(-10,15, by=2.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "blue", size=1.1)+
  labs(title="Metinis santykinio mokytojų darbo užmokesčio (% VDU) augimas, ketvirtiniai duomenys",
       subtitle="Šaltinis: LSD (S3R0050_M3060320_1; S3R0050_M3060315_1). Skaičiavimai: Lithuanian-Economy.net" ,
       y="VDU",
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank(),plot.title = element_text(hjust = 0))
dev.off()