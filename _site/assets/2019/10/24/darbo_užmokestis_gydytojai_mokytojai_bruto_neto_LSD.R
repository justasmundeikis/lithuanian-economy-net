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

df <-  S3R0050_M3060320_1 %>% 
  mutate(LAIKOTARPIS=parse_date_time(LAIKOTARPIS, "y q")) %>%
  filter(darboM3060321 %in% c("bruto", "neto"),
         EVRKM3060213 == "851e") %>%
  select(EVRKM3060213_label.lt,darboM3060321_label.lt, LAIKOTARPIS, obsValue)

df_2 <- cbind(
  S3R0050_M3060320_1 %>% 
    mutate(LAIKOTARPIS=parse_date_time(LAIKOTARPIS, "y q")) %>%
    filter(darboM3060321 %in% c("bruto"),
           EVRKM3060213 == "851e",
           LAIKOTARPIS>="2007-01-01") %>%
    select(EVRKM3060213_label.lt,darboM3060321_label.lt, LAIKOTARPIS, obsValue)%>%
    rename(gyd_alg=obsValue),
  S3R0050_M3060322 %>%
    mutate(LAIKOTARPIS=parse_date_time(LAIKOTARPIS, "y q")) %>%
    filter(darboM3060321 %in% c("bruto"),
           savivaldybesRegdb == "00",
           Ekon_sektoriusM3061118=="0ex",
           LAIKOTARPIS>="2007-01-01")%>%
    select(obsValue)%>%
    rename(vdu=obsValue))
df_2 <- df_2 %>%
  mutate(obsValue=gyd_alg/vdu)


jpeg("gydytojų_bruto_neto_LSD.jpeg", width = 9, height = 6, units = 'in', res = 200)
ggplot(data = df, aes(x=LAIKOTARPIS, y=obsValue, color=darboM3060321_label.lt)) + 
  geom_line(size=1.1)+
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(0,3300, by=250))+
  geom_hline(yintercept=3250, linetype="dashed", color = "red", size=2)+
  labs(title="Darbo užmokestis (Gydytojai - viešojo sektoriaus)",
       subtitle="Šaltinis: LSD (S3R0050_M3060320_1). Skaičiavimai: Lithuanian-Economy.net" ,
       y="Euro",
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank(),plot.title = element_text(hjust = 0))
dev.off()

jpeg("gydytojų_bruto_neto_LSD_rel.jpeg", width = 9, height = 6, units = 'in', res = 200)
ggplot(data = df_2, aes(x=LAIKOTARPIS, y=obsValue)) + 
  geom_line(size=1.1)+
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(limits = c(0,4), breaks = seq(0,4, by=0.5))+
  geom_hline(yintercept=3, linetype="dashed", color = "red", size=2)+
  labs(title="Santykinis gydytojų darbo užmokestis, VDU (Šalies ūkio be individualiųjų įmonių)",
       subtitle="Šaltinis: LSD (S3R0050_M3060320_1; S3R0050_M3060315_1). Skaičiavimai: Lithuanian-Economy.net" ,
       y="VDU",
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank(),plot.title = element_text(hjust = 0))
dev.off()