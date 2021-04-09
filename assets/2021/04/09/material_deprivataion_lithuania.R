## author:
## date_creation:
## date_modification
## purpose:

## libraries
if(!require("rsdmx")) install.packages("rsdmx"); require("rsdmx")
if(!require("tidyverse")) install.packages("tidyverse"); require("tidyverse")
if(!require("lubridate")) install.packages("lubridate"); require("lubridate")
if(!require("eurostat")) install.packages("eurostat"); require("eurostat")
if(!require("kableExtra")) install.packages("kableExtra"); require("kableExtra")


## settings
new_config <- httr::config(ssl_verifypeer = FALSE)
httr::set_config(new_config, override = FALSE)

## data download

# S3R0013 - Materialiniai nepritekliai
# Gyvenamoji vietovė | Nepritekliai
code <- "S3R0013_M3080220"
mat_dep_loc <- readSDMX(providerId = "LSD", resource = "data", flowRef = code, dsd = TRUE)%>%
  as.data.frame(.,labels = TRUE) %>%
  rename(date=LAIKOTARPIS,
         values=obsValue)

# S3R0013 - Materialiniai nepritekliai
# Namų ūkio tipas | Nepritekliai
code <- "S3R0013_M3080221"
mat_dep_hhtype <- readSDMX(providerId = "LSD", resource = "data", flowRef = code, dsd = TRUE)%>%
  as.data.frame(.,labels = TRUE) %>%
  rename(date=LAIKOTARPIS,
         values=obsValue)

# S3R0013 - Materialiniai nepritekliai
# Amžius | Nepritekliai
code <- "S3R0013_M3080222"
mat_dep_age <- readSDMX(providerId = "LSD", resource = "data", flowRef = code, dsd = TRUE)%>%
  as.data.frame(.,labels = TRUE) %>%
  rename(date=LAIKOTARPIS,
         values=obsValue)

# S3R0013 - Materialiniai nepritekliai
# Lytis | Nepritekliai
code <- "S3R0013_M3080247"
mat_dep_sex <- readSDMX(providerId = "LSD", resource = "data", flowRef = code, dsd = TRUE)%>%
  as.data.frame(.,labels = TRUE) %>%
  rename(date=LAIKOTARPIS,
         values=obsValue)

# S3R0013 - Materialiniai nepritekliai
# Nepritekliai | Regionai
code <- "S3R0013_M3080249"
mat_dep_region <- readSDMX(providerId = "LSD", resource = "data", flowRef = code, dsd = TRUE)%>%
  as.data.frame(.,labels = TRUE) %>%
  rename(date=LAIKOTARPIS,
         values=obsValue)


# S3R021 - Asmenys, gyvenantys namų ūkiuose, susiduriančiuose su ekonominiais sunkumais
# Gyvenamoji vietovė | Sunkumai
code <- "S3R021_M3080212"
hh_econ_hard_loc <- readSDMX(providerId = "LSD", resource = "data", flowRef = code, dsd = TRUE)%>%
  as.data.frame(.,labels = TRUE) %>%
  rename(date=LAIKOTARPIS,
         values=obsValue)

# S3R021 - Asmenys, gyvenantys namų ūkiuose, susiduriančiuose su ekonominiais sunkumais
# Namų ūkio tipas | Sunkumai
code <- "S3R021_M3080213"
hh_econ_hard_hhtype <- readSDMX(providerId = "LSD", resource = "data", flowRef = code, dsd = TRUE)%>%
  as.data.frame(.,labels = TRUE) %>%
  rename(date=LAIKOTARPIS,
         values=obsValue)

# S3R021 - Asmenys, gyvenantys namų ūkiuose, susiduriančiuose su ekonominiais sunkumais
# Apskritys | Sunkumai
code <- "S3R021_M3080215"
hh_econ_hard_nuts3 <- readSDMX(providerId = "LSD", resource = "data", flowRef = code, dsd = TRUE)%>%
  as.data.frame(.,labels = TRUE) %>%
  rename(date=LAIKOTARPIS,
         values=obsValue)


# S3R019 - Asmenys, neturintys ilgalaikio naudojimo daiktų dėl lėšų stokos		
# Gyvenamoji vietovė | Ilgalaikio naudojimo prietaisas
code <- "S3R019_M3080216"
hh_durables_durable_loc <- readSDMX(providerId = "LSD", resource = "data", flowRef = code, dsd = TRUE)%>%
  as.data.frame(.,labels = TRUE) %>%
  rename(date=LAIKOTARPIS,
         values=obsValue)

# S3R019 - Asmenys, gyvenantys namų ūkiuose, neturinčiuose ilgalaikio naudojimo daiktų dėl lėšų stokos
# Ilgalaikio naudojimo prietaisas | Namų ūkio tipas
code <- "S3R019_M3080217"
hh_durables_durable_hhtype <- readSDMX(providerId = "LSD", resource = "data", flowRef = code, dsd = TRUE)%>%
  as.data.frame(.,labels = TRUE) %>%
  rename(date=LAIKOTARPIS,
         values=obsValue)


## data processing, chart generation

df <- mat_dep_loc %>% 
  mutate(date=parse_date_time(date, "y"))%>%
  filter(vietoveM2040101_label.lt=="Miestas ir kaimas")%>%
  mutate(nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis" )))


png("lsd_lietuva_mat_dep_total_ts.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(data=df, aes(x=as.Date(date), y=values, color=nepritekliai_label.lt))+
  geom_line(size = 1.2)+
  geom_point()+
  coord_cartesian(ylim = c(0, 40))+
  scale_colour_manual(values = c("#1f78b4", "#d7191c"))+
  #scale_colour_brewer(palette="Set1")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(title="Materialiniai nepritekliai (lygis), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080220). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


## by location ts

df <- mat_dep_loc %>% 
  mutate(date=parse_date_time(date, "y"),
  vietoveM2040101_label.lt=factor(vietoveM2040101_label.lt, 
                                         levels=c("Miestas ir kaimas",
                                                  "Miestas",
                                                  "Didieji miestai",
                                                  "Kiti miestai",
                                                  "Kaimas")),
         nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis" )))%>%
    filter(vietoveM2040101 %in% c(2,11,12))

png("lsd_lietuva_mat_dep_loc_ts.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(data=df, aes(x=as.Date(date), y=values, color=vietoveM2040101_label.lt))+
  geom_line(size = 1.2)+
  geom_point()+
  coord_cartesian(ylim = c(0, 40))+
  facet_wrap(~nepritekliai_label.lt)+
  scale_colour_brewer(palette="Set1")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(title="Materialiniai nepritekliai (lygis, vietovė), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080220). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Laikotarpis",
       caption = "Didieji miestai: Vilnius, Kaunas, Klaipėda, Šiauliai, Panevėžys")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

## by location bar

df <- mat_dep_loc %>% 
  mutate(date=year(parse_date_time(date, "y")))%>%
  filter(date %in% c(max(date), max(date)-1, max(date)-2))%>%
  mutate(date=factor(date, levels=c(max(date)-2,max(date)-1,max(date))))%>%
  mutate(vietoveM2040101_label.lt=factor(vietoveM2040101_label.lt, 
                                         levels=c("Miestas ir kaimas",
                                                  "Miestas",
                                                  "Didieji miestai",
                                                  "Kiti miestai",
                                                  "Kaimas")),
         nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis" )))

png("lsd_lietuva_mat_dep_loc_bar.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(data=df, aes(x=vietoveM2040101_label.lt, y=values, fill=date))+
  geom_bar(stat="identity",
           position="dodge")+
  #scale_colour_brewer(palette="Set1")+
  scale_fill_manual(values = c("Orange", "#1f78b4", "#d7191c"))+
  coord_cartesian(ylim = c(0, 35))+
  geom_text(aes(label=values), position = position_dodge(1), vjust = -0.5, size=3)+
  facet_wrap(~nepritekliai_label.lt)+
  labs(title="Materialiniai nepritekliai (lygis, vietovė), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080220). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# by hh type bar

df <- mat_dep_hhtype%>%
  mutate(date=year(parse_date_time(date, "y")))%>%
  filter(date %in% c(max(date),max(date)-1))%>%
  mutate(date=factor(date, levels=c(max(date),max(date)-1)))%>%
  mutate(nutipasM2050102_label.lt=factor(nutipasM2050102_label.lt, 
                                         levels=c("Trys ar daugiau suaugusių asmenų su vaikais",
                                                  "Du suaugę asmenys su trim ir daugiau vaikų",
                                                  "Du suaugę asmenys su dviem vaikais",
                                                  "Du suaugę asmenys su vienu vaiku",
                                                  "Vienas suaugęs asmuo su vienu ar daugiau vaikų",
                                                  "Namų ūkiai su vaikais",
                                                  "Trys ar daugiau suaugusių asmenų be vaikų",
                                                  "Du suaugę asmenys, bent vienas 65 metų ar vyresnis, be vaikų",
                                                  "Du suaugę jaunesni nei 65 metų asmenys be vaikų",
                                                  "Vienas asmuo",
                                                  "Namų ūkiai be vaikų")),
         nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis")))

png("lsd_lietuva_mat_dep_hhtype_bar.png", width = 10, height = 10, units = 'in', res = 200)
ggplot(df, aes(x=nutipasM2050102_label.lt, y=values, fill = date)) +   
  geom_bar(position = "dodge", 
           stat="identity")+
  coord_flip() +
  facet_grid(rows=vars(nepritekliai_label.lt))+
  scale_fill_manual(values = c("#d7191c","#1f78b4"))+
  geom_text(aes(label=values), position = position_dodge(0.8), vjust = 0.5, hjust=-.2, size=3)+
  labs(title="Materialiniai nepritekliai (lygis, namų ūkio tipas), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080221). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Namų ūkio tipas")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1, size=2))
dev.off()


# by hh, selected groups, ts
df <- mat_dep_hhtype%>%
  mutate(date=parse_date_time(date, "y"))%>%
  mutate(nutipasM2050102_label.lt=factor(nutipasM2050102_label.lt, 
                                         levels=c("Namų ūkiai be vaikų",
                                                  "Vienas asmuo",
                                                  "Du suaugę jaunesni nei 65 metų asmenys be vaikų",
                                                  "Du suaugę asmenys, bent vienas 65 metų ar vyresnis, be vaikų",
                                                  "Trys ar daugiau suaugusių asmenų be vaikų",
                                                  "Namų ūkiai su vaikais",
                                                  "Vienas suaugęs asmuo su vienu ar daugiau vaikų",
                                                  "Du suaugę asmenys su vienu vaiku",
                                                  "Du suaugę asmenys su dviem vaikais",
                                                  "Du suaugę asmenys su trim ir daugiau vaikų",
                                                  "Trys ar daugiau suaugusių asmenų su vaikais")),
         nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis")))%>%
  filter(nutipasM2050102_label.lt %in% c("Du suaugę asmenys su trim ir daugiau vaikų",
                                         "Vienas suaugęs asmuo su vienu ar daugiau vaikų",
                                         "Vienas asmuo"))

png("lsd_lietuva_mat_dep_hhtype_selgroup_ts.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(df, aes(x=as.Date(date), y=values, color = nutipasM2050102_label.lt)) +   
  geom_line(size = 1.2)+
  geom_point()+
  coord_cartesian(ylim = c(0, max(df$values)*1.1))+
  scale_y_continuous(breaks=seq(0,100,10))+
  facet_wrap(~nepritekliai_label.lt)+
  scale_colour_brewer(palette="Set1")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(title="Materialiniai nepritekliai (lygis, pasirinkti namų ūkio tipai), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080221). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()



# by age group bar

df <- mat_dep_age%>%
  mutate(date=year(parse_date_time(date, "y")))%>%
  filter(date %in% c(max(date),max(date)-1,max(date)-2))%>%
  mutate(date=factor(date, levels=c(max(date)-2, max(date)-1, max(date))))%>%
  mutate(amziusM3080222_label.lt=factor(amziusM3080222_label.lt, 
                                        levels=c("0–17",
                                                 "18–24",
                                                 "25–49",
                                                 "50–64",
                                                 "65 ir vyresni")),
         nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis")))

png("lsd_lietuva_mat_dep_age_bar.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(df, aes(x=amziusM3080222_label.lt, y=values, fill = date)) +   
  geom_bar(position = "dodge", 
           stat="identity")+
  facet_wrap(~nepritekliai_label.lt)+
  scale_fill_manual(values = c("Orange", "#1f78b4", "#d7191c"))+
  geom_text(aes(label=values), position = position_dodge(1), vjust = -0.5, size=3)+
  labs(title="Materialiniai nepritekliai (lygis, amžiaus grupės), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080222). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Amžiaus grupės")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# by age ts

df <- mat_dep_age%>%
  mutate(date=parse_date_time(date, "y"))%>%
  mutate(amziusM3080222_label.lt=factor(amziusM3080222_label.lt, 
                                        levels=c("0–17",
                                                 "18–24",
                                                 "25–49",
                                                 "50–64",
                                                 "65 ir vyresni")),
         nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis")))

png("lsd_lietuva_mat_dep_age_ts.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(df, aes(x=as.Date(date), y=values, color = amziusM3080222_label.lt)) +   
  geom_line(size = 1.2)+
  geom_point()+
  coord_cartesian(ylim = c(0, 50))+
  facet_wrap(~nepritekliai_label.lt)+
  scale_colour_brewer(palette="Set1")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(title="Materialiniai nepritekliai (lygis,  amžiaus grupės), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080222). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# by sex group ts

df <- mat_dep_sex %>%
  mutate(date=parse_date_time(date, "y"))%>% 
  mutate(nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis")))

png("lsd_lietuva_mat_dep_sex_ts.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(df, aes(x=as.Date(date), y=values, color = lytisVM_label.lt, linetype=nepritekliai_label.lt)) +   
  geom_line(size = 1.2)+
  geom_point(size = 2.5)+
  coord_cartesian(ylim = c(0, 50))+
  scale_colour_brewer(palette="Set1")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(title="Materialiniai nepritekliai (lygis, lytis), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080247). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# by sex group bar

df <- mat_dep_sex%>%
  mutate(date=year(parse_date_time(date, "y")))%>%
  filter(date %in% c(max(date),max(date)-1,max(date)-2))%>%
  mutate(date=factor(date, levels=c(max(date)-2, max(date)-1, max(date))))%>%
  mutate(nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis")))

png("lsd_lietuva_mat_dep_sex_bar.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(df, aes(x=lytisVM_label.lt, y=values, fill = date)) +   
  geom_bar(position = "dodge", 
           stat="identity")+
  facet_wrap(~nepritekliai_label.lt)+
  scale_fill_manual(values = c("Orange","#1f78b4","#d7191c"))+
  geom_text(aes(label=values), position = position_dodge(1), vjust = -0.5, size=3)+
  labs(title="Materialiniai nepritekliai (lygis, lytis), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080247). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Lytis")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# by sex diff ts

df <- mat_dep_sex %>%
  mutate(date=parse_date_time(date, "y"))%>% 
  mutate(nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis")))%>%
  select(lytisVM_label.lt,nepritekliai_label.lt,date, values)%>%
  spread(lytisVM_label.lt, values)%>%
  mutate(Atotrūkis=Moterys-Vyrai)%>%
  select(nepritekliai_label.lt,date,Atotrūkis)


png("lsd_lietuva_mat_dep_sex_diff_ts.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(df, aes(x=as.Date(date), y=Atotrūkis, color=nepritekliai_label.lt)) +   
  geom_line(size = 1.2)+
  geom_point(size = 2.5)+
  scale_colour_manual(values = c("#1f78b4", "#d7191c"))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(title="Materialiniai nepritekliai, atotrūkis tarp vyrų ir moterų, proc. punktai", 
       subtitle="Šaltinis: LSD (S3R0013_M3080247). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Proc. punktai", 
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()






# by region ts
df <- mat_dep_region %>%
  mutate(date=parse_date_time(date, "y"))%>% 
  mutate(nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis")))

png("lsd_lietuva_mat_dep_nuts2_ts.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(df, aes(x=as.Date(date), y=values, color = savivaldybes_tik_regionai_label.lt)) +   
  geom_line(size = 1.2)+
  geom_point()+
  coord_cartesian(ylim = c(0, 50))+
  facet_wrap(~nepritekliai_label.lt)+
  #scale_colour_manual(values = c("#1f78b4", "#d7191c"))+
  scale_colour_brewer(palette="Set1")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(title="Materialiniai nepritekliai (lygis, NUTS2), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080249). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Laikotarpis")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()




# by sex region bar

df <- mat_dep_region%>%
  mutate(date=year(parse_date_time(date, "y")))%>%
  filter(date %in% c(max(date),max(date)-1))%>%
  mutate(date=factor(date, levels=c(max(date)-1,max(date))))%>%
  mutate(nepritekliai_label.lt=factor(nepritekliai_label.lt,
                                      levels=c("Materialinio nepritekliaus lygis",
                                               "Didelio materialinio nepritekliaus lygis")))

png("lsd_lietuva_mat_dep_nuts2_bar.png", width = 8, height = 5, units = 'in', res = 200)
ggplot(df, aes(x=savivaldybes_tik_regionai_label.lt, y=values, fill = date)) +   
  geom_bar(position = "dodge", 
           stat="identity")+
  facet_wrap(~nepritekliai_label.lt)+
  scale_fill_manual(values = c("#1f78b4","#d7191c"))+
  geom_text(aes(label=values), position = position_dodge(1), vjust = -0.5, size=3)+
  labs(title="Materialiniai nepritekliai (lygis, NUTS2), proc.", 
       subtitle="Šaltinis: LSD (S3R0013_M3080249). Skaičiavimai: Lithuanian-Economy.net" , 
       y="Procentai", 
       x="Lytis")+
  theme(plot.title = element_text(hjust = 0, face="bold"),
        legend.position = "bottom",
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()



## econ hard loc S3R021_M3080212
## hh durables loc S3R019_M3080216
## TABLE _ 3 years

df_1 <- hh_econ_hard_loc %>%
  mutate(date=year(parse_date_time(date, "y")))%>%
  filter(date %in% c(max(date),max(date)-1,max(date)-2),
         vietoveM2040101==0)%>%
  select(sunkumaiM3080212_label.lt, date , values)%>%
  rename(Sunkumai=sunkumaiM3080212_label.lt)%>%
  spread(date, values)


df_2 <- hh_durables_durable_loc%>%
  mutate(date=year(parse_date_time(date, "y")))%>%
  filter(date %in% c(max(date),max(date)-1,max(date)-2),
         vietoveM2040101==0)%>%
  select(prietaisaiM3080216_label.lt, date, values)%>%
  rename(Sunkumai=prietaisaiM3080216_label.lt)%>%
  na.omit()%>%
  spread(date, values)

df <- rbind(df_1, df_2)

kable(df)%>%
  writeLines("table_hard_durables_3_year.html")

## econ hard loc S3R021_M3080212
## hh durables loc S3R019_M3080216
## TABLE

df_1 <- hh_econ_hard_loc %>%
  mutate(date=parse_date_time(date, "y"))%>% 
filter(date==max(date))%>%
  select(sunkumaiM3080212_label.lt, vietoveM2040101_label.lt, values)%>%
  rename(Sunkumai=sunkumaiM3080212_label.lt,
         geo=vietoveM2040101_label.lt)%>%
  spread(geo, values)


df_2 <- hh_durables_durable_loc%>%
  mutate(date=parse_date_time(date, "y"))%>% 
  filter(date==max(date))%>%
  select(prietaisaiM3080216_label.lt, vietoveM2040101_label.lt, values)%>%
  rename(Sunkumai=prietaisaiM3080216_label.lt,
         geo=vietoveM2040101_label.lt)%>%
  na.omit()%>%
  spread(geo, values)

df <- rbind(df_1, df_2)
kable(df)%>%
  writeLines("table_hard_durables_loc.html")


## econ hard hhtype S3R021_M3080213
## hh durables hhtype S3R019_M3080217
## TABLE

df_1 <- hh_econ_hard_hhtype %>%
  mutate(date=parse_date_time(date, "y"))%>% 
  filter(date==max(date))%>%
  select(sunkumaiM3080212_label.lt, nutipasM2050102_label.lt, values)%>%
  rename(Sunkumai=sunkumaiM3080212_label.lt,
         "Namų ūkio tipas"=nutipasM2050102_label.lt)%>%
  spread(Sunkumai, values)


df_2 <- hh_durables_durable_hhtype%>%
  mutate(date=parse_date_time(date, "y"))%>% 
  filter(date==max(date))%>%
  select(prietaisaiM3080216_label.lt, nutipasM2050102_label.lt, values)%>%
  rename(Sunkumai=prietaisaiM3080216_label.lt,
         "Namų ūkio tipas"=nutipasM2050102_label.lt)%>%
  na.omit()%>%
  spread(Sunkumai, values)


df <- left_join(df_1, df_2)
kable(df, booktabs = TRUE)%>%
  kable_styling(font_size = 10)%>%
  writeLines("table_hard_durables_hh.html")



