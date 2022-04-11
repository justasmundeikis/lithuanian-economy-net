library(WDI)
library(tidyverse)
library(lubridate)
library(eurostat)

iso <- c("AF", "AX", "AL", "DZ", "AS", "AD", "AO", "AI", "AQ", "AG", "AR", "AM", "AW", "AU", "AT", "AZ", "BS", "BH", "BD", "BB", "BY", "BE", "BZ", "BJ", "BM", "BT", "BO", "BQ", "BA", "BW", "BV", "BR", "IO", "BN", "BG", "BF", "BI", "KH", "CM", "CA", "CV", "KY", "CF", "TD", "CL", "CN", "CX", "CC", "CO", "KM", "CG", "CD", "CK", "CR", "CI", "HR", "CU", "CW", "CY", "CZ", "DK", "DJ", "DM", "DO", "EC", "EG", "SV", "GQ", "ER", "EE", "ET", "FK", "FO", "FJ", "FI", "FR", "GF", "PF", "TF", "GA", "GM", "GE", "DE", "GH", "GI", "GR", "GL", "GD", "GP", "GU", "GT", "GG", "GN", "GW", "GY", "HT", "HM", "VA", "HN", "HK", "HU", "IS", "IN", "ID", "IR", "IQ", "IE", "IM", "IL", "IT", "JM", "JP", "JE", "JO", "KZ", "KE", "KI", "KP", "KR", "KW", "KG", "LA", "LV", "LB", "LS", "LR", "LY", "LI", "LT", "LU", "MO", "MK", "MG", "MW", "MY", "MV", "ML", "MT", "MH", "MQ", "MR", "MU", "YT", "MX", "FM", "MD", "MC", "MN", "ME", "MS", "MA", "MZ", "MM", "NA", "NR", "NP", "NL", "NC", "NZ", "NI", "NE", "NG", "NU", "NF", "MP", "NO", "OM", "PK", "PW", "PS", "PA", "PG", "PY", "PE", "PH", "PN", "PL", "PT", "PR", "QA", "RE", "RO", "RU", "RW", "BL", "SH", "KN", "LC", "MF", "PM", "VC", "WS", "SM", "ST", "SA", "SN", "RS", "SC", "SL", "SG", "SX", "SK", "SI", "SB", "SO", "ZA", "GS", "SS", "ES", "LK", "SD", "SR", "SJ", "SZ", "SE", "CH", "SY", "TW", "TJ", "TZ", "TH", "TL", "TG", "TK", "TO", "TT", "TN", "TR", "TM", "TC", "TV", "UG", "UA", "AE", "GB", "US", "UM", "UY", "UZ", "VU", "VE", "VN", "VG", "VI", "WF", "EH", "YE", "ZM", "ZW")


gdp_capita_ppp_data <- WDI(indicator='NY.GDP.PCAP.PP.KD', country=c('RU','UA', "LT", "HU"), start=1960, end=2022)

gdp_capita_ppp_data%>%
  tibble()%>%
  rename(gdp_capita_ppp = NY.GDP.PCAP.PP.KD)%>%
  mutate(year=ymd(paste0(year,"-01-01")))%>%
  na.omit()%>%
  ggplot(aes(year, gdp_capita_ppp, color=country))+
  geom_line(size=1.1)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_color_brewer(palette = "Set1")+
  labs(title = "BVP tenkantis vienam gyventojui, perkamosios galios standartas",
       subtitle= "Duomenų šaltinis: Pasaulio bankas (NY.GDP.PCAP.PP.KD). Skaičiavimai: Lithuanian-Economy.net",
       x="Laikotarpis",
       y="Perkamosios galios standartas, vienetai",
       col="Šalis")+
  theme(legend.position = "bottom")



gdp_w_data <- WDI(indicator='NY.GDP.MKTP.CD')%>%
  rename(gdp=NY.GDP.MKTP.CD)%>%
  tibble()


gdp_w_data %>% 
  filter(year==2020,
         iso2c %in% iso) %>%
  mutate(gdp=gdp/1e12)%>%
  arrange(desc(gdp))%>%
  head(15)%>%
  ggplot(aes(reorder(country, -gdp), gdp))+
  geom_col(fill="#00aaff")+
  labs(title = "Nominalus BVP USD, trilijonais, 2020m",
       subtitle= "Duomenų šaltinis: Pasaulio bankas (NY.GDP.MKTP.CD). Skaičiavimai: Lithuanian-Economy.net",
       x="Šalis (top15)",
       y="Trilijonai USD")+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

eu_gas <- get_eurostat("nrg_ti_gasm")

eu_gas%>%
  mutate(year=year(time))%>%
  filter(year==2021,
         geo %in% c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","IS","NO","UK","UA"),
         partner %in% c("TOTAL", "RU" ),
         siec=="G3000",
         unit=="MIO_M3")%>%
  group_by(geo, partner,  year)%>%
  summarise(values=sum(values))%>%
  spread(partner, values)%>%
  mutate(OTHER=TOTAL-RU)%>%
  gather(partner, values, 3:5)%>%
  filter(partner!="TOTAL")%>%
  ggplot(aes(geo, values, fill=partner))+
  geom_col(position = "fill")

  labs(title = "Dujų suvartojimas šalyje, mln kub. m, 2021m`",
       subtitle= "Duomenų šaltinis: Eurostat (nrg_cb_gasm). Skaičiavimai: Lithuanian-Economy.net",
       x="Šalys",
       y="Milijonai kūbinių metrų")+
  theme(legend.position = "bottom")
  
  
