# Author: Justas Mundeikis (Lithuanian-Economy.net)
# Date_creation: 2020-02-10
# Date_modification:---
# Description: 

# Loading packages ====
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("eurostat")) install.packages("eurostat"); library("eurostat")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("RColorBrewer")) install.packages("RColorBrewer"); library("RColorBrewer")
if(!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")
if(!require("rsdmx")) install.packages("rsdmx"); library("rsdmx")
if(!require("knitr")) install.packages("knitr"); library("knitr")

# creating subfolder for figures ====
if(!dir.exists("figures")) dir.create("figures")

# Downloading datasets EUROSTAT  ====

# General government expenditure by function (COFOG) 
gov_10a_exp <- get_eurostat("gov_10a_exp", 
                            stringsAsFactors=FALSE, 
                            filters = list(cofog99=c("GF07","GF0701","GF0702","GF0703","GF0704","GF0705","GF0706"),
                                           geo=c("EU28","EU27_2020","BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK","IS","LI","NO","CH"),
                                           na_item="TE",
                                           sector="S13",
                                           unit=c("PC_GDP", "PC_TOT")))


# Main national accounts tax aggregates 
gov_10a_taxag <- get_eurostat("gov_10a_taxag", stringsAsFactors=FALSE)

# GDP and main components (output, expenditure and income)
nama_10_gdp <- get_eurostat("nama_10_gdp", stringsAsFactors = FALSE)

# Income quintile share ratio S80/S20 for disposable income by sex and age group - EU-SILC survey 
ilc_di11 <- get_eurostat("ilc_di11", stringsAsFactors = FALSE)

# Income quintile share ratio S80/S20 for net market income by sex and age group - EU-SILC survey
ilc_di11b <-get_eurostat("ilc_di11b", stringsAsFactors = FALSE)

# Downloading datasets OECD ====

# OECD DATA   Teachers' statutory salaries 
EAG_TS_STA <- readSDMX(providerId = "OECD", resource = "data", flowRef = "EAG_TS_STA")
EAG_TS_STA <- as.data.frame(EAG_TS_STA)

# OECD DATA  Average annual wages 
AV_AN_WAGE <- readSDMX(providerId = "OECD", resource = "data", flowRef = "AV_AN_WAGE")
AV_AN_WAGE <- as.data.frame(AV_AN_WAGE)

# OECD DATA  Average annual wages 
IDD <- readSDMX(providerId = "OECD", resource = "data", flowRef = "IDD")
IDD <- as.data.frame(IDD)

# Downloading datasets LSD ====
#Darbo uzmokestis
S3R0050_M3060315_1 <- readSDMX(providerId = "LSD", resource = "data", flowRef = "S3R0050_M3060315_1", dsd = TRUE)
S3R0050_M3060315_1 <- as.data.frame(S3R0050_M3060315_1 , labels = TRUE)

S3R0050_M3060320_1 <- readSDMX(providerId = "LSD", resource = "data", flowRef = "S3R0050_M3060320_1", dsd = TRUE)
S3R0050_M3060320_1 <- as.data.frame(S3R0050_M3060320_1 , labels = TRUE)

S3R0050_M3060322 <- readSDMX(providerId = "LSD", resource = "data", flowRef = "S3R0050_M3060322", dsd = TRUE)
S3R0050_M3060322 <- as.data.frame(S3R0050_M3060322 , labels = TRUE)




# General government expenditure on education, GF09, POINT ====
# COM:  Check manually on latest year available, filter accordingly

df_gov_ex_07 <- gov_10a_exp %>%
        filter(
                cofog99=="GF07", 
                sector=="S13",
                na_item=="TE",
                geo %in% c("EU27_2020","BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK","IS","LI","NO","CH"),
                year(time)=="2018",
                unit %in% c("PC_GDP", "PC_TOT")
        ) %>%
        spread(unit, values)

last_year <-   strftime(max(df_gov_ex_07$time),"%Y")

png("./figures/general_government_expenditure_on_education.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(data=df_gov_ex_07, aes(x=PC_GDP, y=PC_TOT))+
        geom_point()+
        scale_x_continuous(limits = c(min(df_gov_ex_07$PC_GDP), max(df_gov_ex_07$PC_GDP)), breaks = seq(0, 100, by = 1))+
        scale_y_continuous(limits = c(min(df_gov_ex_07$PC_TOT), max(df_gov_ex_07$PC_TOT)), breaks = seq(0, 100, by = 1))+
        geom_text(aes(label=geo), hjust=1, vjust=1.5, size=3)+
        geom_hline(yintercept=df_gov_ex_07$PC_TOT[df_gov_ex_07$geo=="EU27_2020"])+
        geom_vline(xintercept = df_gov_ex_07$PC_GDP[df_gov_ex_07$geo=="EU27_2020"])+
        labs(title=paste("Valdžios sektoriaus išlaidos sveikatos apsaugai (GF09),",last_year), 
             subtitle="Šaltinis: Eurostat (gov_10a_exp). Skaičiavimai: Lithuanian-Economy.net", 
             x="% BVP", 
             y="% visų išlaidų")

dev.off()


df <- gov_10a_taxag %>% 
        filter(geo %in% c("EU27_2020", "BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK"),
               na_item=="D2_D5_D91_D61_M_D995",
               sector=="S13",
               year(time)==2018,
               unit=="PC_GDP")

last_year <-   strftime(max(df$time),"%Y") 


png("./figures/tax_to_GDP_ratio.png", width = 9, height = 5, units="in", res=200)
ggplot(data=df %>% filter(geo!="EU27_2020"), aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=values), vjust=-0.5, size=2.5)+
        labs(title=paste("Mokesčių ir BVP santykis, %, ",last_year,"m."), 
             subtitle="Šaltinis: Eurostat (gov_10a_taxag). Skaičiavimai: Lithuanian-Economy.net", 
             x="Šalys", 
             y="% BVP")+
        geom_hline(yintercept = df$values[df$geo=="EU27_2020"])+
        annotate("text", 3, df$values[df$geo=="EU27_2020"], vjust = -1, label = "EU-27")
dev.off()
















# General government expenditure on education vs GDP size  ====

df_gov_ex_07 <- gov_10a_exp %>%
        filter(
                cofog99=="GF09", 
                sector=="S13",
                na_item=="TE",
                geo %in% c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK","IS","LI","NO","CH"),
                year(time)=="2017",
                unit=="PC_GDP")%>%
        select(geo,time, values)%>%
        rename(exp_educ=values)
last_year <-   strftime(max(df_gov_ex_07$time),"%Y")

df_nama_gdp <- nama_10_gdp %>% 
        filter(
                geo %in% c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK","IS","LI","NO","CH"),
                                      year(time)=="2017",
                na_item=="B1GQ",
                unit=="CP_MEUR")%>%
        select(geo, values)%>%
        rename(GDP=values)

unite <- full_join(df_gov_ex_07,df_nama_gdp, by="geo")%>% na.omit()

png("./figures/exp_educ_gdp_size.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=unite, aes(x=log(GDP), y=exp_educ))+
        geom_point()+
        geom_text(aes(label=geo), hjust=1, vjust=1.5, size=4)+
        geom_smooth(method="lm", se = FALSE)+
        labs(title=paste("Expenditure on education and log(size of economy),",last_year), 
             subtitle="Source: Eurostat (gov_10a_exp,nama_10_gdp), calculations: Lithuanian-Economy.net", 
             x="LOG(GDP)", 
             y="Expenditure on education % GDP")

dev.off()




# General government expenditure on education, GF0901, BAR ====

df_gov_ex_0701 <- gov_10a_exp %>%
        filter(cofog99 %in% c("GF0901"),
               na_item=="TE",
               geo %in% c("EU27_2020","BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK","IS","LI","NO","CH"),
               sector=="S13",
               year(time)=="2017",
               unit=="PC_GDP")

last_year <-   strftime(max(df_gov_ex_0701$time),"%Y") 

png("./figures/general_government_expenditure_on_education_bar_gdp_0901.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df_gov_ex_0701, aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=round(values,1)), vjust=-0.5, size=3)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title=paste("General government expenditure on pre-primary and primary education (GF0901),",last_year), 
             subtitle="Source: Eurostat (gov_10a_exp), calculations: Lithuanian-Economy.net", 
             x="Countries", 
             y="% GDP") 
dev.off()

# General government expenditure on education, GF0902, BAR ====

df_gov_ex_0702 <- gov_10a_exp %>%
        filter(cofog99 %in% c("GF0902"),
               na_item=="TE",
               geo %in% c("EU27_2020","BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK","IS","LI","NO","CH"),
               sector=="S13",
               year(time)=="2017",
               unit=="PC_GDP")

last_year <-   strftime(max(df_gov_ex_0702$time),"%Y") 

png("./figures/general_government_expenditure_on_education_bar_gdp_0902.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df_gov_ex_0702, aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=round(values,1)), vjust=-0.5, size=3)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title=paste("General government expenditure on secondary education (GF0902),",last_year), 
             subtitle="Source: Eurostat (gov_10a_exp), calculations: Lithuanian-Economy.net", 
             x="Countries", 
             y="% GDP") 
dev.off()

# Income quintile share ratio S80/S20 for disposable income, BAR ====

df_ilc_di11 <- ilc_di11 %>% filter(age=="Y_LT65",
                          geo %in% c("EU27_2020","BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK","IS","LI","NO","CH"),
                          sex=="T",
                          year(time)=="2018")

last_year <-   strftime(max(df_ilc_di11$time),"%Y") 

png("./figures/S80S20_bar_EU.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df_ilc_di11, aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=round(values,1)), vjust=-0.5, size=3)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title=paste("Income quintile share ratio S80/S20 for disposable income (age under 65),",last_year), 
             subtitle="Source: Eurostat (ilc_di11), calculations: Lithuanian-Economy.net", 
             x="Countries", 
             y="S80/S20 ratio") 
dev.off()



# Income quintile share ratio S80/S20 for disposable income, time series ====

df_ilc_di11 <- ilc_di11 %>% filter(age=="Y_LT65",
                                   geo %in% c("LT", "FI", "LV","EE", "PL"),
                                   year(time)>="2005",
                                   sex=="T")

png("./figures/S80S20_timeseries.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df_ilc_di11, aes(x=time, y=values, col=geo))+
        geom_line(size=1.2)+
        scale_colour_brewer(palette="Set1")+
        scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
        scale_y_continuous(breaks = seq(0,10,by=0.5))+
        labs(title="Income quintile share ratio S80/S20 for disposable income (age under 65)", 
             subtitle="Source: Eurostat (ilc_di11), calculations: Lithuanian-Economy.net", 
             x="Year", 
             y="S80/S20 ratio") 
dev.off()

#### Income quintile share ratio S80/S20 for net market income, BAR ====

df_ilc_di11 <- ilc_di11b %>% filter(age=="Y_LT65",
                                   geo %in% c("EU","BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK","IS","LI","NO","CH"),
                                   sex=="T",
                                   year(time)=="2018")

last_year <-   strftime(max(df_ilc_di11$time),"%Y") 

png("./figures/S80S20_net_market_bar_EU.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df_ilc_di11, aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=round(values,1)), vjust=-0.5, size=3)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title=paste("Income quintile share ratio S80/S20 for net market income (age under 65),",last_year), 
             subtitle="Source: Eurostat (ilc_di11b), calculations: Lithuanian-Economy.net", 
             x="Countries", 
             y="S80/S20 ratio") 
dev.off()

# Teachers' statutory salaries % AVG ====

# COM:  Pre-primary education (ISCED2011 level 0)
df_1 <- EAG_TS_STA %>% 
        filter(YEAR=="2018",
               ISC11=="L0",
               EXP=="EXP0",
               INDICATOR=="NAT_STAT_YR",
               QUAL=="QUAL_TYP")%>%
        select(COUNTRY, YEAR, obsValue)%>%
        rename(salaries=obsValue)

last_year <-   max(df_1$YEAR)

df_2 <- AV_AN_WAGE %>%
        filter(obsTime=="2018",
               SERIES=="CNPNCU")%>%
        select(COUNTRY, obsValue)%>%
        rename(avg=obsValue)

df_l0 <- full_join(df_1, df_2, by="COUNTRY")%>%
        mutate(values=round((salaries/avg)*100),0)%>%
        select(COUNTRY, values)%>%
        na.omit()%>%
        rename(geo=COUNTRY)

rm(list = c("df_1", "df_2"))

png("./figures/EAG_TS_STA_l0.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df_l0, aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=round(values,1)), vjust=-0.5, size=2.5)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title=paste("Annual statutory salaries of pre-primary education teachers (starting sallary) as % of average wage",last_year), 
             subtitle="Source: OECD (EAG_TS_STA, AV_AN_WAGE), calculations: Lithuanian-Economy.net", 
             x="Countries", 
             y="% average wage") 
dev.off()


# Primary education (ISCED2011 level 1)
df_1 <- EAG_TS_STA %>% 
        filter(YEAR=="2018",
               ISC11=="L1",
               EXP=="EXP0", #starting sallary
               INDICATOR=="NAT_STAT_YR",
               QUAL=="QUAL_TYP")%>%
        select(COUNTRY, YEAR, obsValue)%>%
        rename(salaries=obsValue)

last_year <-   max(df_1$YEAR)

df_2 <- AV_AN_WAGE %>%
        filter(obsTime=="2018",
               SERIES=="CNPNCU")%>%
        select(COUNTRY, obsValue)%>%
        rename(avg=obsValue)

df_l1 <- full_join(df_1, df_2, by="COUNTRY")%>%
        mutate(values=round((salaries/avg)*100),0)%>%
        select(COUNTRY, values)%>%
        na.omit()%>%
        rename(geo=COUNTRY)
rm(list = c("df_1", "df_2"))

png("./figures/EAG_TS_STA_l1.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df_l1, aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=round(values,1)), vjust=-0.5, size=2.5)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title=paste("Annual statutory salaries of upper primary education teachers (starting sallary) as % of average wage",last_year), 
             subtitle="Source: OECD (EAG_TS_STA, AV_AN_WAGE), calculations: Lithuanian-Economy.net", 
             x="Countries", 
             y="% average wage") 
dev.off()

# Lower secondary general education (ISCED2011 level 2 (general programmes))
df_1 <- EAG_TS_STA %>% 
        filter(YEAR=="2018",
               ISC11=="L2_C4",
               EXP=="EXP0",
               INDICATOR=="NAT_STAT_YR",
               QUAL=="QUAL_TYP")%>%
        select(COUNTRY, YEAR, obsValue)%>%
        rename(salaries=obsValue)

last_year <-   max(df_1$YEAR)

df_2 <- AV_AN_WAGE %>%
        filter(obsTime=="2018",
               SERIES=="CNPNCU")%>%
        select(COUNTRY, obsValue)%>%
        rename(avg=obsValue)

df_l2_c4 <- full_join(df_1, df_2, by="COUNTRY")%>%
        mutate(values=round((salaries/avg)*100),0)%>%
        select(COUNTRY, values)%>%
        na.omit()%>%
        rename(geo=COUNTRY)

rm(list = c("df_1", "df_2"))

png("./figures/EAG_TS_STA_l2_c4.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df_l2_c4, aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=round(values,1)), vjust=-0.5, size=2.5)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title=paste("Annual statutory salaries of lower secondary general education teachers (starting sallary) as % of average wage",last_year), 
             subtitle="Source: OECD (EAG_TS_STA, AV_AN_WAGE), calculations: Lithuanian-Economy.net", 
             x="Countries", 
             y="% average wage") 
dev.off()



# Upper secondary general education (ISCED2011 level 3 (general programmes))

df_1 <- EAG_TS_STA %>% 
        filter(YEAR=="2018",
               ISC11=="L3_C4",
               EXP=="EXP0",
               INDICATOR=="NAT_STAT_YR",
               QUAL=="QUAL_TYP")%>%
        select(COUNTRY, YEAR, obsValue)%>%
        rename(salaries=obsValue)

last_year <-   max(df_1$YEAR)

df_2 <- AV_AN_WAGE %>%
        filter(obsTime=="2018",
               SERIES=="CNPNCU")%>%
        select(COUNTRY, obsValue)%>%
        rename(avg=obsValue)

df_l3_c4 <- full_join(df_1, df_2, by="COUNTRY")%>%
        mutate(values=round((salaries/avg)*100),0)%>%
        select(COUNTRY, values)%>%
        na.omit()%>%
        rename(geo=COUNTRY)

rm(list = c("df_1", "df_2"))

png("./figures/EAG_TS_STA_l3_c4.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df_l3_c4, aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=round(values,1)), vjust=-0.5, size=2.5)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title=paste("Annual statutory salaries of upper secondary general education teachers (starting sallary) as % of average wage",last_year), 
             subtitle="Source: OECD (EAG_TS_STA, AV_AN_WAGE), calculations: Lithuanian-Economy.net", 
             x="Countries", 
             y="% average wage") 
dev.off()



####  S80/S20 OECD database ====

df_s80_s20 <-  IDD %>%
        filter(MEASURE=="S80S20",
               AGE=="WA",
               DEFINITION=="CURRENT",
               METHODO=="METH2012",
               obsTime=="2016")%>%
        select(LOCATION,obsTime, obsValue)%>%
        rename(geo=LOCATION,
               values=obsValue)

last_year <-   max(df_s80_s20$obsTime)

png("./figures/IDD_S80_S20.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df_s80_s20, aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=round(values,1)), vjust=-0.5, size=2.5)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title=paste("Income quintile share ratio S80/S20 of working age population (18-65),",last_year), 
             subtitle="Source: OECD (IDD), Calculations: Lithuanian-Economy.net", 
             x="Countries", 
             y="S80/S20 ratio") 
dev.off()

####  S80/S20 vs teachers' salaries ====

df <- full_join(df_l2_c4%>%
                        rename(rel_inc=values),
                df_s80_s20%>%
                        rename(ineq=values), by="geo")%>%
        select(-obsTime)%>% na.omit()

png("./figures/S80_S20_teach_sal.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=df, aes(x=ineq, y=rel_inc))+
        geom_point()+
        geom_text(aes(label=geo), hjust=1, vjust=1.5, size=3)+ 
        geom_smooth(method='lm', formula= y~x, se=FALSE)+
        labs(title=paste("Income quintile share ratio S80/S20 vs. secondary general education teachers' share of average income, 2018"), 
             subtitle="Source: OECD (IDD, AV_AN_WAGE, EAG_TS_STA ), calculations: Lithuanian-Economy.net", 
             x="Income quintile share ratio S80/S20 (2016)", 
             y="Salary as percent of AVG wage (2018)")
dev.off()


#### Teacher and public sector employees' income vs AVG ====
df_1 <- S3R0050_M3060315_1 %>%
        filter(Ekon_sektorM2040803==1,
               Lytis==0,
               darboM3060321=="bruto",
               EVRK2M3060207 %in% c("TOTAL", "O84", "P85", "Q86", "Q87_Q88"))%>%
        mutate(time=parse_date_time(LAIKOTARPIS, "y q"))%>%
        select(EVRK2M3060207_label.en, time, obsValue)%>%
        spread(EVRK2M3060207_label.en, obsValue)

df_2 <- S3R0050_M3060320_1 %>%
        filter(darboM3060321=="bruto")%>%
        mutate(time=parse_date_time(LAIKOTARPIS, "y q"))%>%
        select(EVRKM3060213_label.en, time, obsValue)%>%
        spread(EVRKM3060213_label.en, obsValue)

df_3 <- S3R0050_M3060322%>%
        filter(darboM3060321=="bruto",
               savivaldybesRegdb=="00",
               Ekon_sektoriusM3061118=="0ex",
               Lytis==0)%>%
        mutate(time=parse_date_time(LAIKOTARPIS, "y q"))%>%
        select(time, obsValue)%>%
        rename(AVG=obsValue)

df <- full_join(df_3, df_2, by="time")
df <- full_join(df, df_1, by="time")


df_x <- df
df_x[,3] <- df[,3]/df[,2]
df_x[,4] <- df[,4]/df[,2]
df_x[,5] <- df[,5]/df[,2]
df_x[,6] <- df[,6]/df[,2]
df_x[,7] <- df[,7]/df[,2]
df_x[,8] <- df[,8]/df[,2]
df_x[,9] <- df[,9]/df[,2]


sal_data <- df_x %>%
        na.omit()%>%
        select(-AVG)%>%
        gather(sector, values, 2:8)%>%
        arrange(time)

png("./figures/public_vs_AVG.png", width = 12, height = 6, units = 'in', res = 200)
ggplot(data=sal_data, aes(x=as.Date(time), y=values, col=sector))+
        geom_line(size=1.2)+
        scale_colour_brewer(palette="Set1")+
        scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
        scale_y_continuous(breaks = seq(0,10,by=0.1))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title="Public sector average sallaries as percentage of average income in Lithuania", 
             subtitle="Source: LSD (S3R0050_M3060315_1 S3R0050_M3060320_1 S3R0050_M3060322), calculations: Lithuanian-Economy.net", 
             x="Year", 
             y="Percentage of averga income") 
dev.off()

rm(list = c("df_1", "df_2", "df_3", "df_x", "df"))


# this part for table generation in BLOG:
#
# kable(sal_data %>% filter(as.numeric(time) %in% c(1514764800,1561939200))%>%
#        mutate(values=round(values,2))%>%
#        spread(time, values)%>%
#        mutate(Change=round(((.[,3]/.[,2]-1)*100),1)),
# format = "html")

