###############################################################################################################################
#                                                    Loading packages
###############################################################################################################################
if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(eurostat)) install.packages("eurostat"); library(eurostat)
if(!require(lubridate)) install.packages("lubridate"); library(lubridate)
if(!require(RColorBrewer)) install.packages("RColorBrewer"); library(RColorBrewer)
if(!require(gridExtra)) install.packages("gridExtra"); library(gridExtra)

###############################################################################################################################
#                                                    Downloading datasets
###############################################################################################################################

#Employment by sex, age and economic activity (from 2008 onwards, NACE Rev. 2) - 1 000 
lfsa_egan2 <- get_eurostat("lfsa_egan2",stringsAsFactors=FALSE)

# Main national accounts tax aggregates 
gov_10a_taxag <- get_eurostat("gov_10a_taxag", stringsAsFactors=FALSE)



###############################################################################################################################
#                                                    DownloadDate
###############################################################################################################################
DownloadDate <- format(Sys.time(), format = "%Y_%m_%d")

###############################################################################################################################
#                                                    Chart
###############################################################################################################################

df <- lfsa_egan2 %>%
        filter(age =="Y_GE15",
               geo %in% c("EU28", "BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK"),
               nace_r2 %in% c("TOTAL","D","E", "O", "P", "Q", "R"),
               sex=="T") %>%
        spread(nace_r2, values)%>%
        replace(is.na(.), 0) %>%
        mutate(Public=rowSums(.[6:11])) %>%
        mutate(values=round((Public/TOTAL)*100,1)) %>%
        select(geo,time, values)

last_year <-   strftime(max(df$time),"%Y") 
df <- df %>% filter(year(time)==last_year)


png(paste("public_share_total_employment_DEOPQR_",DownloadDate,".png", sep=""), width = 9, height =5, units="in", res=100)
ggplot(data=df, aes(x=reorder(geo,values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=values), vjust=-0.5, size=2.5)+
        labs(title=paste("Užimtųjų dalis dirbančių D,E,O,P,Q,R sektoriuose",last_year,"m."),  
             subtitle="Šaltinis: Eurostat (lfsa_egan2). Skaičiavimai: Lithuanian-Economy.net", 
             x="Šalys", 
             y="Užimtųjų dalis, %")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "right")+
        geom_hline(yintercept = df$values[df$geo=="EU28"])+
        annotate("text", 3, df$values[df$geo=="EU28"], vjust = -1, label = "EU-28")

dev.off()

###############################################################################################################################
#                                                    Chart
###############################################################################################################################
df <- lfsa_egan2 %>%
        filter(age =="Y_GE15",
               geo %in% c("EU28", "BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK"),
               nace_r2 %in% c("TOTAL", "O", "P", "Q"),
               sex=="T") %>%
        spread(nace_r2, values)%>%
        replace(is.na(.), 0) %>%
        mutate(Public=rowSums(.[6:8])) %>%
        mutate(values=round((Public/TOTAL)*100,1)) %>%
        select(geo,time, values)
last_year <-   strftime(max(df$time),"%Y") 
df <- df %>% filter(year(time)==last_year)

png(paste("public_share_total_employment_OPQ_",DownloadDate,".png", sep=""), width = 9, height =5, units="in", res=100)
ggplot(data=df, aes(x=reorder(geo,values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=values), vjust=-0.5, size=2.5)+
        labs(title=paste("Užimtųjų dalis dirbančių O,P,Q sektoriuose",last_year,"m."),  
             subtitle="Šaltinis: Eurostat (lfsa_egan2). Skaičiavimai: Lithuanian-Economy.net", 
             x="Šalys", 
             y="Užimtųjų dalis, %")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "right")+
        geom_hline(yintercept = df$values[df$geo=="EU28"])+
        annotate("text", 3, df$values[df$geo=="EU28"], vjust = -1, label = "EU-28")

dev.off()

###############################################################################################################################
#                                                    Chart
###############################################################################################################################
df <- lfsa_egan2 %>%
        filter(age =="Y_GE15",
               geo %in% c("EU28", "BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK"),
               nace_r2 %in% c("TOTAL", "O"),
               sex=="T") %>%
        spread(nace_r2, values)%>%
        mutate(values=round((O/TOTAL)*100,1))%>%
        select(geo,time, values)
last_year <-   strftime(max(df$time),"%Y") 
df <- df %>% filter(year(time)==last_year)

png(paste("public_share_total_employment_O_",DownloadDate,".png", sep=""), width = 9, height =5, units="in", res=100)
ggplot(data=df, aes(x=reorder(geo,values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=values), vjust=-0.5, size=2.5)+
        labs(title=paste("Užimtųjų dalis dirbančių O sektoriuje",last_year,"m."),  
             subtitle="Šaltinis: Eurostat (lfsa_egan2). Skaičiavimai: Lithuanian-Economy.net", 
             x="Šalys", 
             y="Užimtųjų dalis, %")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "right")+
        geom_hline(yintercept = df$values[df$geo=="EU28"])+
        annotate("text", 3, df$values[df$geo=="EU28"], vjust = -1, label = "EU-28")

dev.off()
###############################################################################################################################
#                                                    Chart
###############################################################################################################################

df <- gov_10a_taxag %>% 
        filter(geo %in% c("EU28", "BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK"),
               na_item=="D2_D5_D91_D61_M_D995",
               sector=="S13",
               unit=="PC_GDP")

last_year <-   strftime(max(df$time),"%Y") 
df <- df %>% filter(year(time)==max(year(df$time[df$geo=="LT"])))

png(paste("tax_to_GDP_ratio_",last_year,"_",DownloadDate,".png", sep=""), width = 9, height =5, units="in", res=100)
ggplot(data=df %>% filter(geo!="EU28"), aes(x=reorder(geo, values), y=values))+
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 show.legend = FALSE,
                 fill="steelblue")+
        geom_text(aes(label=values), vjust=-0.5, size=2.5)+
        labs(title=paste("Mokesčių (D2_D5_D91_D61_M_D995) ir BVP santykis, %, ",last_year,"m."), 
             subtitle="Šaltinis: Eurostat (gov_10a_taxag). Skaičiavimai: Lithuanian-Economy.net", 
             x="Šalys", 
             y="% BVP")+
        geom_hline(yintercept = df$values[df$geo=="EU28"])+
        annotate("text", 3, df$values[df$geo=="EU28"], vjust = -1, label = "EU-28")
dev.off()


###############################################################################################################################
#                                                    Chart
###############################################################################################################################

df <- gov_10a_taxag %>% 
        filter(geo %in% c("LT", "EE", "LV", "PL"),
               na_item=="D2_D5_D91_D61_M_D995",
               sector=="S13",
               year(time)>="2000",
               unit=="PC_GDP")

png(paste("tax_to_GDP_ratio_time_series_baltics_",DownloadDate,".png", sep=""), width = 9, height =5, units="in", res=100)
ggplot(df, aes(x=time, y=values, color=geo))+
        geom_line(size=1.2)+
        scale_x_date(breaks = seq(min(df$time), max(df$time), by= "12 months"), date_labels = "%Y")+
        scale_y_continuous()+
        labs(title=paste("Mokestinių pajamų (D2_D5_D91_D61_M_D995) ir BVP santykis, %"), 
             subtitle="Šaltinis: Eurostat (gov_10a_taxag). Skaičiavimai: Lithuanian-Economy.net", 
             x="Laikotarpis", 
             y="% BVP")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "right")+
        scale_color_brewer(palette="Set1")
dev.off()