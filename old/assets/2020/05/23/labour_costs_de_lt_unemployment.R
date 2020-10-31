library(eurostat)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

lc_lci_lev <- get_eurostat("lc_lci_lev", stringsAsFactors = FALSE)
une_rt_q <- get_eurostat("une_rt_q", stringsAsFactors = FALSE)


df <- lc_lci_lev%>%
        filter(geo %in% c("LT", "DE"),
               lcstruct=="D1_D4_MD5",
               nace_r2 %in% c("C", "F", "H","J"),
               unit=="EUR")%>%
        group_by(nace_r2)%>%
        spread(geo, values)%>%
        mutate(values=round(LT/DE*100,1))


#        filter(year(time)>=2018)%>%
#        select(nace_r2, values, time)%>%
#        spread(time,values)

png("./labour_costs_lt_de.png", width = 9, height = 6, units = 'in', res = 200)
ggplot(df, aes(time, values))+
        geom_bar(stat="identity",
                 fill="steelblue")+
        facet_grid(rows = vars(nace_r2))+
        scale_x_date(breaks = "1 year", date_labels = "%Y")+
        scale_y_continuous(limits = c(0,45))+
        geom_text(aes(label=values), vjust=-0.5, size=2.5)+
        labs(title=paste("Labour cost in different sectors in Lithuania compared to Germany, %"), 
             subtitle="Source: Eurostat (lc_lci_lev), calculations: Lithuanian-Economy.net", 
             x="Time", 
             y="Percent")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()




df <- une_rt_q%>%
        filter(age %in% c("Y15-74","Y25-54","Y55-74"),
               geo %in% c("LT", "LV"),
               sex %in% c("M", "F"),
               year(time)>=2015,
               s_adj=="SA",
               unit=="PC_ACT")%>%
        label_eurostat()

png("./unemployment_lt.png", width = 9, height = 6, units = 'in', res = 200)
ggplot(df, aes(x=time, y=values, col=geo))+
        geom_line(size=1.2)+
        scale_color_brewer(palette = "Set1", type = "qual")+
        facet_grid(age~sex, scales = "free")+
        labs(title=paste("Unemployment in different countries by sex and age, seasionally adjusted, %"), 
             subtitle="Source: Eurostat (une_rt_q), calculations: Lithuanian-Economy.net", 
             x="Time", 
             y="Percent")+
        theme(legend.position = "bottom")
dev.off()
