library(eurostat)
library(tidyverse)

lc_lci_lev <- get_eurostat("lc_lci_lev", stringsAsFactors = FALSE)


df <- lc_lci_lev%>%
        filter(geo %in% c("LT", "DE"),
               lcstruct=="D1_D4_MD5",
               nace_r2 %in% c("C", "F", "H","J", "K"),
               unit=="EUR")%>%
        group_by(nace_r2)%>%
        spread(geo, values)%>%
        mutate(values=round(LT/DE*100,1))

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
