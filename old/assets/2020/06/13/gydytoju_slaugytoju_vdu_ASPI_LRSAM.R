library(tidyverse)


data <- read.csv("Zemelapis_ASPI.csv",
                 stringsAsFactors = FALSE,
                 header = TRUE)

df <- data%>%filter(Trec_slaug >0)
df %>% group_by(ASPI_grupe)%>%summarise(med=median(Trec_slaug))
png("./slaugytoju_vdu.png", width = 9, height = 6, units = 'in', res = 200)
ggplot(data=data, aes(x=ASPI_grupe, y=Trec_slaug))+
        geom_boxplot()+
        geom_hline(yintercept = 1625, col="red")+
        coord_flip()+
        labs(title="Vidutinis etatinis atlyginimas pagal ASPI grupę",
             subtitle="Šaltinis: LR SAM, Skaičiavimai:Lithuanian-Economy.net",
             x="ASPI grupė",
             y="Eurai")
dev.off()


df <- data%>%filter(Trec_gyd >0)
df %>% group_by(ASPI_grupe)%>%summarise(med=median(Trec_gyd))
png("./gydytoju_vdu.png", width = 9, height = 6, units = 'in', res = 200)
ggplot(data=df, aes(x=ASPI_grupe, y=Trec_gyd))+
        geom_boxplot()+
        geom_hline(yintercept = 3250, col="red")+
        coord_flip()+
        labs(title="Vidutinis etatinis atlyginimas pagal ASPI grupę",
             subtitle="Šaltinis: LR SAM, Skaičiavimai:Lithuanian-Economy.net",
             x="ASPI grupė",
             y="Eurai")
dev.off()