library(tidyverse)
df <- read.csv("bd_svki.csv")
df <- df%>%
  mutate(BD_indeksas=BD_euro/BD_euro[1],
         SVKI_indeksas=SVKI_index/SVKI_index[1])%>%
  select(1,4,5)%>%
  gather(var, values, 2:3)

png("bd_svki_indeksai.png", width = 9, height = 4, units = 'in', res = 200)
ggplot(df, aes(time, values, col=var))+
  geom_line(size=1.1)+
  geom_point(size=2.2)+
  scale_color_manual(name="Kintamieji",
                     values=c("red", "blue"))+
  scale_x_continuous(breaks=seq(from=2005, to=2025, by=1))+
  labs(title="Bazinio dydžio (BD) ir infliacijos (SVKI) raida nuo 2006 metų",
       x="Laikotarpis",
       y="Indeksas (2006=1)")+ 
  theme(legend.position="bottom")
dev.off()

