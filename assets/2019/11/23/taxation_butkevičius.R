if(!require(tidyverse)) install.packages("tidyverse");require(tidyverse)  
if(!require(knitr)) install.packages("knitr");require(knitr)
if(!require(kableExtra)) install.packages("kableExtra");require(kableExtra)


url <- paste("https://raw.githubusercontent.com/justasmundeikis/",
             "research/master/GPMvertinimas2019/apdraustuju_pajamu_analize.csv",
             sep="")
data <- read.csv(url, 
                 sep=";", 
                 header = TRUE, 
                 stringsAsFactors = FALSE, 
                 fileEncoding = "ISO-8859-13")
# global parameters: 

# duomenų filtravimas
data_f <- data %>% 
  filter(Amžius>=14,
         Mėnesio.pajamos>=55.5)
# observacijų be duomenų pašalinimas
data_f <-na.omit(data_f)




base_2019 <- function(x, index=1) {
  # egzogeniniai parametrai
  GPM_1 <- 0.20
  GPM_2 <- 0.27
  VSD <- 0.1252 
  PSD <- 0.0698 
  DVSD <- 0.0177
  MMA <- 555
  NPD <- 300
  NPD_coef <- 0.15
  VDU <- 1290
  lubos <- 12*VDU
  bruto <- x*index
  # skaičiavimo dalis
  npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
  gpm_baz <- pmax(0,(bruto-npd))
  gpm <- ifelse(bruto<=lubos, gpm_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
  sodra <- pmin(bruto*VSD, lubos * VSD)
  psd <- bruto*PSD
  neto <- bruto - gpm - sodra - psd
  dvsd <- ifelse(bruto<MMA, MMA*DVSD + (MMA-bruto)*(VSD+PSD),bruto*DVSD)
  dvk <- bruto+dvsd
  tax=dvk-neto
  # list objekto sukūrimas
  list(bruto=bruto, 
       neto = neto,
       ITR=((dvk-neto)/dvk),
       tax=tax,
       marg=(tax-lag(tax,1))/100)
} 

base_2020 <- function(x, index=1.074) {
  # egzogeniniai parametrai
  GPM_1 <- 0.20
  GPM_2 <- 0.27
  VSD <- 0.1252 
  PSD <- 0.0698 
  DVSD <- 0.0177
  MMA <- 607
  NPD <- 400
  NPD_coef <- 0.2
  VDU <- 1386.00
  lubos <- 6*VDU
  bruto <- x*index
  # skaičiavimo dalis
  npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
  gpm_baz <- pmax(0,(bruto-npd))
  gpm <- ifelse(bruto<=lubos, gpm_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
  sodra <- pmin(bruto*VSD, lubos * VSD)
  psd <- bruto*PSD
  neto <- bruto - gpm - sodra - psd
  dvsd <- ifelse(bruto<MMA, MMA*DVSD + (MMA-bruto)*(VSD+PSD),bruto*DVSD)
  dvk <- bruto+dvsd
  tax=dvk-neto
  # list objekto sukūrimas
  list(bruto=bruto, 
       neto = neto,
       ITR=((dvk-neto)/dvk),
       tax=tax,
       marg=(tax-lag(tax,1))/100)
} 

base_2021 <- function(x, index=1.136292) {
  # egzogeniniai parametrai
  GPM_1 <- 0.20
  GPM_2 <- 0.27
  VSD <- 0.1252 
  PSD <- 0.0698 
  DVSD <- 0.0177
  MMA <- 651.92
  NPD <- 500
  NPD_coef <- 0.23
  VDU <- 1466.00
  lubos <- 5*VDU
  bruto <- x*index
  # skaičiavimo dalis
  npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
  gpm_baz <- pmax(0,(bruto-npd))
  gpm <- ifelse(bruto<=lubos, gpm_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
  sodra <- pmin(bruto*VSD, lubos * VSD)
  psd <- bruto*PSD
  neto <- bruto - gpm - sodra - psd
  dvsd <- ifelse(bruto<MMA, MMA*DVSD + (MMA-bruto)*(VSD+PSD),bruto*DVSD)
  dvk <- bruto+dvsd
  tax=dvk-neto
  # list objekto sukūrimas
  list(bruto=bruto, 
       neto = neto,
       ITR=((dvk-neto)/dvk),
       tax=tax,
       marg=(tax-lag(tax,1))/100)
} 

lvzs_2020 <- function(x, index=1.074) {
  # egzogeniniai parametrai
  GPM_1 <- 0.20
  GPM_2 <- 0.32
  VSD <- 0.1252 
  PSD <- 0.0698 
  DVSD <- 0.0177
  MMA <- 607
  NPD <- 350
  NPD_coef <- 0.2
  VDU <- 1386.00
  lubos <- 6*VDU
  bruto <- x*index
  # skaičiavimo dalis
  npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
  gpm_baz <- pmax(0,(bruto-npd))
  gpm <- ifelse(bruto<=lubos, gpm_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
  sodra <- pmin(bruto*VSD, lubos * VSD)
  psd <- bruto*PSD
  neto <- bruto - gpm - sodra - psd
  dvsd <- ifelse(bruto<MMA, MMA*DVSD + (MMA-bruto)*(VSD+PSD),bruto*DVSD)
  dvk <- bruto+dvsd
  tax=dvk-neto
  # list objekto sukūrimas
  list(bruto=bruto, 
       neto = neto,
       ITR=((dvk-neto)/dvk),
       tax=tax,
       marg=(tax-lag(tax,1))/100)
} 



lvzs_2021 <- function(x, index=1.136292) {
  # egzogeniniai parametrai
  GPM_1 <- 0.20
  GPM_2 <- 0.32
  VSD <- 0.1252 
  PSD <- 0.0698 
  DVSD <- 0.0177
  MMA <- 651.92
  NPD <- 400
  NPD_coef <- 0.23
  VDU <- 1466.00
  lubos <- 5*VDU
  bruto <- x*index
  # skaičiavimo dalis
  npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
  gpm_baz <- pmax(0,(bruto-npd))
  gpm <- ifelse(bruto<=lubos, gpm_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
  sodra <- pmin(bruto*VSD, lubos * VSD)
  psd <- bruto*PSD
  neto <- bruto - gpm - sodra - psd
  dvsd <- ifelse(bruto<MMA, MMA*DVSD + (MMA-bruto)*(VSD+PSD),bruto*DVSD)
  dvk <- bruto+dvsd
  tax=dvk-neto
  # list objekto sukūrimas
  list(bruto=bruto, 
       neto = neto,
       ITR=((dvk-neto)/dvk),
       tax=tax,
       marg=(tax-lag(tax,1))/100)
} 



butk_2020 <- function(x, index=1.074) {
  # egzogeniniai parametrai
  GPM_1 <- 0.20
  GPM_2 <- 0.32
  VSD <- 0.1252 
  PSD <- 0.0698 
  DVSD <- 0.0177
  MMA <- 607
  NPD <- 400
  NPD_coef <- 0.27
  VDU <- 1386.00
  lubos <- 6*VDU
  bruto <- x*index
  # skaičiavimo dalis
  npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
  gpm_baz <- pmax(0,(bruto-npd))
  gpm <- ifelse(bruto<=lubos, gpm_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
  sodra <- pmin(bruto*VSD, lubos * VSD)
  psd <- bruto*PSD
  neto <- bruto - gpm - sodra - psd
  dvsd <- ifelse(bruto<MMA, MMA*DVSD + (MMA-bruto)*(VSD+PSD),bruto*DVSD)
  dvk <- bruto+dvsd
  tax=dvk-neto
  # list objekto sukūrimas
  list(bruto=bruto, 
       neto = neto,
       ITR=((dvk-neto)/dvk),
       tax=tax,
       marg=(tax-lag(tax,1))/100)
} 


butk_2021 <- function(x, index=1.136292) {
  # egzogeniniai parametrai
  GPM_1 <- 0.20
  GPM_2 <- 0.32
  VSD <- 0.1252 
  PSD <- 0.0698 
  DVSD <- 0.0177
  MMA <- 651.92
  NPD <- 500
  NPD_coef <- 0.39
  VDU <- 1466.00
  lubos <- 5*VDU
  bruto <- x*index
  # skaičiavimo dalis
  npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
  gpm_baz <- pmax(0,(bruto-npd))
  gpm <- ifelse(bruto<=lubos, gpm_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
  sodra <- pmin(bruto*VSD, lubos * VSD)
  psd <- bruto*PSD
  neto <- bruto - gpm - sodra - psd
  dvsd <- ifelse(bruto<MMA, MMA*DVSD + (MMA-bruto)*(VSD+PSD),bruto*DVSD)
  dvk <- bruto+dvsd
  tax=dvk-neto
  # list objekto sukūrimas
  list(bruto=bruto, 
       neto = neto,
       ITR=((dvk-neto)/dvk),
       tax=tax,
       marg=(tax-lag(tax,1))/100)
} 




################################################################################
### Gini
################################################################################

GINI<-function(x){
  x <- sort(x)
  n <- length(x)
  G <- (2*sum(x*1:n))/(n*sum(x)) - (n+1)/n
  G*100
}

base_2019_g=GINI(base_2019(data_f$Mėnesio.pajamos)$neto)
base_2020_g=GINI(base_2020(data_f$Mėnesio.pajamos)$neto)
lvzs_2020_g=GINI(lvzs_2020(data_f$Mėnesio.pajamos)$neto)
butk_2020_g=GINI(butk_2020(data_f$Mėnesio.pajamos)$neto)

data_gini <- data.frame(
  row.names = c("base_2019","base_2020", "lvzs_2020", "butk_2020"),
  "2020"=c(base_2019_g, base_2020_g,lvzs_2020_g,  butk_2020_g)
)
colnames(data_gini) <- c("Gini indeksas")

options(knitr.kable.NA = '')

write(
  kable(data_gini, 
        booktabs = T,
        format = "html" ,
        digits = 2, 
        caption = "Neto pajamų Gini koeficientai"),
  "./table_1.txt")


################################################################################
### Mokestinės įplaukos
################################################################################

b20= sum(base_2020(data_f$Mėnesio.pajamos)$tax)*12/1000000
l20= sum(lvzs_2020(data_f$Mėnesio.pajamos)$tax)*12/1000000
a20= sum(butk_2020(data_f$Mėnesio.pajamos)$tax)*12/1000000


data_budget <- data.frame(
  row.names = c("mln. eur","dif. 2020_base", "dif. 2020_lvzs"),
  base_2020=c(b20,NA,NA),
  lvzs_2020=c(l20, l20-b20, NA),
  butk_2020=c(a20,a20-b20, a20-l20)
)

options(knitr.kable.NA = '')

write(
  kable(data_budget, 
        booktabs = T,
        format = "html" ,
        digits = 2, 
        caption = "Mokestinių įplaukų skirtumas"),
  "./table_2.txt")


################################################################################
### Buto neto
################################################################################

x <- c(seq(100, 3000, by=100))
data_neto <- data.frame(bruto=x,
                        base_2019=base_2019(x,1)$neto,
                        base_2020=base_2020(x,1)$neto,
                        lvzs_2020=lvzs_2020(x,1)$neto,
                        butk_2020=butk_2020(x,1)$neto,
                        butk_vs_lvzs=butk_2020(x,1)$neto-lvzs_2020(x,1)$neto)
write(
  kable(data_neto, 
        digits = 0, 
        booktabs = T,
        format = "html" ,
        caption = "Bruto - neto skirtingais scenarijais"),
  "./table_3.txt")


################################################################################

################# 1

x <- seq(500, 18000, by=10)
df_itr <- data.frame(bruto=x,
                     base_ITR_2019=base_2019(x,1)$ITR,
                     base_ITR_2020=base_2020(x,1)$ITR,
                     lvzs_ITR_2020=lvzs_2020(x,1)$ITR,
                     butk_ITR_2020=butk_2020(x,1)$ITR)%>%
  gather(var, values,2:length(.))

png("./ITR_1.png", width = 9, height = 5, units = 'in', res = 200)

ggplot(data=df_itr, aes(x=bruto,y=values, color=var))+
  geom_line()+
  scale_color_manual(values=c("#0000FF","#000000","#FF0000", "#00FF00"))+
  scale_x_continuous(breaks = seq(0,18000, by=1000))+
  scale_y_continuous(breaks = seq(0,.8, by=0.02))+
  labs(title="Efektyvus mokesčių tarifas (ITR)",
       subtitle="Skaičiavimai: Lithuanian-Economy.net",
       x="Pajamos, eurai", 
       y="ITR, proc.")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.0, face="bold"),
        axis.text.x = element_text(angle=45, hjust=1))

dev.off()


################# 2

x <- seq(500, 2750, by=10)
df_itr <- data.frame(bruto=x,
                     butk_ITR_2020=butk_2020(x,1)$ITR,
                     base_ITR_2019=base_2019(x,1)$ITR,
                     base_ITR_2020=base_2020(x,1)$ITR,
                     lvzs_ITR_2020=lvzs_2020(x,1)$ITR
)%>%
  gather(var, values,2:length(.))

png("./ITR_2.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(data=df_itr, aes(x=bruto,y=values, color=var))+
  geom_line(size=1.02)+
  scale_color_manual(values=c("#0000FF","#000000","#FF0000", "#00FF00"))+
  scale_x_continuous(breaks = seq(0,18000, by=100))+
  scale_y_continuous(breaks = seq(0,.8, by=0.01))+
  labs(title="Efektyvus mokesčių tarifas (ITR)",
       subtitle="Skaičiavimai: Lithuanian-Economy.net",
       x="Pajamos, eurai", 
       y="ITR, proc.")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.0, face="bold"),
        axis.text.x = element_text(angle=45, hjust=1))

dev.off()


################################################################################
### Ribinės mokesčių normos
################################################################################

################# 1

x <- seq(0, 16000, by=10)
df_marg <- data.frame(bruto=x,
                      base_2019=base_2019(x,1)$marg*10,
                      base_2020=base_2020(x,1)$marg*10,
                      lvzs_2020=lvzs_2020(x,1)$marg*10,
                      butk_2020=butk_2020(x,1)$marg*10
)%>%
  gather(var, values,2:length(.))

png("./marg_normos_1.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(data=df_marg, aes(x=bruto,y=values, color=var))+
  geom_line(size=1.1)+
  scale_color_manual(values=c("#000000","#0000FF","#FF0000", "#00FF00"))+
  scale_x_continuous(breaks = seq(0,16000, by=500))+
  scale_y_continuous(breaks = seq(0, .8, by=0.1))+
  labs(title="Ribinė mokesčių norma, proc.", 
       subtitle="Skaičiavimai: Lithuanian-Economy.net",
       x="Pajamos, eurai",
       y="Ribinė mokesčių norma, proc.")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.0, face="bold"),
        axis.text.x = element_text(angle=45, hjust=1))

dev.off()

################# 2

x <- seq(0, 3000, by=10)
df_marg <- data.frame(bruto=x,
                      base_2019=base_2019(x,1)$marg*10,
                      base_2020=base_2020(x,1)$marg*10,
                      lvzs_2020=lvzs_2020(x,1)$marg*10,
                      butk_2020=butk_2020(x,1)$marg*10
)%>%
  gather(var, values,2:length(.))

png("./marg_normos_2.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(data=df_marg, aes(x=bruto,y=values, color=var))+
  geom_line(size=1.1)+
  scale_color_manual(values=c("#000000","#0000FF","#FF0000", "#00FF00"))+
  scale_x_continuous(breaks = seq(0,16000, by=500))+
  scale_y_continuous(breaks = seq(0, .8, by=0.1))+
  labs(title="Ribinė mokesčių norma, proc.", 
       subtitle="Skaičiavimai: Lithuanian-Economy.net",
       x="Pajamos, eurai",
       y="Ribinė mokesčių norma, proc.")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.0, face="bold"),
        axis.text.x = element_text(angle=45, hjust=1))

dev.off()






################################################################################
### Taikomas NPD
################################################################################

npd_2019 <- function(x, index=1) {
  # egzogeniniai parametrai
  MMA <- 555
  NPD <- 300
  NPD_coef <- 0.15
  bruto <- x*index
  # skaičiavimo dalis
  npd <- pmax(NPD - NPD_coef*pmax(0,(bruto - MMA)),0)
  gpm_baz <- pmax(0,(bruto-npd))
  # list objekto sukūrimas
  list(bruto=bruto,
       npd=npd,
       gpm_baz=gpm_baz)
} 

x <- seq(0, 3000, by=1)
taikomas_npd <- data.frame(bruto=x,
                           butk_marg_2020=npd_2019(x,1)$npd
)%>%
  gather(var, values,2:length(.))

png("./taikomas_npd_2019.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(data=taikomas_npd, aes(x=bruto,y=values))+
  geom_point()+
  scale_x_continuous(breaks = seq(0,3000, by=300))+
  scale_y_continuous(breaks = seq(0,400, by=50))+
  labs(title="Taikomas NPD, 2019 m.", 
       subtitle="Skaičiavimai: Lithuanian-Economy.net",
       x="Pajamos, eurai",
       y="Taikomas NPD, eurai.")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.0, face="bold"),
        axis.text.x = element_text(angle=45, hjust=1))

dev.off()

################################################################################
### Procentinė nauda
################################################################################
x <- seq(100, 3000, by=10)
df_neto <- data.frame(bruto=x,
                     base_neto_2019=base_2019(x,1)$neto,
                     butk_neto_2020=butk_2020(x,1)$neto,
                     butk_neto_2021=butk_2021(x,1)$neto)%>%
  mutate(butk_2020p=round(.[,3]/.[,2]-1,3),
         butk_2021p=round(.[,4]/.[,3]-1,3))%>%
  select(bruto,butk_2020p, butk_2021p) %>%
  gather(var, values,2:length(.))

png("./neto_p_change.png", width = 9, height = 5, units = 'in', res = 200)
ggplot(data=df_neto, aes(x=bruto,y=values, color=var))+
  geom_line()+
  scale_color_manual(values=c("#0000FF","#000000","#FF0000", "#00FF00"))+
  scale_x_continuous(breaks = seq(0,18000, by=100))+
  scale_y_continuous(breaks = seq(-1,.8, by=0.01))+
  labs(title="Neto altyginimų pokytis lyginant su ankstesniu periodu, proc.",
       subtitle="Skaičiavimai: Lithuanian-Economy.net",
       x="Pajamos, eurai", 
       y="Neto pokytis, proc.")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.0, face="bold"),
        axis.text.x = element_text(angle=45, hjust=1))

dev.off()
