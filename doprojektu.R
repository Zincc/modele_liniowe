load("/home/zinc/studia/modele liniowe i mieszane/actionTimeScoreMath.rda")
library(foreign)
library(outliers)
library(ggplot2)
dataset <- read.spss("/home/zinc/studia/modele liniowe i mieszane/Cy6_ms_cmb_stu_qqq.sav", to.data.frame=TRUE)
dd<-dataset[c(1:50),]
atsm<-actionTimeScoreMath


#sprawdźmy normlaność rozkładu czasu
hist(log(atsm$T)) #naoczne sprawdzenie
  #testu shaprio nie można dla zbiorów większych niż 5000 więc można po próbkowac parę razy zbiory o wielkości 5000 
  t_sample<-sample(atsm$T, 5000) #próbkowanie
  shapiro.test(t_sample) #widać ze jest normalne

#usunmy wiersze z "-1" w position i "NA" w T
atsm<- atsm[!atsm$position=="-1",]
atsm_prim<- atsm[!atsm$T=="NA",]

#sprawdzenie outlierów
grubbs.result <- grubbs.test(atsm$T)
  #trzeba by to zautomatyzować bo dostajemy wynik że największa wartość jest outlierem
  #trzeba by to powtórzyć parę razy i pokasować te wartości

#przerobienie position na numeric
atsm$position<-as.numeric(as.character(atsm$position))
atsm_small <- atsm[c(1:5000),] #zbior pierwszych 5000 wierszy

#model liniowy
ml<-lm(log(T)~position, data=atsm)

#diagnostyka modelu
#normal QQ
#cook's distance

#anova
model1 <- anova(lm(log(T)~position, data=atsm)) 
model2 <-aov(log(T)~position, data=atsm)

#test SNK i HSD 
library(agricolae)
HSD.test(model2, "Group", console=TRUE)
SNK.test(model1, "Group", console=TRUE)
