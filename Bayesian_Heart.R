##Pakiety
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install()
# BiocManager::install(c("graph", "Rgraphviz"))
# 
# if (!("gRain" %in% rownames(installed.packages()))) install.packages("gRain")
# if (!("bnlearn" %in% rownames(installed.packages()))) install.packages("bnlearn")
# if (!("lattice" %in% rownames(installed.packages()))) install.packages("lattice")
# if (!("lattice" %in% rownames(installed.packages()))) install.packages("lattice")

library(gRain)
library(bnlearn)
library(lattice)
library(BiocManager)


################## HEART

Dane<-read.csv("C:/Users/Patryk/Desktop/Semestr 6/Wnioskowanie w warunkach niepewnoœci/Projekt/Projekt/Statlog_Heart.csv")


names(Dane)[names(Dane) == "chest_pain_type"] <- "Chest"
names(Dane)[names(Dane) == "resting_blood_pressure"] <- "Press"
names(Dane)[names(Dane) == "serum_cholestoral"] <- "Cholest"
names(Dane)[names(Dane) == "fasting_blood_sugar"] <- "Sugar"
names(Dane)[names(Dane) == "resting_electrocardiographic_results"] <- "Electro"
names(Dane)[names(Dane) == "maximum_heart_rate_achieved"] <- "MaxRate"
names(Dane)[names(Dane) == "exercise_induced_angina"] <- "Angina"
names(Dane)[names(Dane) == "slope_of_the_peak_exercise_ST_segment"] <- "ST"
names(Dane)[names(Dane) == "number_of_major_vessels"] <- "Vessels"


##Zmienianie na czynniki
for (i in 2:3)
{
  Dane[,i]<-as.factor(Dane[,i])
}

for (i in 6:7)
{
  Dane[,i]<-as.factor(Dane[,i])
}

for (i in 9:14)
{
  Dane[,i]<-as.factor(Dane[,i])
}


## Zmiana ciaglych int na numeric
Dane[,1]<-as.numeric(Dane[,1])
Dane[,4]<-as.numeric(Dane[,4])
Dane[,5]<-as.numeric(Dane[,5])
Dane[,8]<-as.numeric(Dane[,8])






#Sieci hc
hc_siec<-hc(Dane)


#Histogramy dla danych ciaglych

bn<-bn.fit(hc_siec,Dane)

par(mfrow=c(1,1))

bn.fit.histogram(bn$age)
bn.fit.histogram(bn$Press)
bn.fit.histogram(bn$Cholest)
bn.fit.histogram(bn$MaxRate)

shapiro.test(Dane$age) #Nie ma rozkladu normalnego p<0.05
shapiro.test(Dane$Press) #Nie ma rozkladu normalnego p<0.05
shapiro.test(Dane$Cholest) #Nie ma rozkladu normalnego p<0.05
shapiro.test(Dane$MaxRate) #Nie ma rozkladu normalnego p<0.05 


#Wykresy slupkowe dla danych dyskretnych
bn.fit.barchart(bn$sex)
bn.fit.barchart(bn$class)


#Dyskretyzacja danych
DaneDys<-discretize(Dane,method="interval",breaks=c(6,2,4,5,4,2,3,4,2,2,3,4,3,2))





##Sieci z wykorzystaniem algorytmu hc
hc_siecDys<-hc(DaneDys)

graphviz.plot(hc_siec)
graphviz.plot(hc_siecDys)

bn_hc_siec<-bn.fit(hc_siec,Dane)
bn_hc_siecDys<-bn.fit(hc_siecDys,DaneDys)


score(hc_siec,data=Dane)
score(hc_siecDys,data=DaneDys,type="bic")

##Siec hc  dla danych dyskretnych stworzona na wzór zwyklej

hc_siecDys2<-hc_siecDys

hc_siecDys2<-set.arc(hc_siecDys2, from="sex", to="Cholest")
hc_siecDys2<-set.arc(hc_siecDys2, from="Cholest", to="age")
hc_siecDys2<-set.arc(hc_siecDys2, from="age", to="Press")
hc_siecDys2<-set.arc(hc_siecDys2, from="ST", to="MaxRate")
hc_siecDys2<-set.arc(hc_siecDys2, from="MaxRate", to="age")
hc_siecDys2<-drop.arc(hc_siecDys2, from="class", to="MaxRate")


graphviz.plot(hc_siecDys2)

bn_hc_siecDys2<-bn.fit(hc_siecDys2,DaneDys)

score(hc_siecDys2,data=DaneDys,type="bic")


### PC.Stable


pc_siec<-pc.stable(Dane)
#Dodaje kierunki do ³uków
pc_siec<-set.arc(pc_siec, from="age", to="Press")
pc_siec<-set.arc(pc_siec, from="Angina", to="Chest")

pc_siecDys<-pc.stable(DaneDys)
pc_siecDys2<-pc_siecDys
pc_siecDys2<-set.arc(pc_siecDys2, from="age", to="Press")
pc_siecDys2<-set.arc(pc_siecDys2, from="Angina", to="Chest")
pc_siecDys2<-set.arc(pc_siecDys2, from="Chest", to="class")
pc_siecDys2<-drop.arc(pc_siecDys2, from="thal", to="class")
pc_siecDys2<-set.arc(pc_siecDys2, from="thal", to="sex")
pc_siecDys2<-set.arc(pc_siecDys2, from="sex", to="Cholest")
pc_siecDys2<-set.arc(pc_siecDys2, from="Chest", to="class")
pc_siecDys2<-set.arc(pc_siecDys2, from="class", to="thal")

graphviz.plot(pc_siec)
graphviz.plot(pc_siecDys)
graphviz.plot(pc_siecDys2)



bn_pc_siec<-bn.fit(pc_siec,Dane)
#bn_pc_siecDys<-bn.fit(pc_siecDys,DaneDys)


score(pc_siec,data=Dane)
#score(pc_siecDys,data=DaneDys,type="bic")
score(pc_siecDys2,data=DaneDys,type="bic")







# iamb
iamb_siec<-iamb(Dane)
#Dodaje kierunki do ³uków, nie wiem czy mogê tak
iamb_siec<-set.arc(iamb_siec, from="age", to="MaxRate")
iamb_siec<-set.arc(iamb_siec, from="Vessels", to="class")


iamb_siecDys<-iamb(DaneDys)


graphviz.plot(iamb_siec)
graphviz.plot(iamb_siecDys)

iamb_siecDys<-set.arc(iamb_siecDys, from="age", to="MaxRate")
iamb_siecDys<-set.arc(iamb_siecDys, from="Vessels", to="class")


bn_iamb_siec<-bn.fit(iamb_siec,Dane)
bn_iamb_siecDys<-bn.fit(iamb_siecDys,DaneDys)


score(iamb_siec,data=Dane)
score(iamb_siecDys,data=DaneDys,type="bic")

#iamb dyskretne na wzor mieszanych

iamb_siecDys2<-iamb_siecDys
iamb_siecDys2<-set.arc(iamb_siecDys2, from="Vessels", to="class")
iamb_siecDys2<-set.arc(iamb_siecDys2, from="age", to="Press")
iamb_siecDys2<-set.arc(iamb_siecDys2, from="Cholest", to="Press")

graphviz.plot(iamb_siec)
graphviz.plot(iamb_siecDys2)


score(iamb_siecDys2,data=DaneDys,type="bic")


##gs


gs_siec<-gs(Dane)

##Dodaje kierunki do ³uków
gs_siec<-set.arc(gs_siec, from="Angina", to="Chest")

gs_siecDys<-gs(DaneDys)
gs_siecDys<-set.arc(gs_siecDys, from="sex", to="thal")
gs_siecDys<-set.arc(gs_siecDys, from="age", to="Vessels")
gs_siecDys<-set.arc(gs_siecDys, from="Angina", to="Chest")
gs_siecDys<-set.arc(gs_siecDys, from="ST", to="oldpeak")


graphviz.plot(gs_siec)
graphviz.plot(gs_siecDys)


bn_gs_siec<-bn.fit(gs_siec,Dane)
bn_gs_siecDys<-bn.fit(gs_siecDys,DaneDys)


score(gs_siec,data=Dane)
score(gs_siecDys,data=DaneDys,type="bic")



gs_siecDys2<-gs_siecDys
gs_siecDys2<-set.arc(gs_siecDys2, from="oldpeak", to="MaxRate")
gs_siecDys2<-set.arc(gs_siecDys2, from="oldpeak", to="MaxRate")
gs_siecDys2<-set.arc(gs_siecDys2, from="MaxRate", to="age")
gs_siecDys2<-set.arc(gs_siecDys2, from="age", to="Press")
gs_siecDys2<-set.arc(gs_siecDys2, from="Cholest", to="Press")
gs_siecDys2<-drop.arc(gs_siecDys2, from="ST", to="oldpeak")
gs_siecDys2<-drop.arc(gs_siecDys2, from="sex", to="thal")
gs_siecDys2<-drop.arc(gs_siecDys2, from="age", to="Vessels")

score(gs_siecDys2,data=DaneDys,type="bic")





#NAJLEPSZA SIEÆ
graphviz.plot(hc_siecDys)





#Prawdopodobobienstwa
jun<-compile(as.grain(bn_hc_siecDys))








check3<-0
Alicz_oldpeak_class_02<-0
Alicz_oldpeak_class_ST_022<-0


for (i in 1:nrow(DaneDys))
{
  if(DaneDys$oldpeak[i]==0 && DaneDys$class[i]==2 && DaneDys$ST[i]==2)
  {
    Alicz_oldpeak_class_ST_022<-Alicz_oldpeak_class_ST_022+1
    Alicz_oldpeak_class_02<-Alicz_oldpeak_class_02+1
  }
  
  else if (DaneDys$oldpeak[i]==0 && DaneDys$class[i]==2 )
  {
    Alicz_oldpeak_class_02<-Alicz_oldpeak_class_02+1
  }

  else
  {check3<-check3+1}
  
  
}

# Algorytmem ST pod warunkiem clas==2 oldpeak==0
  war_20 <- setEvidence(jun,nodes=c("class","oldpeak")
                        ,states=c("2","0"))
  querygrain(war_20,nodes="ST")$ST  
  Alicz_oldpeak_class_ST_022/Alicz_oldpeak_class_02


  
  
##
bn_hc_siecDys$thal
bn_hc_siecDys$sex
  
querygrain(jun,nodes="thal")$thal
  

bn_hc_siecDys$class

querygrain(jun,nodes="class")$class



  bn.fit.barchart(bn$sex)
  bn.fit.barchart(bn$thal)
  bn.fit.barchart(bn$oldpeak)
  bn.fit.barchart(bn$class)
  bn.fit.barchart(bn$ST)
  bn.fit.barchart(bn$Vessels)

  
