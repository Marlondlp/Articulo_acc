setwd("~/Cogido del dropbox/Accesiones/INRA")

library(ggplot2)
library(tidyr)
library(export)

#subir archivo de datos
DatosBurdeosDMdef12_05_2021 <- read_excel("C:/Users/malon/Dropbox/Tesis PhD/Tesis escrita/Datos Burdeos/DatosBurdeosDMdef12.05.2021.xlsx", 
                                          +     sheet = "DatosBurdeosDMdef")

#Convertir las variables accesiones y fuente en factores para que siempre se grafiquen en el orden que yo quiero
DatosBurdeosDMdef12_05_2021$Accesion <- factor(DatosBurdeosDMdef12_05_2021$Accesion,levels = c("Foz1","Adi-2","ABR2","Bd3-1","RON2","ABR4","ABR6","ABR8","Tek-4","ABR5","Sig2","Bd1-1","BdTR7a","ABR3","Bd29-1","Adi-10","Luc1","Jer1","Koz-3","Uni2","BdTR12c","Bd21-3","Bd2-3","Bd30-1","BdTR9K","Arn1","BdTR11G","BdTR2G","S8iiC","Bis-1","Kah-1","BdTR8i","Bd21","ABR7","Kah-5","BdTR5I","Adi-12","Mon3","BdTR3C","BdTR13a","BdTR13C","Bd18-1","BdTR11A","BdTR11I","Gaz-8","BdTR2B","BdTR10C","Mur1","Per1","Mig3","Koz-1","BdTR1i") )
DatosBurdeosDMdef12_05_2021$Source <- factor(DatosBurdeosDMdef12_05_2021$Source, levels=c("N", "A"))

#Crear un DataFrame con el promedio de los datos teniedo en cuenta el organo y fuente de nitrogeno
DatosBurdeosDMdef12_05_2021_unite<-unite(DatosBurdeosDMdef12_05_2021,Treatment, c("Source", "Accesion", 
                                      "Organ"),sep="_", remove=FALSE)
DatosBurdeosDMdef12_05_2021_Mean<-aggregate(DatosBurdeosDMdef12_05_2021_unite[,5:33], by=list(DatosBurdeosDMdef12_05_2021_unite$Treatment), mean, 
                      na.rm=TRUE)

#separar las variables de la columna Group.1
sepmean.Datos<-separate(DatosBurdeosDMdef12_05_2021_Mean,Group.1,into=c("Source", "Accesion", 
                                                  "Organ" ),sep="_")

# Volver a convertir las variables accesiones y fuente en factores para que siempre se grafiquen en el orden que yo quiero
sepmean.Datos$Accesion <- factor(sepmean.Datos$Accesion,levels = c("Foz1","Adi-2","ABR2","Bd3-1","RON2","ABR4","ABR6","ABR8","Tek-4","ABR5","Sig2","Bd1-1","BdTR7a","ABR3","Bd29-1","Adi-10","Luc1","Jer1","Koz-3","Uni2","BdTR12c","Bd21-3","Bd2-3","Bd30-1","BdTR9K","Arn1","BdTR11G","BdTR2G","S8iiC","Bis-1","Kah-1","BdTR8i","Bd21","ABR7","Kah-5","BdTR5I","Adi-12","Mon3","BdTR3C","BdTR13a","BdTR13C","Bd18-1","BdTR11A","BdTR11I","Gaz-8","BdTR2B","BdTR10C","Mur1","Per1","Mig3","Koz-1","BdTR1i") )
sepmean.Datos$Source <- factor(sepmean.Datos$Source, levels=c("N", "A"))


#Crear Dataframes con los datos de hojas y raiz por separado
hojas_mean<-subset(sepmean.Datos, Organ=="leaves")
raiz_mean<-subset(sepmean.Datos, Organ=="Root")

#_______________________________________________________________________________________


#selecciono el ratio A/N de la biomasa total, utilizo cualquiera de los organos y fuente porque este dato se repite

Ratio_Biomass_T<- subset(hojas_mean[,c(1,2,3,7)], Source=="A",na.rm=TRUE)
  

#Grafico de barras
names(Ratio_Biomass_T)[4] <- "Ratio"
ggplot(Ratio_Biomass_T, aes(x=Accesion, y=Ratio)) + 
  geom_bar(stat="identity", position="identity", colour="grey100", fill = "grey")+ theme(text = element_text(size=8), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8), axis.text.y = element_text(size=10), panel.background = element_rect(fill = "white", colour = "grey1"), axis.title.x=element_blank()) + scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))   + expand_limits(y=c(0,1)) +  ylab(expression(paste("Total Biomass ratio ( ", NH[4]^+1, " / " , NO[3]^-1," )", sep=""))) 

#Exportar grafica a un ppt

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#Grafico vioplot 
ggplot(Ratio_Biomass_T,aes(x=Source, y=Ratio, fill=Source))+
  geom_violin(show.legend=FALSE, fill = "grey") + geom_boxplot(width=0.1, 
                                                               show.legend=FALSE, fill = "grey")+
  scale_fill_manual(values=c("#FF9999", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,1))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) + expand_limits(y=c(0,1)) 

#Exportar grafica
graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#graficar boxplot:
#selecciono la biomasa total sin promediar del archivo DatosBurdeosDMdef12_05_2021, filtro para cualquiera de los organos porque este dato se repite

Biomass_T<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,5)], Organ=="leaves",na.rm=TRUE)
names(Biomass_T)[4] <- "valor"
ggplot(Biomass_T, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") +  expand_limits(y=c(0,1.6)) 

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_biomass_T<- hojas_mean[,c(1,2,3,5)]
names(hojas_mean_biomass_T)[4] <- "valor"
ggplot(hojas_mean_biomass_T,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1, 
                                                show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,1.6))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) 

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)


#__________________________________________________________________________
#ratio Shoot biomass
#el ratio A/N de la biomasa shoot no se encuentra en la base de datos, por lo que creo un dataframen con este dato
split_biomass_shoot<-split(hojas_mean[,c(1,2,3,4)], f = hojas_mean$Source)
ratio_biomass_shoot<-cbind(hojas_mean[c(1:52),c(1:3)], data.frame(split_biomass_shoot[["A"]][["Organ-FW"]]/split_biomass_shoot[["N"]][["Organ-FW"]]))

# Volver a convertir las variables accesiones y fuente en factores para que siempre se grafiquen en el orden que yo quiero
ratio_biomass_shoot$Accesion <- factor(ratio_biomass_shoot$Accesion,levels = c("Foz1","Adi-2","ABR2","Bd3-1","RON2","ABR4","ABR6","ABR8","Tek-4","ABR5","Sig2","Bd1-1","BdTR7a","ABR3","Bd29-1","Adi-10","Luc1","Jer1","Koz-3","Uni2","BdTR12c","Bd21-3","Bd2-3","Bd30-1","BdTR9K","Arn1","BdTR11G","BdTR2G","S8iiC","Bis-1","Kah-1","BdTR8i","Bd21","ABR7","Kah-5","BdTR5I","Adi-12","Mon3","BdTR3C","BdTR13a","BdTR13C","Bd18-1","BdTR11A","BdTR11I","Gaz-8","BdTR2B","BdTR10C","Mur1","Per1","Mig3","Koz-1","BdTR1i") )
ratio_biomass_shoot$Source <- factor(ratio_biomass_shoot$Source, levels=c("N", "A"))

#Grafico de barras
names(ratio_biomass_shoot)[4] <- "Ratio"
ggplot(ratio_biomass_shoot, aes(x=Accesion, y=Ratio)) + 
  geom_bar(stat="identity", position="identity", colour="grey100", fill = "grey")+ theme(text = element_text(size=8), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8), axis.text.y = element_text(size=10), panel.background = element_rect(fill = "white", colour = "grey1"), axis.title.x=element_blank()) + scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))   + expand_limits(y=c(0,1)) +  ylab(expression(paste("Shoot Biomass ratio ( ", NH[4]^+1, " / " , NO[3]^-1," )", sep=""))) 

#Exportar grafica a un ppt

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#Grafico vioplot 
ggplot(ratio_biomass_shoot,aes(x=Source, y=Ratio, fill=Source))+
  geom_violin(show.legend=FALSE, fill = "grey") + geom_boxplot(width=0.1, 
                                                               show.legend=FALSE, fill = "grey")+
  scale_fill_manual(values=c("#FF9999", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,1))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) 

#Exportar grafica
graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#graficar boxplot:
#selecciono la biomasa total sin promediar del archivo DatosBurdeosDMdef12_05_2021, filtro para "leaves"

Biomass_shoot<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,4)], Organ=="leaves",na.rm=TRUE)
names(Biomass_shoot)[4] <- "valor"
ggplot(Biomass_shoot, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") +  expand_limits(y=c(0,1))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_Biomass_shoot<- hojas_mean[,c(1,2,3,4)]
names(hojas_mean_Biomass_shoot)[4] <- "valor"
ggplot(hojas_mean_Biomass_shoot,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,1))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#-------------------------------------------------------------------------------------------

#ratio Root biomass
#el ratio A/N de la biomasa Root no se encuentra en la base de datos, por lo que creo un dataframen con este dato
split_biomass_Root<-split(raiz_mean[,c(1,2,3,4)], f = raiz_mean$Source)
ratio_biomass_Root<-cbind(raiz_mean[c(1:52),c(1:3)], data.frame(split_biomass_Root[["A"]][["Organ-FW"]]/split_biomass_Root[["N"]][["Organ-FW"]]))

# Volver a convertir las variables accesiones y fuente en factores para que siempre se grafiquen en el orden que yo quiero
ratio_biomass_Root$Accesion <- factor(ratio_biomass_Root$Accesion,levels = c("Foz1","Adi-2","ABR2","Bd3-1","RON2","ABR4","ABR6","ABR8","Tek-4","ABR5","Sig2","Bd1-1","BdTR7a","ABR3","Bd29-1","Adi-10","Luc1","Jer1","Koz-3","Uni2","BdTR12c","Bd21-3","Bd2-3","Bd30-1","BdTR9K","Arn1","BdTR11G","BdTR2G","S8iiC","Bis-1","Kah-1","BdTR8i","Bd21","ABR7","Kah-5","BdTR5I","Adi-12","Mon3","BdTR3C","BdTR13a","BdTR13C","Bd18-1","BdTR11A","BdTR11I","Gaz-8","BdTR2B","BdTR10C","Mur1","Per1","Mig3","Koz-1","BdTR1i") )
ratio_biomass_Root$Source <- factor(ratio_biomass_Root$Source, levels=c("N", "A"))

#Grafico de barras
names(ratio_biomass_Root)[4] <- "Ratio"
ggplot(ratio_biomass_Root, aes(x=Accesion, y=Ratio)) + 
  geom_bar(stat="identity", position="identity", colour="grey100", fill = "grey")+ theme(text = element_text(size=8), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8), axis.text.y = element_text(size=10), panel.background = element_rect(fill = "white", colour = "grey1"), axis.title.x=element_blank()) + scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))   + expand_limits(y=c(0,1)) +  ylab(expression(paste("Total Biomass ratio ( ", NH[4]^+1, " / " , NO[3]^-1," )", sep=""))) 

#Exportar grafica a un ppt

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#Grafico vioplot 
ggplot(ratio_biomass_Root,aes(x=Source, y=Ratio, fill=Source))+
  geom_violin(show.legend=FALSE, fill = "grey") + geom_boxplot(width=0.1, 
                                                               show.legend=FALSE, fill = "grey")+
  scale_fill_manual(values=c("#FF9999", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) + expand_limits(y=c(0,1)) 

#Exportar grafica
graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#graficar boxplot:
#selecciono la biomasa total sin promediar del archivo DatosBurdeosDMdef12_05_2021, filtro para "leaves"

Biomass_Root<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,4)], Organ=="leaves",na.rm=TRUE)
names(Biomass_Root)[4] <- "valor"
ggplot(Biomass_Root, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") +  expand_limits(y=c(0,0.6))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_Biomass_Root<- raiz_mean[,c(1,2,3,4)]
names(raiz_mean_Biomass_Root)[4] <- "valor"
ggplot(raiz_mean_Biomass_Root,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,0.6))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#----------------------------------------------------------------------------------------------

#shoot/root biomasa
#graficar boxplot:
#selecciono shoot/root biomasa sin promediar del archivo DatosBurdeosDMdef12_05_2021, filtro para cualquera de los organo porque este dato se repite

Ratio_shoot_root<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,6)], Organ=="leaves",na.rm=TRUE)
names(Ratio_shoot_root)[4] <- "valor"
ggplot(Ratio_shoot_root, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") +  expand_limits(y=c(0.5,2.5))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
mean_Ratio_shoot_root<- hojas_mean[,c(1,2,3,6)]
names(mean_Ratio_shoot_root)[4] <- "valor"
ggplot(mean_Ratio_shoot_root,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0.5,2.5))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#♠---------------------------------------------------------------------------------------------
#clorofila
#graficar boxplot:
#selecciono la biomasa total sin promediar del archivo DatosBurdeosDMdef12_05_2021, filtro para "leaves"

Chl<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,8)], Organ=="leaves",na.rm=TRUE)
names(Chl)[4] <- "valor"
ggplot(Chl, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(1,2.5))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_Chl<- hojas_mean[,c(1,2,3,8)]
names(hojas_mean_Chl)[4] <- "valor"
ggplot(hojas_mean_Chl,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(1,2.5)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-----------------------------------------------------------------------------------------------
#amonio en hoja
NH4<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,9)], Organ=="leaves",na.rm=TRUE)
names(NH4)[4] <- "valor"
ggplot(NH4, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,30))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_NH4<- hojas_mean[,c(1,2,3,9)]
names(hojas_mean_NH4)[4] <- "valor"
ggplot(hojas_mean_NH4,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,30)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#-----------------------------------------------------------------------------------------------
#Nitrato en hoja
 
#------------------------------------------------------------------------------------------------
# proteinas en hojas
PROTEINS<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,10)], Organ=="leaves",na.rm=TRUE)
names(PROTEINS)[4] <- "valor"
ggplot(PROTEINS, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(35,70))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_PROTEINS<- hojas_mean[,c(1,2,3,10)]
names(hojas_mean_PROTEINS)[4] <- "valor"
ggplot(hojas_mean_PROTEINS,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(35,70)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#----------------------------------------------------------------------------------------

#Aminoacidos en hojas
AMINOACIDS<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,11)], Organ=="leaves",na.rm=TRUE)
names(AMINOACIDS)[4] <- "valor"
ggplot(AMINOACIDS, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,200))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_AMINOACIDS<- hojas_mean[,c(1,2,3,11)]
names(hojas_mean_AMINOACIDS)[4] <- "valor"
ggplot(hojas_mean_AMINOACIDS,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,200)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#----------------------------------------------------------------------------------------
#Glutamato en hojas
GLU<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,16)], Organ=="leaves",na.rm=TRUE)
names(GLU)[4] <- "valor"
ggplot(GLU, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,40))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_GLU<- hojas_mean[,c(1,2,3,16)]
names(hojas_mean_GLU)[4] <- "valor"
ggplot(hojas_mean_GLU,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,40)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-------------------------------------------------------------------------------------------
#fructosa en hojas
FRUC<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,13)], Organ=="leaves",na.rm=TRUE)
names(FRUC)[4] <- "valor"
ggplot(FRUC, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,6))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_FRUC<- hojas_mean[,c(1,2,3,13)]
names(hojas_mean_FRUC)[4] <- "valor"
ggplot(hojas_mean_FRUC,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,6)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#---------------------------------------------------------------------------------------------
#glucosa en hojas
GLUC<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,14)], Organ=="leaves",na.rm=TRUE)
names(GLUC)[4] <- "valor"
ggplot(GLUC, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,20))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_GLUC<- hojas_mean[,c(1,2,3,14)]
names(hojas_mean_GLUC)[4] <- "valor"
ggplot(hojas_mean_GLUC,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,20))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#---------------------------------------------------------------------------------------------
#sucrose en hojas
SUC<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,15)], Organ=="leaves",na.rm=TRUE)
names(SUC)[4] <- "valor"
ggplot(SUC, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(20,110))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_SUC<- hojas_mean[,c(1,2,3,15)]
names(hojas_mean_SUC)[4] <- "valor"
ggplot(hojas_mean_SUC,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(20,110)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-------------------------------------------------------------------------------------------
#Starch en hojas
STARCH<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,18)], Organ=="leaves",na.rm=TRUE)
names(STARCH)[4] <- "valor"
ggplot(STARCH, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,130))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_STARCH<- hojas_mean[,c(1,2,3,18)]
names(hojas_mean_STARCH)[4] <- "valor"
ggplot(hojas_mean_STARCH,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,130)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-------------------------------------------------------------------------------------------
#citrate en hojas
CITRATE<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,19)], Organ=="leaves",na.rm=TRUE)
names(CITRATE)[4] <- "valor"
ggplot(CITRATE, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,42))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_CITRATE<- hojas_mean[,c(1,2,3,19)]
names(hojas_mean_CITRATE)[4] <- "valor"
ggplot(hojas_mean_CITRATE,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,42)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#-------------------------------------------------------------------------------------------------
#malate en hojas
MALATE<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,17)], Organ=="leaves",na.rm=TRUE)
names(MALATE)[4] <- "valor"
ggplot(MALATE, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,16))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_MALATE<- hojas_mean[,c(1,2,3,17)]
names(hojas_mean_MALATE)[4] <- "valor"
ggplot(hojas_mean_MALATE,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,16))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#---------------------------------------------------------------------------------------------
#gluthathione en hojas
GLUTATHIONE<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,30)], Organ=="leaves",na.rm=TRUE)
names(GLUTATHIONE)[4] <- "valor"
ggplot(GLUTATHIONE, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,9))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_GLUTATHIONE<- hojas_mean[,c(1,2,3,30)]
names(hojas_mean_GLUTATHIONE)[4] <- "valor"
ggplot(hojas_mean_GLUTATHIONE,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,9)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-------------------------------------------------------------------------------------------
#Glucokinase en hojas
GK<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,29)], Organ=="leaves",na.rm=TRUE)
names(GK)[4] <- "valor"
ggplot(GK, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,40))


graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_GK<- hojas_mean[,c(1,2,3,29)]
names(hojas_mean_GK)[4] <- "valor"
ggplot(hojas_mean_GK,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,40))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#-------------------------------------------------------------------------------------------
#fructokinase en hojas
FK<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,28)], Organ=="leaves",na.rm=TRUE)
names(FK)[4] <- "valor"
ggplot(FK, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,60))


graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_FK<- hojas_mean[,c(1,2,3,28)]
names(hojas_mean_FK)[4] <- "valor"
ggplot(hojas_mean_FK,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,60))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-------------------------------------------------------------------------------------------
#piruvate kinase
PK<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,27)], Organ=="leaves",na.rm=TRUE)
names(PK)[4] <- "valor"
ggplot(PK, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(20,150))


graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_PK<- hojas_mean[,c(1,2,3,27)]
names(hojas_mean_PK)[4] <- "valor"
ggplot(hojas_mean_PK,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(20,150)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#-------------------------------------------------------------------------------------------
#citrato syntasa en hojas
CS<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,20)], Organ=="leaves",na.rm=TRUE)
names(CS)[4] <- "valor"
ggplot(CS, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(25,150))


graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_CS<- hojas_mean[,c(1,2,3,20)]
names(hojas_mean_CS)[4] <- "valor"
ggplot(hojas_mean_CS,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(25,150))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#----------------------------------------------------------------------------------------------
#Csm en hojas
CSM<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,21)], Organ=="leaves",na.rm=TRUE)
names(CSM)[4] <- "valor"
ggplot(CSM, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,45))


graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_CSM<- hojas_mean[,c(1,2,3,21)]
names(hojas_mean_CSM)[4] <- "valor"
ggplot(hojas_mean_CSM,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,45)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#------------------------------------------------------------------------------------------
#NADP ICDH en hojas
ICDH<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,25)], Organ=="leaves",na.rm=TRUE)
names(ICDH)[4] <- "valor"
ggplot(ICDH, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(15,120))


graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_ICDH<- hojas_mean[,c(1,2,3,25)]
names(hojas_mean_ICDH)[4] <- "valor"
ggplot(hojas_mean_ICDH,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(15,120)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#----------------------------------------------------------------------------------------------
#NAD GDH en hojas
GDH<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,22)], Organ=="leaves",na.rm=TRUE)
names(GDH)[4] <- "valor"
ggplot(GDH, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,85))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_GDH<- hojas_mean[,c(1,2,3,22)]
names(hojas_mean_GDH)[4] <- "valor"
ggplot(hojas_mean_GDH,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,85))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-------------------------------------------------------------------------------------------
#MDH en hojas
MDH<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,24)], Organ=="leaves",na.rm=TRUE)
names(MDH)[4] <- "valor"
ggplot(MDH, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(5,20))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_MDH<- hojas_mean[,c(1,2,3,24)]
names(hojas_mean_MDH)[4] <- "valor"
ggplot(hojas_mean_MDH,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(5,20)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#----------------------------------------------------------------------------------------------------
#PEPC en hojas
PEPC<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,26)], Organ=="leaves",na.rm=TRUE)
names(PEPC)[4] <- "valor"
ggplot(PEPC, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(20,90))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
hojas_mean_PEPC<- hojas_mean[,c(1,2,3,26)]
names(hojas_mean_PEPC)[4] <- "valor"
ggplot(hojas_mean_PEPC,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(20,90)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#-----------------------------------------------------------------------------------------

#Amonio en raices
NH4<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,9)], Organ=="Root",na.rm=TRUE)
names(NH4)[4] <- "valor"
ggplot(NH4, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,55))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_NH4<- raiz_mean[,c(1,2,3,9)]
names(raiz_mean_NH4)[4] <- "valor"
ggplot(raiz_mean_NH4,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,55)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#--------------------------------------------------------------------------------------
#nitrato en raiz
NO3<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,12)], Organ=="Root",na.rm=TRUE)
names(NO3)[4] <- "valor"
ggplot(NO3, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,250))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_NO3<- raiz_mean[,c(1,2,3,12)]
names(raiz_mean_NO3)[4] <- "valor"
ggplot(raiz_mean_NO3,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,250))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#--------------------------------------------------------------------------------------------
#proteinas en raiz
PROTEINS<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,10)], Organ=="Root",na.rm=TRUE)
names(PROTEINS)[4] <- "valor"
ggplot(PROTEINS, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(10,30))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_PROTEINS<- raiz_mean[,c(1,2,3,10)]
names(raiz_mean_PROTEINS)[4] <- "valor"
ggplot(raiz_mean_PROTEINS,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(10,30)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#-----------------------------------------------------------------------------------------------
#amino acids en raiz
AMINOACIDS<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,11)], Organ=="Root",na.rm=TRUE)
names(AMINOACIDS)[4] <- "valor"
ggplot(AMINOACIDS, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,400))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_AMINOACIDS<- raiz_mean[,c(1,2,3,11)]
names(raiz_mean_AMINOACIDS)[4] <- "valor"
ggplot(raiz_mean_AMINOACIDS,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,400))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#------------------------------------------------------------------------------------------------
#glutamato en raiz
GLU<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,16)], Organ=="Root",na.rm=TRUE)
names(GLU)[4] <- "valor"
ggplot(GLU, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,30))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_GLU<- raiz_mean[,c(1,2,3,16)]
names(raiz_mean_GLU)[4] <- "valor"
ggplot(raiz_mean_GLU,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,30)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-----------------------------------------------------------------------------
#fructose en raiz
FRUC<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,13)], Organ=="Root",na.rm=TRUE)
names(FRUC)[4] <- "valor"
ggplot(FRUC, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,6.5))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_FRUC<- raiz_mean[,c(1,2,3,13)]
names(raiz_mean_FRUC)[4] <- "valor"
ggplot(raiz_mean_FRUC,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,6.5)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#-----------------------------------------------------------------------------------
#
GLUC<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,14)], Organ=="Root",na.rm=TRUE)
names(GLUC)[4] <- "valor"
ggplot(GLUC, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,6.5))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_GLUC<- raiz_mean[,c(1,2,3,14)]
names(raiz_mean_GLUC)[4] <- "valor"
ggplot(raiz_mean_GLUC,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,6.5)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#----------------------------------------------------------------------------------------------
#sacarosa en raiz
SUC<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,15)], Organ=="Root",na.rm=TRUE)
names(SUC)[4] <- "valor"
ggplot(SUC, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,40))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_SUC<- raiz_mean[,c(1,2,3,15)]
names(raiz_mean_SUC)[4] <- "valor"
ggplot(raiz_mean_SUC,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,40))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#♦---------------------------------------------------------------------------------------------
#Citrate en raiz
CITRATE<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,19)], Organ=="Root",na.rm=TRUE)
names(CITRATE)[4] <- "valor"
ggplot(CITRATE, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,5))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_CITRATE<- raiz_mean[,c(1,2,3,19)]
names(raiz_mean_CITRATE)[4] <- "valor"
ggplot(raiz_mean_CITRATE,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,5))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#------------------------------------------------------------------------------------------------
#malato en raiz
MALATE<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,17)], Organ=="Root",na.rm=TRUE)
names(MALATE)[4] <- "valor"
ggplot(MALATE, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,4))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_MALATE<- raiz_mean[,c(1,2,3,17)]
names(raiz_mean_MALATE)[4] <- "valor"
ggplot(raiz_mean_MALATE,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,4)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#----------------------------------------------------------------------
#glutathione raiz
GLUTATHIONE<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,30)], Organ=="Root",na.rm=TRUE)
names(GLUTATHIONE)[4] <- "valor"
ggplot(GLUTATHIONE, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,1.5))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_GLUTATHIONE<- raiz_mean[,c(1,2,3,30)]
names(raiz_mean_GLUTATHIONE)[4] <- "valor"
ggplot(raiz_mean_GLUTATHIONE,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,1.5)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#---------------------------------------------------------------------------------------------
#Gk en raiz
GK<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,29)], Organ=="Root",na.rm=TRUE)
names(GK)[4] <- "valor"
ggplot(GK, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,150))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_GK<- raiz_mean[,c(1,2,3,29)]
names(raiz_mean_GK)[4] <- "valor"
ggplot(raiz_mean_GK,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,150))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#------------------------------------------------------------------------------------------
#Fk en raiz
FK<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,28)], Organ=="Root",na.rm=TRUE)
names(FK)[4] <- "valor"
ggplot(FK, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,120))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_FK<- raiz_mean[,c(1,2,3,28)]
names(raiz_mean_FK)[4] <- "valor"
ggplot(raiz_mean_FK,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,120))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#------------------------------------------------------------------------------------------------
#piruvate kinase
#PK<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,27)], Organ=="Root",na.rm=TRUE)
names(PK)[4] <- "valor"
ggplot(PK, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(20,200))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_PK<- raiz_mean[,c(1,2,3,27)]
names(raiz_mean_PK)[4] <- "valor"
ggplot(raiz_mean_PK,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(20,200)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-----------------------------------------------------------------------------------------------
CS<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,20)], Organ=="Root",na.rm=TRUE)
names(CS)[4] <- "valor"
ggplot(CS, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(25,250))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_CS<- raiz_mean[,c(1,2,3,20)]
names(raiz_mean_CS)[4] <- "valor"
ggplot(raiz_mean_CS,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(25,250)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-------------------------------------------------------------------------------------------------
#Csm en raiz
CSM<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,21)], Organ=="Root",na.rm=TRUE)
names(CSM)[4] <- "valor"
ggplot(CSM, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,45))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_CSM<- raiz_mean[,c(1,2,3,21)]
names(raiz_mean_CSM)[4] <- "valor"
ggplot(raiz_mean_CSM,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,45)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-----------------------------------------------------------------------------------------------
#ICDH en raiz
ICDH<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,25)], Organ=="Root",na.rm=TRUE)
names(ICDH)[4] <- "valor"
ggplot(ICDH, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(10,150))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_ICDH<- raiz_mean[,c(1,2,3,25)]
names(raiz_mean_ICDH)[4] <- "valor"
ggplot(raiz_mean_ICDH,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(10,150)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#---------------------------------------------------------------------------------
#GDH en raiz
GDH<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,22)], Organ=="Root",na.rm=TRUE)
names(GDH)[4] <- "valor"
ggplot(GDH, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,250))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_GDH<- raiz_mean[,c(1,2,3,22)]
names(raiz_mean_GDH)[4] <- "valor"
ggplot(raiz_mean_GDH,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,250)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#-------------------------------------------------------------------------------
#MDH en raiz
GDH<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,24)], Organ=="Root",na.rm=TRUE)
names(GDH)[4] <- "valor"
ggplot(GDH, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits (y=c(2.5,13))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_GDH<- raiz_mean[,c(1,2,3,24)]
names(raiz_mean_GDH)[4] <- "valor"
ggplot(raiz_mean_GDH,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits (y=c(2.5,13))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
#------------------------------------------------------------------------------
#PEPC en raiz
PEPC<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,26)], Organ=="Root",na.rm=TRUE)
names(PEPC)[4] <- "valor"
ggplot(PEPC, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") + expand_limits(y=c(0,330))

graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot con el promediode los datos por accesion:
raiz_mean_PEPC<- raiz_mean[,c(1,2,3,26)]
names(raiz_mean_PEPC)[4] <- "valor"
ggplot(raiz_mean_PEPC,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1,                                       show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,330)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)


