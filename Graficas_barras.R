setwd("~/Cogido del dropbox/Accesiones/INRA")

library(ggplot2)
#subir archivo de datos
DatosBurdeosDMdef12_05_2021 <- read_excel("C:/Users/malon/Dropbox/Tesis PhD/Tesis escrita/Datos Burdeos/DatosBurdeosDMdef12.05.2021.xlsx", 
                                          +     sheet = "DatosBurdeosDMdef")

#selecciono el ratio A/N de la biomasa total, filtro para cualquiera de los organos y fuente porque este dato se repite
library(tidyr)
Ratio_Biomass_T<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,7)], Organ=="leaves" & Source=="A",na.rm=TRUE)

#Grafico de barras
names(Ratio_Biomass_T)[4] <- "Ratio"
Ratio_Biomass_T$Accesion <- factor(Ratio_Biomass_T$Accesion,levels = c("Foz1","Adi-2","ABR2","Bd3-1","RON2","ABR4","ABR6","ABR8","Tek-4","ABR5","Sig2","Bd1-1","BdTR7a","ABR3","Bd29-1","Adi-10","Luc1","Jer1","Koz-3","Uni2","BdTR12c","Bd21-3","Bd2-3","Bd30-1","BdTR9K","Arn1","BdTR11G","BdTR2G","S8iiC","Bis-1","Kah-1","BdTR8i","Bd21","ABR7","Kah-5","BdTR5I","Adi-12","Mon3","BdTR3C","BdTR13a","BdTR13C","Bd18-1","BdTR11A","BdTR11I","Gaz-8","BdTR2B","BdTR10C","Mur1","Per1","Mig3","Koz-1","BdTR1i"))
ggplot(Ratio_Biomass_T, aes(x=Accesion, y=Ratio)) + 
  geom_bar(stat="identity", position="identity", colour="grey100", fill = "grey")+ theme(text = element_text(size=8), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8), axis.text.y = element_text(size=10), panel.background = element_rect(fill = "white", colour = "grey1"), axis.title.x=element_blank()) + scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))   + expand_limits(y=c(0,1)) +  ylab(expression(paste("Total Biomass ratio ( ", NH[4]^+1, " / " , NO[3]^-1," )", sep=""))) 

#Exportar grafica a un ppt
library(export)
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
library(export)
graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)

#graficar boxplot:
#selecciono la biomasa total del archivo DatosBurdeosDMdef12_05_2021, filtro para cualquiera de los organos porque este dato se repite

Biomass_T<-subset(DatosBurdeosDMdef12_05_2021[,c(1,2,3,5)], Organ=="leaves",na.rm=TRUE)
Biomass_T$Source = factor(Biomass_T$Source, levels=c("N", "A"))
names(Biomass_T)[4] <- "valor"
Biomass_T$Accesion <- factor(hojas$Accesion,levels = c("Foz1","Adi-2","ABR2","Bd3-1","RON2","ABR4","ABR6","ABR8","Tek-4","ABR5","Sig2","Bd1-1","BdTR7a","ABR3","Bd29-1","Adi-10","Luc1","Jer1","Koz-3","Uni2","BdTR12c","Bd21-3","Bd2-3","Bd30-1","BdTR9K","Arn1","BdTR11G","BdTR2G","S8iiC","Bis-1","Kah-1","BdTR8i","Bd21","ABR7","Kah-5","BdTR5I","Adi-12","Mon3","BdTR3C","BdTR13a","BdTR13C","Bd18-1","BdTR11A","BdTR11I","Gaz-8","BdTR2B","BdTR10C","Mur1","Per1","Mig3","Koz-1","BdTR1i") )
ggplot(Biomass_T, aes(x=Accesion, y=valor, fill=(Source)))+
  geom_boxplot(show.legend=FALSE)+  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  theme_bw()+ 
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(size=10))+
  xlab("")+ ylab("") +  expand_limits(y=c(0,1.6)) 

library(export)
graph2ppt(file="Ratio_plot.pptx", width=7, height=2.5, append=TRUE)

#graficar vioplot de la media de los datos en la fuente de Nitrogeno:
#con tydr usar unite para crear una columna nueva de tratamiento.
library(tidyr)
Datos<-unite(Biomass_T,Treatment, c("Source", "Accesion", 
                                    "Organ"),sep="_", remove=FALSE)
#sacar la media de cada tratamiento
mean.Datos<-aggregate(Datos[,5], by=list(Datos$Treatment), mean, 
                      na.rm=TRUE)
#separar las variables
sepmean.Datos<-separate(mean.Datos,Group.1,into=c("Source", "Accesion", 
                                                  "Organ" ),sep="_")
#ordenar las accesiones con el orden determinado
sepmean.Datos$Accesion <- factor(sepmean.Datos$Accesion,levels = c("Foz1","Adi-2","ABR2","Bd3-1","RON2","ABR4","ABR6","ABR8","Tek-4","ABR5","Sig2","Bd1-1","BdTR7a","ABR3","Bd29-1","Adi-10","Luc1","Jer1","Koz-3","Uni2","BdTR12c","Bd21-3","Bd2-3","Bd30-1","BdTR9K","Arn1","BdTR11G","BdTR2G","S8iiC","Bis-1","Kah-1","BdTR8i","Bd21","ABR7","Kah-5","BdTR5I","Adi-12","Mon3","BdTR3C","BdTR13a","BdTR13C","Bd18-1","BdTR11A","BdTR11I","Gaz-8","BdTR2B","BdTR10C","Mur1","Per1","Mig3","Koz-1","BdTR1i") )
sepmean.Datos$Source = factor(sepmean.Datos$Source, levels=c("N", "A"))

ggplot(sepmean.Datos,aes(x=Source, y=valor, fill=Source))+
  geom_violin(show.legend=FALSE) + geom_boxplot(width=0.1, 
                                                show.legend=FALSE)+
  scale_fill_manual(values=c("#56B4E9", "#FF9999"))+
  xlab("") + ylab("") +
  theme_bw()+ theme(panel.grid = element_blank())+
  expand_limits(y=c(0,1.6))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) 

library(export)
graph2ppt(file="Ratio_plot.pptx", width=1, height=2.5, append=TRUE)
