##subir archivo de datos
DatosBurdeosDMdef12_05_2021 <- read_excel("C:/Users/malon/Dropbox/Tesis PhD/Tesis escrita/Datos Burdeos/DatosBurdeosDMdef12.05.2021.xlsx", 
                                          +     sheet = "DatosBurdeosDMdef")


#Cargar librerias
library("gplots")
library(tidyr)


#Crear un DataFrame con el promedio de los datos teniedo en cuenta el organo y fuente de nitrogeno
DatosBurdeosDMdef12_05_2021_unite<-unite(DatosBurdeosDMdef12_05_2021,Treatment, c("Source", "Accesion", 
                                                                                  "Organ"),sep="_", remove=FALSE)
DatosBurdeosDMdef12_05_2021_Mean<-aggregate(DatosBurdeosDMdef12_05_2021_unite[,5:33], by=list(DatosBurdeosDMdef12_05_2021_unite$Treatment), mean, 
                                            na.rm=TRUE)

#separar las variables de la columna Group.1
sepmean.Datos<-separate(DatosBurdeosDMdef12_05_2021_Mean,Group.1,into=c("Source", "Accesion", 
                                                                        "Organ" ),sep="_")
#Crear Dataframes con los datos de hojas y raiz por separado
hojas_mean<-subset(sepmean.Datos, Organ=="leaves")
raiz_mean<-subset(sepmean.Datos, Organ=="Root")

#Separar los datos de amonio y nitrato
A_hojas_mean<-subset(hojas_mean, Source=="A")
A_raiz_mean<-subset(raiz_mean, Source=="A")
N_hojas_mean<-subset(hojas_mean, Source=="N")
N_raiz_mean<-subset(raiz_mean, Source=="N")


#Nombre de los rows
rownames(A_hojas_mean)=A_hojas_mean$Accesion
rownames(A_raiz_mean)=A_raiz_mean$Accesion
rownames(N_hojas_mean)=N_hojas_mean$Accesion
rownames(N_raiz_mean)=N_raiz_mean$Accesion


#convertir a matrix con datos de biomasa
matrix_A_hojas_mean_no_biomass<-as.matrix(A_hojas_mean[,c(-1:-7,-23,-31,-32)])
#convertir a matrix con datos de biomasa
matrix_A_hojas_mean_yes_biomass<-as.matrix(A_hojas_mean[,c(-1:-3,-6,-7,-23,-31,-32)])



#Pruebas cambiando el procesamiento de los datos antes de graficar el Heatmap:

#Datos escalados y con biomasas (parecido a la Tesis)
Prueba1<-scale(matrix_A_hojas_mean_yes_biomass, center = FALSE, scale = TRUE)

#Datos escalados y sin biomasas biomasa 
Prueba2<-scale(matrix_A_hojas_mean_no_biomass, center = FALSE, scale = TRUE)

#Datos centrados y escalados
Prueba3<-scale(matrix_A_hojas_mean_no_biomass, center = TRUE, scale = TRUE)
  
#Datos transformados, centrados, escalados
Prueba4<-scale(sqrt(matrix_A_hojas_mean_no_biomass), center = TRUE, scale = TRUE)


#Crear matrix para que al graficar me coloree las 10 accesiones mas sensibles y las 10 mas tolerantes
rowCols <-ifelse(rownames(Prueba1)=="BdTR1i"|rownames(Prueba1)=="Koz-1"|rownames(Prueba1)=="Mig3"|rownames(Prueba1)=="Per1"|rownames(Prueba1)=="Mur1"|rownames(Prueba1)=="BdTR10C"|rownames(Prueba1)=="BdTR2B"|rownames(Prueba1)=="Gaz-8"|rownames(Prueba1)=="BdTR11I"|rownames(Prueba1)=="BdTR11A", " darkorchid4",ifelse (rownames(Prueba1)=="Foz1"|rownames(Prueba1)=="Adi-2"|rownames(Prueba1)=="ABR2"|rownames(Prueba1)=="Bd3-1"|rownames(Prueba1)=="RON2"|rownames(Prueba1)=="ABR4"|rownames(Prueba1)=="ABR6"|rownames(Prueba1)=="ABR8"|rownames(Prueba1)=="Tek-4"|rownames(Prueba1)=="ABR5","yellow4","white"))
#Nota: como el orden de las accesiones en todas las matrices es el mismo, este rowCols me sirve para todas.


#Graficar el Heatmap:
Prueba1<-heatmap.2(Prueba1, col="bluered", cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="Datos escalados y con biomasas (parecido a la Tesis)", trace="none",scale="col")
Prueba2<-heatmap.2(Prueba2, col="bluered",cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="Datos escalados y sin biomasas", trace="none", scale="col")
Prueba3<-heatmap.2(Prueba3, col="bluered",cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="Datos centrados y escalados", trace="none", scale="col")
Prueba4<-heatmap.2(Prueba4, col="bluered",cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="Datos transformados, centrados, escalados", trace="none", scale="col")