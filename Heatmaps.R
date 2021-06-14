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


#convertir a matrix
matrix_A_hojas_mean<-as.matrix(A_hojas_mean[,c(-1:-7,-23,-31,-32)])
matrix_A_raiz_mean<-as.matrix(A_raiz_mean[,c(-1:-8,-18,-23,-31,-32)])
matrix_N_hojas_mean<-as.matrix(N_hojas_mean[,c(-1:-7,-23,-31,-32)])
matrix_N_raiz_mean<-as.matrix(N_raiz_mean[,c(-1:-8,-18,-23,-31,-32)])

#Datos escalados (igual que en la Tesis)
Prueba1<-scale(matrix_A_hojas_mean, center = FALSE, scale = TRUE)

#Datos centrados y escalados
Prueba2<-scale(matrix_A_hojas_mean, center = TRUE, scale = TRUE)
  
#Datos transformados, centrados, escalados
Prueba3<-scale(sqrt(matrix_A_hojas_mean), center = TRUE, scale = TRUE)



#Datos tranformados, centrados y escalado  de los fenotipos
scaled_matrix_A_hojas_mean<-scale(sqrt(matrix_A_hojas_mean), center = TRUE, scale = TRUE)
scaled_matrix_A_raiz_mean<-scale(sqrt(matrix_A_raiz_mean), center = TRUE, scale = TRUE)
scaled_matrix_N_hojas_mean<-scale(sqrt(matrix_N_hojas_mean), center = TRUE, scale = TRUE)
scaled_matrix_N_raiz_mean<-scale(sqrt(matrix_N_raiz_mean), center = TRUE, scale = TRUE)

#Teniendo en cuenta el orden de las accesiones en la matrix
#Asignar colores a las accesiones en relacion al ratio A/N de la siguiete manera: 
#diez primeras Accesiones con ratio A/N mas alto: purpura oscuro
#posiciones 11-20 con ratio A/N mas alto: purpura intermedio
#posiciones 21-26 con ratio A/N mas alto: purpura claro
#diez primeras Accesiones con ratio A/N mas bajo: amarillo con verde oscuro
#posiciones 11-20 con ratio A/N mas bajo: amarilo con verde claro
#posiciones 21-26 con ratio A/N mas bajo: amarilo
rowCols <-ifelse(rownames(scaled_matrix_A_hojas_mean)=="BdTR1i"|rownames(scaled_matrix_A_hojas_mean)=="Koz-1"|rownames(scaled_matrix_A_hojas_mean)=="Mig3"|rownames(scaled_matrix_A_hojas_mean)=="Per1"|rownames(scaled_matrix_A_hojas_mean)=="Mur1"|rownames(scaled_matrix_A_hojas_mean)=="BdTR10C"|rownames(scaled_matrix_A_hojas_mean)=="BdTR2B"|rownames(scaled_matrix_A_hojas_mean)=="Gaz-8"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11I"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11A", " darkorchid4",ifelse(rownames(scaled_matrix_A_hojas_mean)=="Bd18-1"|rownames(scaled_matrix_A_hojas_mean)=="BdTR13C"|rownames(scaled_matrix_A_hojas_mean)=="BdTR13a"|rownames(scaled_matrix_A_hojas_mean)=="BdTR3C"|rownames(scaled_matrix_A_hojas_mean)=="Mon3"|rownames(scaled_matrix_A_hojas_mean)=="Adi-12"|rownames(scaled_matrix_A_hojas_mean)=="BdTR5"|rownames(scaled_matrix_A_hojas_mean)=="Kah-5"|rownames(scaled_matrix_A_hojas_mean)=="ABR7"|rownames(scaled_matrix_A_hojas_mean)=="Bd21", " darkorchid3" ,ifelse (rownames(scaled_matrix_A_hojas_mean)=="BdTR8i"|rownames(scaled_matrix_A_hojas_mean)=="Kah-1"|rownames(scaled_matrix_A_hojas_mean)=="Bis-1"|rownames(scaled_matrix_A_hojas_mean)=="S8iiC"|rownames(scaled_matrix_A_hojas_mean)=="BdTR2G"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11G","violet",ifelse (rownames(scaled_matrix_A_hojas_mean)=="Arn1"|rownames(scaled_matrix_A_hojas_mean)=="BdTR9K"|rownames(scaled_matrix_A_hojas_mean)=="Bd30-1"|rownames(scaled_matrix_A_hojas_mean)=="Bd2-3"|rownames(scaled_matrix_A_hojas_mean)=="Bd21-3"|rownames(scaled_matrix_A_hojas_mean)=="BdTR12c","yellow",ifelse (rownames(scaled_matrix_A_hojas_mean)=="Foz1"|rownames(scaled_matrix_A_hojas_mean)=="Adi-2"|rownames(scaled_matrix_A_hojas_mean)=="ABR2"|rownames(scaled_matrix_A_hojas_mean)=="Bd3-1"|rownames(scaled_matrix_A_hojas_mean)=="RON2"|rownames(scaled_matrix_A_hojas_mean)=="ABR4"|rownames(scaled_matrix_A_hojas_mean)=="ABR6"|rownames(scaled_matrix_A_hojas_mean)=="ABR8"|rownames(scaled_matrix_A_hojas_mean)=="Tek-4"|rownames(scaled_matrix_A_hojas_mean)=="ABR5","yellow4","yellow3")))))
#Nota: como el orden de las accesiones en todas las matrices es el mismo, este rowCols me sirve para todas.

#Graficarel el Heatmap de los datos estandarizados
heatmap.2(scaled_matrix_A_hojas_mean,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D"),cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="LEAF-AMMONIUM NUTRITION")
heatmap.2(scaled_matrix_A_raiz_mean,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D"),cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="ROOT-AMMONIUM NUTRITION")
heatmap.2(scaled_matrix_N_hojas_mean,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D"),cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="LEAF-NITRATE NUTRITION")
heatmap.2(scaled_matrix_N_raiz_mean,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D"),cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="ROOT-NITRATE NUTRITION")



#Graficar el Heatmap con los datos del fold change(log2(A/N))
#Nota: esta grafica es buena para indicarque fenotipos estan mas expresados en las plantas de amonio y nitrato
#Nota2: no la pondria en el articulo porque conclusiones similares puedo extraerde las PCAs
FC_hojas<-log2(matrix_A_hojas_mean)-log2(matrix_N_hojas_mean)
heatmap.2(FC_hojas,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D"),cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="grafica no publicable")
FC_raiz<-log2(matrix_A_raiz_mean)-log2(matrix_N_raiz_mean)
heatmap.2(FC_raiz,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D"),cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="grafica no publicable")



#Graficar el Heatmap con los datos del fold change estandarizado
scaled_FC_hojas<-scale(FC_hojas, center = TRUE, scale = TRUE)
heatmap.2(scaled_FC_hojas,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D"),cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="LEAF FOLD CHANGE AMMONIUM/NITRATE NUTRITION")
scaled_FC_raiz<-scale(FC_raiz, center = TRUE, scale = TRUE)
heatmap.2(scaled_FC_raiz,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D"),cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="ROOT FOLD CHANGE AMMONIUM/NITRATE NUTRITION")
