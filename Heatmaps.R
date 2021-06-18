##subir archivo de datos
DatosBurdeosDMdef12_05_2021 <- read_excel("C:/Users/malon/Dropbox/Tesis PhD/Tesis escrita/Datos Burdeos/DatosBurdeosDMdef12.05.2021.xlsx", 
                                          +     sheet = "DatosBurdeosDMdef")


#Cargar librerias
library("gplots")
library(tidyr)
library(RColorBrewer)

#♦Crear paleta de colores
gamadecolores = brewer.pal(11, "RdBu")
gamadecolores = colorRampPalette(gamadecolores)(20)

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
A_organos_mean<-subset(sepmean.Datos, Source=="A")
N_organos_mean<-subset(sepmean.Datos, Source=="N")

#Nombre de los rows
rownames(A_hojas_mean)=A_hojas_mean$Accesion
rownames(A_raiz_mean)=A_raiz_mean$Accesion
rownames(N_hojas_mean)=N_hojas_mean$Accesion
Nombre1<-unite(A_organos_mean,Treatment, c("Accesion", "Organ"),sep="_", remove=FALSE)
rownames(A_organos_mean)=Nombre1$Treatment
Nombre2<-unite(N_organos_mean,Treatment, c("Accesion", "Organ"),sep="_", remove=FALSE)
rownames(N_organos_mean)=Nombre2$Treatment

#convertir a matrix con datos de biomasa
matrix_A_hojas_mean<-as.matrix(A_hojas_mean[,c(-1:-7,-23,-31,-32)])
matrix_A_raiz_mean<-as.matrix(A_raiz_mean[,c(-1:-8,-18,-23,-31,-32)])
matrix_N_hojas_mean<-as.matrix(N_hojas_mean[,c(-1:-7,-23,-31,-32)])
matrix_N_raiz_mean<-as.matrix(N_raiz_mean[,c(-1:-8,-18,-23,-31,-32)])
matrix_A_organos_mean<-as.matrix(A_organos_mean[,c(-1:-8,-18,-23,-31,-32)])
matrix_N_organos_mean<-as.matrix(N_organos_mean[,c(-1:-8,-18,-23,-31,-32)])

#Trabajar con los datos escalados de los fenotipos

scaled_matrix_A_hojas_mean<-scale(matrix_A_hojas_mean, center = FALSE, scale = TRUE)
scaled_matrix_A_raiz_mean<-scale(matrix_A_raiz_mean, center = FALSE, scale = TRUE)
scaled_matrix_N_hojas_mean<-scale(matrix_N_hojas_mean, center = FALSE, scale = TRUE)
scaled_matrix_N_raiz_mean<-scale(matrix_N_raiz_mean, center = FALSE, scale = TRUE)
scaled_matrix_A_organos_mean<-scale(matrix_A_organos_mean, center = FALSE, scale = TRUE)
scaled_matrix_N_organos_mean<-scale(matrix_N_organos_mean, center = FALSE, scale = TRUE)


#Crear matrix para que al graficar me coloree las 10 accesiones mas sensibles y las 10 mas tolerantes
rowCols <-ifelse(rownames(scaled_matrix_A_hojas_mean)=="BdTR1i"|rownames(scaled_matrix_A_hojas_mean)=="Koz-1"|rownames(scaled_matrix_A_hojas_mean)=="Mig3"|rownames(scaled_matrix_A_hojas_mean)=="Per1"|rownames(scaled_matrix_A_hojas_mean)=="Mur1"|rownames(scaled_matrix_A_hojas_mean)=="BdTR10C"|rownames(scaled_matrix_A_hojas_mean)=="BdTR2B"|rownames(scaled_matrix_A_hojas_mean)=="Gaz-8"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11I"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11A", " darkorchid4",ifelse (rownames(scaled_matrix_A_hojas_mean)=="Foz1"|rownames(scaled_matrix_A_hojas_mean)=="Adi-2"|rownames(scaled_matrix_A_hojas_mean)=="ABR2"|rownames(scaled_matrix_A_hojas_mean)=="Bd3-1"|rownames(scaled_matrix_A_hojas_mean)=="RON2"|rownames(scaled_matrix_A_hojas_mean)=="ABR4"|rownames(scaled_matrix_A_hojas_mean)=="ABR6"|rownames(scaled_matrix_A_hojas_mean)=="ABR8"|rownames(scaled_matrix_A_hojas_mean)=="Tek-4"|rownames(scaled_matrix_A_hojas_mean)=="ABR5","yellow4","white"))
#Nota: como el orden de las accesiones en todas las matrices es el mismo, este rowCols me sirve para todas.


#Graficar el Heatmap:
heatmap.2(scaled_matrix_A_hojas_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="LEAF-AMMONIUM NUTRITION",trace="none")
heatmap.2(scaled_matrix_A_raiz_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="ROOT-AMMONIUM NUTRITION",trace="none")
heatmap.2(scaled_matrix_N_hojas_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="LEAF-NITRATE NUTRITION",trace="none")
heatmap.2(scaled_matrix_N_raiz_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="LEAF-NITRATE NUTRITION",trace="none")

#para la grafica de los dos organos debo antes crear una matriz de colores
rowCols2 <-ifelse(rownames(scaled_matrix_A_organos_mean)=="BdTR1i_leaves"|rownames(scaled_matrix_A_organos_mean)=="Koz-1_leaves"|rownames(scaled_matrix_A_organos_mean)=="Mig3_leaves"|rownames(scaled_matrix_A_organos_mean)=="Per1_leaves"|rownames(scaled_matrix_A_organos_mean)=="Mur1__leaves"|rownames(scaled_matrix_A_organos_mean)=="BdTR10C__leaves"|rownames(scaled_matrix_A_organos_mean)=="BdTR2B_leaves"|rownames(scaled_matrix_A_organos_mean)=="Gaz-8_leaves"|rownames(scaled_matrix_A_organos_mean)=="BdTR11I_leaves"|rownames(scaled_matrix_A_organos_mean)=="BdTR11A_leaves"|rownames(scaled_matrix_A_organos_mean)=="BdTR1i_Root"|rownames(scaled_matrix_A_organos_mean)=="Koz-1_Root"|rownames(scaled_matrix_A_organos_mean)=="Mig3_Root"|rownames(scaled_matrix_A_organos_mean)=="Per1_Root"|rownames(scaled_matrix_A_organos_mean)=="Mur1__Root"|rownames(scaled_matrix_A_organos_mean)=="BdTR10C__Root"|rownames(scaled_matrix_A_organos_mean)=="BdTR2B_Root"|rownames(scaled_matrix_A_organos_mean)=="Gaz-8_Root"|rownames(scaled_matrix_A_organos_mean)=="BdTR11I_Root"|rownames(scaled_matrix_A_organos_mean)=="BdTR11A_Root", " darkorchid4",ifelse (rownames(scaled_matrix_A_organos_mean)=="Foz1_leaves"|rownames(scaled_matrix_A_organos_mean)=="Adi-2_leaves"|rownames(scaled_matrix_A_organos_mean)=="ABR2_leaves"|rownames(scaled_matrix_A_organos_mean)=="Bd3-1_leaves"|rownames(scaled_matrix_A_organos_mean)=="RON2_leaves"|rownames(scaled_matrix_A_organos_mean)=="ABR4_leaves"|rownames(scaled_matrix_A_organos_mean)=="ABR6_leaves"|rownames(scaled_matrix_A_organos_mean)=="ABR8_leaves"|rownames(scaled_matrix_A_organos_mean)=="Tek-4_leaves"|rownames(scaled_matrix_A_organos_mean)=="ABR5_leaves"|rownames(scaled_matrix_A_organos_mean)=="Foz1_Root"|rownames(scaled_matrix_A_organos_mean)=="Adi-2_Root"|rownames(scaled_matrix_A_organos_mean)=="ABR2_Root"|rownames(scaled_matrix_A_organos_mean)=="Bd3-1_Root"|rownames(scaled_matrix_A_organos_mean)=="RON2_Root"|rownames(scaled_matrix_A_organos_mean)=="ABR4_Root"|rownames(scaled_matrix_A_organos_mean)=="ABR6_Root"|rownames(scaled_matrix_A_organos_mean)=="ABR8_Root"|rownames(scaled_matrix_A_organos_mean)=="Tek-4_Root"|rownames(scaled_matrix_A_organos_mean)=="ABR5_Root","yellow4","white"))
heatmap.2(scaled_matrix_A_organos_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,RowSideColors=rowCols2, main="AMMONIUM NUTRITION",trace="none")
heatmap.2(scaled_matrix_N_organos_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols2, main="NITRATE NUTRITION",trace="none")



#Graficar el Heatmap con los datos del fold change(log2(A/N))
#Nota: esta grafica es buena para indicarque fenotipos estan mas expresados en las plantas de amonio y nitrato
#Nota2: no la pondria en el articulo porque conclusiones similares puedo extraerde las PCAs
FC_hojas<-log2(matrix_A_hojas_mean)-log2(matrix_N_hojas_mean)
heatmap.2(FC_hojas,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="grafica no publicable",trace="none")
FC_raiz<-log2(matrix_A_raiz_mean)-log2(matrix_N_raiz_mean)
heatmap.2(FC_raiz,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="grafica no publicable", trace="none")

#Graficar el Heatmap con los datos del fold change estandarizado
scaled_FC_hojas<-scale(FC_hojas, center = FALSE, scale = TRUE)
heatmap.2(scaled_FC_hojas,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="LEAF FOLD CHANGE AMMONIUM/NITRATE NUTRITION", trace="none")
scaled_FC_raiz<-scale(FC_raiz, center = FALSE, scale = TRUE)
heatmap.2(scaled_FC_raiz,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="ROOT FOLD CHANGE AMMONIUM/NITRATE NUTRITION", trace="none")