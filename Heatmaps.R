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


#Nombre de los rows
rownames(A_hojas_mean)=A_hojas_mean$Accesion
rownames(A_raiz_mean)=A_raiz_mean$Accesion
rownames(N_hojas_mean)=N_hojas_mean$Accesion

#convertir a matrix con datos sin biomasa
matrix_A_hojas_mean<-as.matrix(A_hojas_mean[,c(-1:-7,-23,-31,-32)])
matrix_A_raiz_mean<-as.matrix(A_raiz_mean[,c(-1:-8,-18,-23,-31,-32)])
matrix_N_hojas_mean<-as.matrix(N_hojas_mean[,c(-1:-7,-23,-31,-32)])
matrix_N_raiz_mean<-as.matrix(N_raiz_mean[,c(-1:-8,-18,-23,-31,-32)])
matrix_A_Total_mean<-cbind(matrix_A_hojas_mean,matrix_A_raiz_mean)
colnames(matrix_A_Total_mean)<-c("Chl_Leaf","NH4+_Leaf","Protein_Leaf","Amino acids_Leaf","Nitrate_Leaf","Fruc_Leaf","Gluc_Leaf","Suc_Leaf","Glu_Leaf","Malate_Leaf","Starch_Leaf","Citrate_Leaf","CS_Leaf","CSm_Leaf","GDH_Leaf","MDH_Leaf","ICDH_Leaf","PEPC_Leaf","PK_Leaf","FK_Leaf","GK_Leaf","Glutathione_Leaf","NH4+_Root","Protein_Root","Amino acids_Root","Nitrate_Root","Fruc_Root","Gluc_Root","Suc_Root","Glu_Root","Malate_Root","Citrate_Root","CS_Root","CSm_Root","GDH_Root","MDH_Root","ICDH_Root","PEPC_Root","PK_Root","FK_Root","GK_Root","Glutathione_Root")
matrix_N_Total_mean<-cbind(matrix_N_hojas_mean,matrix_N_raiz_mean)
colnames(matrix_N_Total_mean)<-c("Chl_Leaf","NH4+_Leaf","Protein_Leaf","Amino acids_Leaf","Nitrate_Leaf","Fruc_Leaf","Gluc_Leaf","Suc_Leaf","Glu_Leaf","Malate_Leaf","Starch_Leaf","Citrate_Leaf","CS_Leaf","CSm_Leaf","GDH_Leaf","MDH_Leaf","ICDH_Leaf","PEPC_Leaf","PK_Leaf","FK_Leaf","GK_Leaf","Glutathione_Leaf","NH4+_Root","Protein_Root","Amino acids_Root","Nitrate_Root","Fruc_Root","Gluc_Root","Suc_Root","Glu_Root","Malate_Root","Citrate_Root","CS_Root","CSm_Root","GDH_Root","MDH_Root","ICDH_Root","PEPC_Root","PK_Root","FK_Root","GK_Root","Glutathione_Root")


#Trabajar con los datos escalados de los fenotipos

scaled_matrix_A_hojas_mean<-scale(matrix_A_hojas_mean, center = FALSE, scale = TRUE)
scaled_matrix_A_raiz_mean<-scale(matrix_A_raiz_mean, center = FALSE, scale = TRUE)
scaled_matrix_N_hojas_mean<-scale(matrix_N_hojas_mean, center = FALSE, scale = TRUE)
scaled_matrix_N_raiz_mean<-scale(matrix_N_raiz_mean, center = FALSE, scale = TRUE)
scaled_matrix_A_Total_mean<-scale(matrix_A_Total_mean, center = FALSE, scale = TRUE)
scaled_matrix_N_Total_mean<-scale(matrix_N_Total_mean, center = FALSE, scale = TRUE)


#Crear matrix para que al graficar me coloree las 10 accesiones mas sensibles y las 10 mas tolerantes
rowCols <-ifelse(rownames(scaled_matrix_A_hojas_mean)=="BdTR1i"|rownames(scaled_matrix_A_hojas_mean)=="Koz-1"|rownames(scaled_matrix_A_hojas_mean)=="Mig3"|rownames(scaled_matrix_A_hojas_mean)=="Per1"|rownames(scaled_matrix_A_hojas_mean)=="Mur1"|rownames(scaled_matrix_A_hojas_mean)=="BdTR10C"|rownames(scaled_matrix_A_hojas_mean)=="BdTR2B"|rownames(scaled_matrix_A_hojas_mean)=="Gaz-8"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11I"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11A", " darkorchid4",ifelse (rownames(scaled_matrix_A_hojas_mean)=="Foz1"|rownames(scaled_matrix_A_hojas_mean)=="Adi-2"|rownames(scaled_matrix_A_hojas_mean)=="ABR2"|rownames(scaled_matrix_A_hojas_mean)=="Bd3-1"|rownames(scaled_matrix_A_hojas_mean)=="RON2"|rownames(scaled_matrix_A_hojas_mean)=="ABR4"|rownames(scaled_matrix_A_hojas_mean)=="ABR6"|rownames(scaled_matrix_A_hojas_mean)=="ABR8"|rownames(scaled_matrix_A_hojas_mean)=="Tek-4"|rownames(scaled_matrix_A_hojas_mean)=="ABR5","yellow4","white"))
#Nota: como el orden de las accesiones en todas las matrices es el mismo, este rowCols me sirve para todas.


#Graficar el Heatmap:
heatmap.2(scaled_matrix_A_hojas_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="LEAF-AMMONIUM NUTRITION",trace="none")
heatmap.2(scaled_matrix_A_raiz_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="ROOT-AMMONIUM NUTRITION",trace="none")
heatmap.2(scaled_matrix_N_hojas_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="LEAF-NITRATE NUTRITION",trace="none")
heatmap.2(scaled_matrix_N_raiz_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="ROOT-NITRATE NUTRITION",trace="none")
heatmap.2(scaled_matrix_A_Total_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="AMMONIUM NUTRITION",trace="none")
heatmap.2(scaled_matrix_N_Total_mean,  scale = "col", col=gamadecolores,cexCol = 0.6,cexRow = 0.6,  RowSideColors=rowCols, main="NITRATE NUTRITION",trace="none")
