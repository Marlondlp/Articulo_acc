##subir archivo de datos
DatosBurdeosDMdef12_05_2021 <- read_excel("C:/Users/malon/Dropbox/Tesis PhD/Tesis escrita/Datos Burdeos/DatosBurdeosDMdef12.05.2021.xlsx", 
                                          +     sheet = "DatosBurdeosDMdef")


#Cargar librerias
library("gplots")
library(tidyr)
library(ComplexHeatmap)

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

#Trabajar con los datos estandarizados por accesiones
scaled_matrix_A_hojas_mean<-scale(sqrt(matrix_A_hojas_mean), center = TRUE, scale = TRUE)

Heatmap(scaled_matrix_A_hojas_mean, row_names_gp = gpar(fontsize = 4), scale = "none")


rowCols <- ifelse(rownames(scaled_matrix_A_hojas_mean)=="Koz-3"|rownames(scaled_matrix_A_hojas_mean)=="BdTR12c"|rownames(scaled_matrix_A_hojas_mean)=="Arn1"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11G"|rownames(scaled_matrix_A_hojas_mean)=="S8iiC"|rownames(scaled_matrix_A_hojas_mean)=="BdTR8i"|rownames(scaled_matrix_A_hojas_mean)=="ABR7"|rownames(scaled_matrix_A_hojas_mean)=="BdTR5I"|rownames(scaled_matrix_A_hojas_mean)=="Mon3"|rownames(scaled_matrix_A_hojas_mean)=="BdTR3C"|rownames(scaled_matrix_A_hojas_mean)=="BdTR13C"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11I"|rownames(scaled_matrix_A_hojas_mean)=="Gaz-8"|rownames(scaled_matrix_A_hojas_mean)=="BdTR2B"|rownames(scaled_matrix_A_hojas_mean)=="BdTR10C"|rownames(scaled_matrix_A_hojas_mean)=="Mur1"|rownames(scaled_matrix_A_hojas_mean)=="Per1"|rownames(scaled_matrix_A_hojas_mean)=="Mig3"|rownames(scaled_matrix_A_hojas_mean)=="Koz-1"|rownames(scaled_matrix_A_hojas_mean)=="BdTR1i","purple","darkorchid")
rowCols <-ifelse(rownames(scaled_matrix_A_hojas_mean)=="BdTR1i"|rownames(scaled_matrix_A_hojas_mean)=="Koz-1"|rownames(scaled_matrix_A_hojas_mean)=="Mig3"|rownames(scaled_matrix_A_hojas_mean)=="Per1"|rownames(scaled_matrix_A_hojas_mean)=="Mur1"|rownames(scaled_matrix_A_hojas_mean)=="BdTR10C"|rownames(scaled_matrix_A_hojas_mean)=="BdTR2B"|rownames(scaled_matrix_A_hojas_mean)=="Gaz-8"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11I"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11A", "darkorchid4",ifelse(rownames(scaled_matrix_A_hojas_mean)=="Bd18-1"|rownames(scaled_matrix_A_hojas_mean)=="BdTR13C"|rownames(scaled_matrix_A_hojas_mean)=="BdTR13a"|rownames(scaled_matrix_A_hojas_mean)=="BdTR3C"|rownames(scaled_matrix_A_hojas_mean)=="Mon3"|rownames(scaled_matrix_A_hojas_mean)=="Adi-12"|rownames(scaled_matrix_A_hojas_mean)=="BdTR5"|rownames(scaled_matrix_A_hojas_mean)=="Kah-5"|rownames(scaled_matrix_A_hojas_mean)=="ABR7"|rownames(scaled_matrix_A_hojas_mean)=="Bd21", "darkorchid3" ,ifelse (rownames(scaled_matrix_A_hojas_mean)=="BdTR8i"|rownames(scaled_matrix_A_hojas_mean)=="Kah-1"|rownames(scaled_matrix_A_hojas_mean)=="Bis-1"|rownames(scaled_matrix_A_hojas_mean)=="S8iiC"|rownames(scaled_matrix_A_hojas_mean)=="BdTR2G"|rownames(scaled_matrix_A_hojas_mean)=="BdTR11G","darkorchid2",ifelse (rownames(scaled_matrix_A_hojas_mean)=="Arn1"|rownames(scaled_matrix_A_hojas_mean)=="BdTR9K"|rownames(scaled_matrix_A_hojas_mean)=="Bd30-1"|rownames(scaled_matrix_A_hojas_mean)=="Bd2-3"|rownames(scaled_matrix_A_hojas_mean)=="Bd21-3"|rownames(scaled_matrix_A_hojas_mean)=="BdTR12c","yellow2",ifelse (rownames(scaled_matrix_A_hojas_mean)=="Foz1"|rownames(scaled_matrix_A_hojas_mean)=="Adi-2"|rownames(scaled_matrix_A_hojas_mean)=="ABR2"|rownames(scaled_matrix_A_hojas_mean)=="Bd3-1"|rownames(scaled_matrix_A_hojas_mean)=="RON2"|rownames(scaled_matrix_A_hojas_mean)=="ABR4"|rownames(scaled_matrix_A_hojas_mean)=="ABR6"|rownames(scaled_matrix_A_hojas_mean)=="ABR8"|rownames(scaled_matrix_A_hojas_mean)=="Tek-4"|rownames(scaled_matrix_A_hojas_mean)=="ABR5","yellow4","yellow3")))))


heatmap.2(scaled_matrix_A_hojas_mean,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D2"),cexCol = 0.6,cexRow = 0.5,  RowSideColors=rowCols)



#trabajar con el fold change(A/N)
FC_hoja<-log2(matrix_A_hojas_mean)-log2(matrix_N_hojas_mean)
rowCols <- ifelse(rownames(FC_hoja)=="Koz-3"|rownames(FC_hoja)=="BdTR12c"|rownames(FC_hoja)=="Arn1"|rownames(FC_hoja)=="BdTR11G"|rownames(FC_hoja)=="S8iiC"|rownames(FC_hoja)=="BdTR8i"|rownames(FC_hoja)=="ABR7"|rownames(FC_hoja)=="BdTR5I"|rownames(FC_hoja)=="Mon3"|rownames(FC_hoja)=="BdTR3C"|rownames(FC_hoja)=="BdTR13C"|rownames(FC_hoja)=="BdTR11I"|rownames(FC_hoja)=="Gaz-8"|rownames(FC_hoja)=="BdTR2B"|rownames(FC_hoja)=="BdTR10C"|rownames(FC_hoja)=="Mur1"|rownames(FC_hoja)=="Per1"|rownames(FC_hoja)=="Mig3"|rownames(FC_hoja)=="Koz-1"|rownames(FC_hoja)=="BdTR1i","purple","red")
heatmap.2(FC_hoja,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D"),cexCol = 0.6,cexRow = 0.5,  RowSideColors=rowCols)

scaled_FC_hoja<-scale(FC_hoja, center = TRUE, scale = TRUE)
heatmap.2(scaled_FC_hoja,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D2"),cexCol = 0.6,cexRow = 0.5,  RowSideColors=rowCols)



library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))
col_fun(seq(-3, 3))

