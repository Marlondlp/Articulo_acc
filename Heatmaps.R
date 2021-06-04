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



#convertir a matrix
matrix_A_hojas_mean<-as.matrix(A_hojas_mean[,c(-1:-7,-23,-31,-32)])
#Trabajar con los datos estandarizados por accesiones
scaled_matrix_A_hojas_mean<-scale(sqrt(matrix_A_hojas_mean), center = TRUE, scale = TRUE)

Heatmap(scaled_matrix_A_hojas_mean, row_names_gp = gpar(fontsize = 4), scale = "none")


rowCols <- ifelse(rownames(scaled_matrix_A_hojas_mean)=="Uni2","purple","red")


heatmap.2(scaled_matrix_A_hojas_mean,  scale = "none", col="bluered",hclustfun = function(x) hclust(x, method="ward.D2"),cexCol = 0.6,cexRow = 0.5,  RowSideColors=rowCols)


library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))
col_fun(seq(-3, 3))

