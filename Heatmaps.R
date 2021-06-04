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

#Separar los datos de amonio
A_hojas_mean<-subset(hojas_mean, Source=="A")
A_raiz_mean<-subset(raiz_mean, Source=="A")

#Nombre de los rows
rownames(A_hojas_mean)=A_hojas_mean$Accesion



#convertir a matrix
matrix_A_hojas_mean<-as.matrix(A_hojas_mean[,c(-1:-7,-23,-31,-32)])
#Trabajar con los datos estandarizados por accesiones
scaled_matrix_A_hojas_mean<-t(scale(t(matrix_A_hojas_mean)))

Heatmap(scaled_matrix_A_hojas_mean, row_names_gp = gpar(fontsize = 4), scale = "none")
heatmap.2(scaled_matrix_A_hojas_mean, Colv = FALSE, dendrogram="row", scale = "none", col="bluered", row_names_gp = gpar(fontsize = 2), lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ) )


library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))
col_fun(seq(-3, 3))

