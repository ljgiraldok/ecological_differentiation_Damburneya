############################################################
# Laura Giraldo-Kalil  13 diciembre 2021
#
# Script para hacer un PCA de los rasgos funcionales de 
# Damburneya incluyendo en los puntos la altitud donde se colecto 
# cada muestra.
#
##############################################################

#Primero, llamamos librerias necesarias
library(here)
library(magrittr)
library(purrr) # v. 0.3.2
library(tidyverse)
library(pastecs)
library(ggstatsplot)
library(grid)
library(gridExtra)
library(dplyr)
library(ggpubr)
library(ggplot2) 
library(ggfortify)
library(ggstatsplot)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(readxl)

#Cargar archivo

rasgos <- read_excel(here("Datos","Finales", "Sp_trait_values.xlsx"))
colnames(rasgos)


#Reducir base de datos a variables de interes para 
#hacer analisis de componentes principales (PCA)

reducido<-rasgos[,c("species","Altitude","SLA","LDMC","LNmass","LPmass")] #selecciono variables de interés
colnames(reducido)

str(reducido)
reducido<-as.data.frame(reducido)

# Convertir columnas categoricas en factores
tmp_factor <- c("species","Altitude")

reducido[tmp_factor] <- lapply(reducido[tmp_factor], factor)



#funcion para estandarizacion de los rasgos para hacer PCA
estandarizalo<- function(x) {
  if (is.numeric (x)) { return((x- mean(x)) /sd(x))
  } else {x}}


#Obtener datos de rasgos estandarizados

datos<-data.frame(lapply(reducido,estandarizalo ))
#View(estandarizado)

#Para verificar, revisamos un histograma
hist(datos$SLA)

#verificamos que no hayan datos faltantes
which(is.na(datos))


#Hacer analisis de componentes principales - PCA con datos estandarizados


res.datos<-PCA(datos, quali.sup=1:2,
                  scale.unit = TRUE, graph = TRUE)

#A continuación, revisar listado de resultados
print(res.datos)


#Descripcion de las dimensiones: Correlacion de variables y el primer eje,
# se muestran las distintas de cero en orden de significancia y correlacion

corrsPCA<-dimdesc(res.datos, axes=1:4, proba= 0.05)
corrsPCA

# Exportar resultados (Tabla 1) en un archivo CSV 

write.infile(corrsPCA,sep = ";",here::here( "Resultados","corrs_pca.csv"))

write.infile(res.datos,  sep = ",",
             here::here("Resultados","corr_pca_results.csv"))


#Graficar resultados

#Crear una paleta de color

cbPalette <- c("#D55E00","#0072B2", 
               "#E69F00", "#009E73",
               "#999999", "#CC79A7",
               "#F0E442",  "#56B4E9"
               )

# Grafico general, original (version sometida, con altitudes)

general<- 
  fviz_pca_biplot(res.datos, geom.ind = "point",
                  label="var",
                  habillage = "species",
                  addEllipses = T,
                  elliopse.alpha=0.3,
                  repel = TRUE, 
                  col.var = "black", 
                   arrowsize=0.65, alpha.ind=0.4
  )


#Grafico editado con altitudes

pca_species_biplot<- 
  fviz_pca_biplot(res.datos, 
                  habillage= "species", addEllipses = TRUE,
                  ellipse.type = "norm", ellipse.level=0.95,
                  ellipse.alpha=0.3, label="var", repel = TRUE,
                  pointshape=16, col.var = "black",
                  arrowsize=0.65, alpha.ind=0.6
  )+ 
  scale_color_manual(values=cbPalette)+ 
  scale_fill_manual(values=cbPalette)+  
  theme_classic2()+
  theme(plot.title= element_blank(),
        axis.text = element_text(size = 8),
        axis.title= element_text(size= 9),
        legend.text= element_text (size=8),
        legend.position="bottom",
        legend.box="vertical",
        legend.title= element_text(size= 9),
        panel.border = element_rect(colour = "black", fill=NA))


pca_species_biplot <- ggpubr::ggpar(pca_species_biplot)+
  geom_point(aes(shape=factor(datos$Altitude), color= factor(datos$species),
                 fill=factor(datos$species)))+
  scale_shape_manual(values=c(15,16), name="altitude")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))+
  labs(x="PC1 (50%)", y="PC2 (23%)")


pca_species_biplot

#Para guardar grafico,
ggsave("Figure1.pdf", 
      plot= pca_species_biplot,
       path = "Figuras",
       device= "pdf",
       height=4,
       width=3.5,
       units= "in", dpi=600)










