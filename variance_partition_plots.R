##############################################################
# L. Giraldo-Kalil        22 marzo 2021
#
# script para graficar la particion de varianza (porcentage de variación)
# de los modelos mixtos de las tablas previamente resumidas y generadas
# con el script Mixedmodel_Appendices_S5_S6.R
# Estos datos se incluyeron manualmente en este script
#
##############################################################



library(here)
library(ggplot2)
library(tidyr)
library(dplyr)
library("tidyverse")
library(forcats)



# Crear conjunto de datos con resultados de modelos mixtos
datos_mix <- data.frame (
                  Trait  = c("SLA", "LDMC","LNmass",
                             "LPmass","Leaf N:P"),
                  Plot = c(26,17,44,12,19),
                  Species = c(24,43,27,7,2),
                  Residual = c(49,40,29,81,79),
                  Order = c(5,4,3,2,1))


# arreglar datos en formato largo
longm1<-gather(datos_mix, key="Variance_factor",
                   value= "Values",2:4)

View(longm1)


#Organizar rasgos segun orden deseado
longm1<- longm1 %>%
  mutate(tr = fct_reorder(Trait, Order))

levels(longm1$tr)


#graficar

m1<-ggplot(longm1,aes(x=tr, 
                  y=Values,
                  fill=Variance_factor))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()+ 
  xlab("Leaf traits")+ ylab("% explained variability")+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.key.size = unit(0.8,"line"),
        axis.text = element_text(size = 8),
        axis.title= element_text(size= 9),
        legend.text= element_text (size=8),
        panel.border = element_rect(colour = "black", fill=NA))+
  labs(fill = "relative contribution")+
  scale_fill_grey(start = 0.1, end = 0.9)

m1



ggsave("var_partition_Fig2.pdf", 
       plot= m1,
       path = "Figuras",
       device= "pdf",
       height=3.5,
       width=3.5,
       units= "in", dpi=600)






