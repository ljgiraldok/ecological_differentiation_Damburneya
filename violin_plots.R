##############################################################################
#   18 marzo 2021 
#  Modificacion del script para incluir solo LDMC, SLA, Nmass y Pmass   
#
#   03 noviembre 2020    Laura Giraldo
#
#   Script para graficar boxplots de rasgos por especie
#   el 04 marzo 2021: Editado para graficar SE (intervalo de confianza) en lugar de sd 
#
#
###############################################################################

# Cargar librerias y datos
library(ggpubr)
library(ggplot2)
library(here)
library(xlsx)
library(purrr)
library(RColorBrewer)
library(purrr) # v. 0.3.2
library(tidyverse)
library(readxl)

#Cargar archivo

rasgos <- read_excel(here("Datos","Finales", "Sp_trait_values.xlsx"))
colnames(rasgos)


which(is.na(rasgos))

#View(rasgos)

# reviso un poco de las columnas

#___________________________________________________________________________

#Cargar paleta colorblind-friendly :) -de Okabe e Ito
cbPalette <- c("#D55E00", "#0072B2",
               "#E69F00", "#009E73",
               "#F0E442", "#56B4E9",
               "#CC79A7", "#999999")



#######################################################################
#_____________________________________________________________

nrow(rasgos)

#View(rasgos)
colnames(rasgos)

response_dat<-rasgos %>% 
  select(SLA,LDMC,LNmass, LPmass, LeafNP)


response <- names(response_dat)
response

response = set_names(response)
response

#############
# Una funcion para que el violin plot muestre la media , y el maximo y minimo (ya no es necesaria!)

#data_summary <- function(x) {
 # m <- mean(x)
  #ymin <- m-(sd(x)/sqrt(length(x)))
  #ymax <- m+(sd(x)/sqrt(length(x)))
#  return(c(y=m,ymin=ymin,ymax=ymax))
#}




########Funcion para graficar
viol_fun = function(y) 
{ggplot(rasgos, aes(x = species, 
                    y = .data[[y]],
                    fill=species,
                    color=species))+
    geom_violin(trim=TRUE, alpha=0.5, size=0.5)+
    geom_jitter(position=position_jitter(0.1),
                size=1, alpha=0.5)+
    stat_summary(fun = "mean", geom = "point", 
                 fill="black", color="black", size=1.5)+
    geom_errorbar(stat="summary", fun.data="mean_se", # Para intervalo de confianza, multiplicar SE*1.96: 
                  fun.args = list(mult = 1.96), 
                  color="black", width=0.1, size=0.5)+
    theme_classic()+
    theme(axis.text=element_text(size=10),
          legend.position = "bottom",
          legend.key.size = unit(0.8,"line"),
          legend.title=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size= 9),
          legend.text= element_text (size=8),
          panel.border = element_rect(colour = "black", 
                                      fill=NA))+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette)+
    facet_grid(cols = vars(Altitude))
}






#############################################
# todos los graficos juntos
violines <- map(response,viol_fun)

# Edicion por separado de cada grafico

c <-viol_fun("SLA")+
  ylab(bquote('SLA'~(cm^2/g)))+
  theme(legend.position = "none")

d <- viol_fun("LDMC")+
  ylab(bquote('LDMC'~(mg/g)))+
  theme(legend.position = "none")

e <- viol_fun("LNmass")+
  ylab(bquote('LNmass'~(mg/g)))+
  theme(legend.position = "none")

f <- viol_fun("LPmass")+
  ylab(bquote('LPmass'~(mg/g)))+
  ylim(0.1,1.7)+
  theme(legend.position = "none")

f
g <- viol_fun("LeafNP")+
  ylab('Leaf N:P')+theme(legend.position="none")


##### Layout


figura<-ggarrange(c,d,e,f,g,
                   labels = c("A", "B","C","D","E"),
                   ncol = 2, nrow = 3, 
                  hjust = c(-2),
                  vjust = c(2),
                  font.label = list(size = 10),
                  common.legend = TRUE, legend="bottom")

figura


 
ggsave("AppendixS3.pdf", 
                 plot= figura,
       path = "Figuras",
                 device= "pdf",
                    height=6,
                    width=5,
                   units= "in", dpi=600)

