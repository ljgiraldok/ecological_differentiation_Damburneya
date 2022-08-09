##############################################################################
#
# L. Giraldo-Kalil        Modificacion: 15 diciembre 2021
# 
#
#   Script para graficar media y error estandar de rasgos 
#   de cuatro especies de Damburneya respecto
#   a suelos, por parcela. Los colores indican la especie,
#   los simbolos de los puntos indican la altitud. Las lineas 
#   negras indican los valores promedio del rasgo por parcela
#
################################################################################

# Cargar librerias y datos
library(ggpubr)
library(ggplot2)
library(here)
library(xlsx)
library(purrr)
library(RColorBrewer)
library(purrr) 
library(tidyverse)
library(plyr) 

datos <- read.xlsx(here("Datos","Finales", "Mean_trait_values.xlsx"),1)

which(is.na(datos))

View(datos)

# reviso un poco de las columnas

#___________________________________________________________________________

#Cargar paleta colorblind-friendly :) -de Okabe e Ito
cbPalette <- c("#D55E00", "#0072B2",
               "#E69F00", "#009E73",
               "#F0E442", "#56B4E9",
               "#CC79A7", "#999999")

#######################################################################
#_____________________________________________________________


#Reorganizo niveles para controlar output en el facet_wrap

# Convertir columnas categoricas en factores
tmp_factor <- c("species","Altitude","Trait","Plot")

datos[tmp_factor] <- lapply(datos[tmp_factor], factor)


#Reorganizo rasgos
neworder <- c("SLA","LDMC","LNmass","LPmass","LeafNP")

rasgos <- arrange(transform(datos,
                           Trait=factor(Trait,levels=neworder)),Trait)
View(rasgos)
levels(rasgos$Trait)



response_dat<-rasgos %>% 
  select(mean_trait_value_plot)


response <- names(response_dat)
response


expl_dat <- rasgos %>% 
  select(Clay,pH,SOC,STN,CNratio,NO3,NH4,STP,SAP)


expl <- names(expl_dat)
expl


########Funcion para graficar


scatter_fun = function(x, y) 
{ggplot(rasgos, aes(x = .data[[x]], 
                    y= weighted_mean_trait_plot
                    ))+
    geom_point(aes(shape=Altitude),
               color= "black", 
               size=1)+
    theme_classic()+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette)+
    scale_shape_manual(values=c(15,16))+
    geom_smooth(aes(y=weighted_mean_trait_plot),
                show.legend = TRUE,
                color= "black", 
                size=1,  method=lm, se=F)+
    geom_smooth(aes(y = sp_mean_trait_value, 
                    color=species),
                    show.legend = TRUE,
                linetype="dashed",
                size=0.7,  method=lm, se=F)+
   # geom_point(aes(y = sp_mean_trait_value, 
    #               color=species),
     #           size=1, shape=16)+
   # geom_smooth(aes(y=mean_trait_values_plot,fill= "species"),
    #            size=0.5, method=lm,linetype="dashed",
     #           se=F)+
    #geom_errorbar(aes(ymin=sp_mean_trait_value-Standard_error_mean, 
     #                 ymax=sp_mean_trait_value+Standard_error_mean),
      #            position=position_dodge(0.05))+
    guides(fill = FALSE)+
    theme(legend.position = c(1, 0),
          legend.box.margin=margin(c(0,0,0,90)),
          legend.box = "horizontal",
          strip.text.x = element_text(hjust = 1, face="bold",
                                      margin=margin(0,0,0,0)),
          strip.background = element_rect(colour=NA),
          strip.placement = "outside")+
    labs(colour = "Species")+
  guides(fill = FALSE)+
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          legend.key.size = unit(0.8,"line"),
          axis.text = element_text(size = 8),
          axis.title= element_text(size= 9),
          legend.text= element_text (size=8),
          legend.box = "horizontal",
          panel.border = element_rect(colour = "black", fill=NA),
          strip.text.x = element_text(hjust = 1, face="bold",
                                      margin=margin(0,0,0,0)),
          strip.background = element_rect(colour=NA),
          strip.placement = "outside")+
    ylab(NULL)+
    facet_wrap(~Trait, scales = "free",
               strip.position = "left", 
               ncol = 1,
               labeller= as_labeller(c(SLA="SLA (cm^2/g)",
                                       LDMC="LDMC(mg/g)",
                                       LNmass="LNmass (mg/g)",
                                       LPmass="LPmass (mg/g)",
                                       LeafNP="Leaf~N:P"),
                                       default = label_parsed))
}

#############################################


all_plots = map(response,
                ~map(expl, scatter_fun, y = .x)
)


#TODAS

a<-all_plots[[1]][[1]]+labs(x="Clay (%)")
b<-all_plots[[1]][[2]]
c<-all_plots[[1]][[3]]+labs(x="SOC (mg/g)")
d<-all_plots[[1]][[4]]+labs(x="STN (mg/g)")
e<-all_plots[[1]][[5]]+labs(x="C:N")
f<-all_plots[[1]][[6]]+labs(x=expression("NO"[3]*" ("*"µg/g)"))
g<-all_plots[[1]][[7]]+labs(x=expression("NH"[4]*" ("*"µg/g)"))
h<-all_plots[[1]][[8]]+labs(x=expression("STP ("*"µg/g)"))
i<-all_plots[[1]][[9]] +labs(x=expression("SAP ("*"µg/g)"))


#STN, NO3,NH4_todos los rasgos
Figure_4<-ggarrange(d,f,g,
                   ncol = 3, nrow = 1, 
                   font.label = list(size = 10),
                   common.legend = TRUE, legend="bottom")


#_Figura Suplementaria:
#SOC_CN_STP___TODOS LOS RASGOS
Appendix_S7<-ggarrange(c,e,h,
                   ncol = 3, nrow = 1, 
                   font.label = list(size = 10),
                   common.legend = TRUE, legend="bottom")

Figure_4
Appendix_S7




ggsave("Figure_4.pdf", 
       plot= Figure_4,
       path = "Figuras",
       device= "pdf",
       height=9,
       width=7.25,
       units= "in", dpi=600)




ggsave("Appendix_S7.pdf", 
       plot= Appendix_S7,
       path = "Figuras",
       device= "pdf",
       height=9,
       width=7.25,
       units= "in", dpi=600)



# Para figuras de SAP, clay, pH por tipos de rasgos


#nutrientes foliares
scatter_fun_nut = function(x, y) 
{ggplot(subset(rasgos, Trait %in% c("LNmass", "LPmass", "LeafNP")),
               aes(x = .data[[x]], 
                   y= weighted_mean_trait_plot))+
    geom_point(aes(shape=Altitude),
               color= "black", 
               size=1)+
    theme_classic()+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette)+
    scale_shape_manual(values=c(15,16))+
    geom_smooth(aes(y=weighted_mean_trait_plot),
                show.legend = TRUE,
                color= "black", 
                size=1,  method=lm, se=F)+
    geom_smooth(aes(y = sp_mean_trait_value, 
                    color=species),
                show.legend = TRUE,
                linetype="dashed",
                size=0.7,  method=lm, se=F)+
    guides(fill = FALSE)+
    theme(legend.position = c(1, 0),
          legend.box.margin=margin(c(0,0,0,90)),
          legend.box = "horizontal",
          strip.text.x = element_text(hjust = 1, face="bold",
                                      margin=margin(0,0,0,0)),
          strip.background = element_rect(colour=NA),
          strip.placement = "outside")+
    labs(colour = "Species")+
    guides(fill = FALSE)+
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          legend.key.size = unit(0.8,"line"),
          axis.text = element_text(size = 8),
          axis.title= element_text(size= 9),
          legend.text= element_text (size=8),
          legend.box = "horizontal",
          panel.border = element_rect(colour = "black", fill=NA),
          strip.text.x = element_text(hjust = 1, face="bold",
                                      margin=margin(0,0,0,0)),
          strip.background = element_rect(colour=NA),
          strip.placement = "outside")+
    ylab(NULL)+
    facet_wrap(~Trait, scales = "free",
               strip.position = "left", 
               ncol = 1,
               labeller= as_labeller(c(SLA="SLA (cm^2/g)",
                                       LDMC="LDMC(mg/g)",
                                       LNmass="LNmass (mg/g)",
                                       LPmass="LPmass (mg/g)",
                                       LeafNP="Leaf~N:P"),
                                     default = label_parsed))
}

#############################################


all_plots_nut = map(response,
                ~map(expl, scatter_fun_nut, y = .x)
)

#Subplots: 
j<-all_plots_nut[[1]][[1]]+labs(x="Clay (%)")
k<-all_plots_nut[[1]][[2]]
l<-all_plots_nut[[1]][[9]] +labs(x=expression("SAP ("*"µg/g)"))


###########

#SAP_pH_clay___Nutrientes foliares
Figure_3<-ggarrange(l,k,j,
                   ncol = 3, nrow = 1, 
                   font.label = list(size = 10),
                   common.legend = TRUE, legend="bottom")

Figure_3

ggsave("Figure_3.pdf", 
       plot= Figure_3,
       path = "Figuras",
       device= "pdf",
       height=6,
       width=7.25,
       units= "in", dpi=600)


###################

#morfol?gicos
scatter_fun_morf = function(x, y) 
{ggplot(subset(rasgos, Trait %in% c("SLA", "LDMC")),
        aes(x = .data[[x]], 
            y = weighted_mean_trait_plot))+
    geom_point(aes(shape=Altitude),
               color= "black", 
               size=1)+
    theme_classic()+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette)+
    scale_shape_manual(values=c(15,16))+
    geom_smooth(aes(y=weighted_mean_trait_plot),
                show.legend = TRUE,
                color= "black", 
                size=1,  method=lm, se=F)+
    geom_smooth(aes(y = sp_mean_trait_value, 
                    color=species),
                show.legend = TRUE,
                linetype="dashed",
                size=0.7,  method=lm, se=F)+
       guides(fill = FALSE)+
    theme(legend.position = c(1, 0),
          legend.box.margin=margin(c(0,0,0,90)),
          legend.box = "horizontal",
          strip.text.x = element_text(hjust = 1, face="bold",
                                      margin=margin(0,0,0,0)),
          strip.background = element_rect(colour=NA),
          strip.placement = "outside")+
    labs(colour = "Species")+
    guides(fill = FALSE)+
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          legend.key.size = unit(0.8,"line"),
          axis.text = element_text(size = 8),
          axis.title= element_text(size= 9),
          legend.text= element_text (size=8),
          legend.box = "horizontal",
          panel.border = element_rect(colour = "black", fill=NA),
          strip.text.x = element_text(hjust = 1, face="bold",
                                      margin=margin(0,0,0,0)),
          strip.background = element_rect(colour=NA),
          strip.placement = "outside")+
    ylab(NULL)+
    facet_wrap(~Trait, scales = "free",
               strip.position = "left", 
               ncol = 1,
               labeller= as_labeller(c(SLA="SLA (cm^2/g)",
                                       LDMC="LDMC(mg/g)",
                                       LNmass="LNmass (mg/g)",
                                       LPmass="LPmass (mg/g)",
                                       LeafNP="Leaf~N:P"),
                                     default = label_parsed))
}

#############################################


all_plots_morf = map(response,
                    ~map(expl, scatter_fun_morf, y = .x)
)

#Subplots: 
m<-all_plots_morf[[1]][[1]]+labs(x="Clay (%)")
n<-all_plots_morf[[1]][[2]]
o<-all_plots_morf[[1]][[9]] +labs(x=expression("SAP ("*"µg/g)"))


#Figura suplementaria
#SAP_pH_clay___Nutrientes foliares

Appendix_S8<-ggarrange(o,n,m,
                   ncol = 3, nrow = 1, 
                   font.label = list(size = 10),
                   common.legend = TRUE, legend="bottom")

Appendix_S8

ggsave("Appendix_S8.pdf", 
       plot= Appendix_S8,
       path = "Figuras",
       device= "pdf",
       height=4,
       width=7.25,
       units= "in", dpi=600)

