#################################################################
# L. Giraldo-Kalil      Modificaddo 16 noviembre 2020
#
# Script diseñado para graficar porcentaje de variacion explicado 
# segun flexanova. Se basa en los resutlados de este análisis generado con el script
# flexanova_traits_soils.R que se basa en la funcion "flexanova 
# publicada por Leps et al. 2011 (Ecography 34: 856-863)
#
#################################################################

library(here)
library(xlsx)
library(ggplot2)
library(tidyr)
library(dplyr)
library("tidyverse")
library(forcats)
library(lemon)

datos<-read.xlsx(here("Datos", "Finales","weigthed_trait_intra_intersp_var.xlsx"),1)
pvalue<-read.xlsx(here("Datos", "Finales","weigthed_trait_intra_intersp_var.xlsx"),3)

#View(datos)
# arreglar datos en formato largo
longform<-gather(datos, key="relative_contribution",
                 value= "Values",7:10)

#Expresar en porcentage
#View(longform)
longform$relative_contribution<-as.factor(longform$relative_contribution)
longform$Percentage<-(longform$Values)*100

longform<-unite(longform, key,c(7:8), sep="_",remove=F)

#agregar clave completa
longform$key_comp<-paste(longform$key,"_",longform$variance_descriptors)
#View(longform)

#Extraer signos de covariacion

longform$sign<- ifelse(longform$Values<0,"-","+")
longform$csign<-ifelse(longform$relative_contribution!="Covariation"," ",longform$sign)


#View(longform)

#head(longform)

plong<-gather(pvalue, key="relative_contribution",
              value="pvalues", 7:9)
plong<-unite(plong, key, c("key_tmp","relative_contribution"), sep="_", remove=F)
#head(plong)

#agregar clave completa
plong$key_comp<-paste(plong$key,"_",plong$variance_descriptors)
#View(plong)


#juntar las bases
todo<-left_join(longform, plong[,10:11], by="key_comp")

#View(todo)
#head(todo)

#Aqu?, excluyo los residuales y el total de los factores de variacion
rasgos<-subset(todo,todo$variance_descriptors!="Residuals")
rasgos<-subset(rasgos,rasgos$variance_descriptors!="Total")

#Verifico
rasgos$variance_descriptors %>% levels()

View(rasgos)


# agrego simbologia a la significancia
rasgos$significance<-ifelse(rasgos$pvalues<0.05,"*","")

nrow(rasgos)
head(rasgos)
#View(rasgos)

# borro valores feos en la significancia
rasgos$significance<-ifelse(rasgos$significance=="NANANA",
                            NA,rasgos$significance)
#View(rasgos)


#ajustamos para trampear usando el total y la covariacion como nuevas columnas
#Hacemos un subset solo con total y covariacion
Tot<-subset(rasgos,rasgos$relative_contribution=="Total")
cov_<-subset(rasgos,rasgos$relative_contribution=="Covariation")


TotW<-pivot_wider(Tot,names_from= relative_contribution,
                   values_from=Percentage)

cov_<-pivot_wider(cov_,names_from= relative_contribution,
                  values_from=Percentage)

 
#View(TotW)
#View(cov_)

#Hacemos aparte otro subset con las variables que nos interesan
#intra and turnover variation
new<-subset(rasgos,rasgos$relative_contribution=="Intraspec."|
              rasgos$relative_contribution=="Turnover")
all<-left_join(new, TotW[,c(8,11,13,15)], by="key_tmp",
               suffix=c("_var","_T"))
#View(all)
all<-all[,-c(14)]


all<-left_join(all,cov_[,c(8,12,15)], by="key_tmp")
#View(all)
all$significanceT<-ifelse(all$pvalues_T<0.05,"*","")

#quito signos y asteriscos duplicados
all$sign_T<-ifelse(all$relative_contribution=="Turnover",
                       all$sign_T,NA)
all$significanceT<-ifelse(all$relative_contribution=="Turnover",
                   all$significanceT,NA)

all$csign<-ifelse(all$relative_contribution=="Turnover",
                          all$csign,NA)


# Reordenar niveles de variables ambientales
all$group <- factor(all$environmental_variable,      # Reordering group factor levels
                         levels = c("Clay",
                                    "pH",
                                    "SOC",
                                    "STN",
                                    "NO3",
                                    "NH4",
                                    "CNratio",
                                    "STP","SAP"
                                    ))

View(all)


all$Functional_trait<-ifelse(all$Functional_trait=="LeafNP","Leaf N:P",
                             all$Functional_trait)




#View(all)

#Escribir tablas modificadas en formato excel


write.xlsx(all, here("Resultados","flexanova",
                     file="flexanova_var_P.xlsx"))

write.xlsx(longform, here("Resultados",
                          file="flexanova_var_longformat.xlsx"))

#Acotamos a lo que m?s nos interesa
#red<-subset(all,subset = (all$environmental_variable=="Clay"|
  #                         all$environmental_variable=="NO3"|
   #                        all$environmental_variable=="NH4"|
    #                       all$environmental_variable=="SAP"))


#graficamos

all$ord<-ifelse(all$Functional_trait=="SLA",1,
                   ifelse(all$Functional_trait=="LDMC",2,
                          ifelse(all$Functional_trait=="LNmass",3,
                                 ifelse(all$Functional_trait=="LPmass",4,5))))



# Reorganizacion de factores (rasgos)


all <- all %>%
  mutate(trait = fct_reorder(Functional_trait, ord))

levels(all$trait)

#anotacion letras texto
dat_text <- data.frame(
  label = c("A","B","C","D","E","G","H","I"),
  group   = c("Clay","pH","STN","NH4","NO3","CNratio","STP","SAP") 
)




#Graficar usando ggplot2
#gestion de leyendas y ejes: con el paquete lemon


wplot<-ggplot(subset(all, type %in% c("weighted")),
              aes(x=trait, 
                           y=Percentage,
                           fill=relative_contribution))+
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = significance), 
            position = position_stack(vjust = 0.05),
            size=7, color="black")+
    facet_wrap(~group,
               labeller = as_labeller(c(Clay="Clay",
                                        pH="pH",
                                        SOC="SOC",
                                STN="STN",
                                NH4 = "NH[4]",
                                NO3 = "NO[3]",
                                CNratio="C:N",
                                STP="STP",
                                SAP = "SAP"),
                                default = label_parsed))+
  facet_rep_wrap(~group, repeat.tick.labels = T)+
  xlab("Leaf traits")+ ylab("% Explained variability")+
  theme_classic()+
  theme(legend.position="bottom",
        aspect.ratio = 1,
        legend.key.size = unit(0.8,"line"),
        axis.text = element_text(size = 8),
        axis.title= element_text(size= 9),
        axis.text.x = element_text(angle=45,hjust = 1),
        legend.text= element_text (size=8),
        strip.text.x = element_text(hjust = 1, face="bold",
                                    margin=margin(0,0,0,0)),
        strip.background = element_rect(colour=NA),
        strip.placement = "inside",
        strip.text= element_text(size=8),
        panel.border = element_rect(colour = "black", fill=NA))+
  labs(fill = "relative contribution")+
  stat_summary(fun = mean, geom = "errorbar", 
                  aes(x=Functional_trait,
                      y=Total,
                    ymax = ..y.., ymin = ..y..,
                      width = 1, linetype = "solid"),show.legend = F)+
  geom_text(aes(label=csign),position= position_stack(vjust = 1.4),
             color="blue")+
  scale_fill_manual(labels = c("Intraspecific","Interspecific"),
                           values=c("azure4","lightgrey"))+
  labs(fill = "relative contribution")+
  geom_text(aes(label = significanceT), 
            position = position_stack(vjust = 1.5),fontface="bold",
            color="darkgreen", size=7)


  
wplot 


 
ggsave("Appendix_S9.pdf", 
       plot= wplot,
       path = "Figuras",
       device= "pdf",
       height=9,
       width=7.25,
       units= "in", dpi=600)






