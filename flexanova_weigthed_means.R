#####################################
#
#   L. Giraldo -Kalil       modificado -22 marzo 2021-
#   
#   script para hacer la descomposicion de varianza de los modelos de regresión lineal 
#   de la variación local de los rasgos de cuatro especies de Damburneya en respuesta a
#   las variables del suelo. El analisis cuantifica la contribucion de la variación intra
#   e interespecifica de los rasgos en respuesta a cada variable edafica.
#   Para ello usamos la funcion "flexanova" publicada por Leps et al. 2011 
#   Vease la publicación original de la función y el material suplementario disponibles en:
#   Lepš, J., de Bello, F., Šmilauer, P. y Doležal, J. 2011. 
#   Community trait response to environment: disentangling species turnover vs
#   intraspecific trait variability effects. - Ecography 34: 856-863
#
#   Para correr este script es necesario que el usuario descargue el script del material 
#   suplementario de dicho articulo en donde esta almacenada esta funcion, en 
#   este caso se guardo en la carpeta "Scripts" junto al resto de scripts empleados
#   en nuestro articulo
#
####################################

library(here)
library(xlsx)
rasgos <- read.xlsx(here("Datos","Finales", "Mean_trait_values.xlsx"),1)

#Cargar la funcion flexanova de Leps et al. (2011)

source(here("Scripts","e6904_trait-flexanova-v3.r"))

LDMC<-subset(rasgos,rasgos$Trait=="LDMC")
LeafNP<-subset(rasgos,rasgos$Trait=="LeafNP")
LNmass<-subset(rasgos,rasgos$Trait=="LNmass")
LPmass<-subset(rasgos,rasgos$Trait=="LPmass")
SLA<-subset(rasgos,rasgos$Trait=="SLA")

#Aplicar la funcion de acuerdo a cada propiedad del suelo
#________________________Clay


LDMC_Clay<-trait.flex.anova(~Clay,  sp_mean_trait_value, 
                            weighted_mean_trait_plot, data=LDMC)

LeafNP_Clay<-trait.flex.anova(~Clay, sp_mean_trait_value, 
                              weighted_mean_trait_plot, data=LeafNP)

LNmass_Clay<-trait.flex.anova(~Clay, sp_mean_trait_value, 
                              weighted_mean_trait_plot, data=LNmass)

LPmass_Clay<-trait.flex.anova(~Clay, sp_mean_trait_value, 
                              weighted_mean_trait_plot, data=LPmass)

SLA_Clay<-trait.flex.anova(~Clay, sp_mean_trait_value, 
                           weighted_mean_trait_plot, data=SLA)


#________ pH


LDMC_pH<-trait.flex.anova(~pH,  sp_mean_trait_value, 
                          weighted_mean_trait_plot, data=LDMC)

LeafNP_pH<-trait.flex.anova(~pH, sp_mean_trait_value, 
                            weighted_mean_trait_plot, data=LeafNP)

LNmass_pH<-trait.flex.anova(~pH, sp_mean_trait_value, 
                            weighted_mean_trait_plot, data=LNmass)

LPmass_pH<-trait.flex.anova(~pH, sp_mean_trait_value, 
                            weighted_mean_trait_plot, data=LPmass)

SLA_pH<-trait.flex.anova(~pH, sp_mean_trait_value, 
                         weighted_mean_trait_plot, data=SLA)

#___________________SOC


LDMC_SOC<-trait.flex.anova(~SOC,  sp_mean_trait_value, 
                           weighted_mean_trait_plot, data=LDMC)

LeafNP_SOC<-trait.flex.anova(~SOC, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LeafNP)

LNmass_SOC<-trait.flex.anova(~SOC, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LNmass)

LPmass_SOC<-trait.flex.anova(~SOC, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LPmass)

SLA_SOC<-trait.flex.anova(~SOC, sp_mean_trait_value, 
                          weighted_mean_trait_plot, data=SLA)

#______________________STN

LDMC_STN<-trait.flex.anova(~STN,  sp_mean_trait_value, 
                           weighted_mean_trait_plot, data=LDMC)

LeafNP_STN<-trait.flex.anova(~STN, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LeafNP)

LNmass_STN<-trait.flex.anova(~STN, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LNmass)

LPmass_STN<-trait.flex.anova(~STN, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LPmass)

SLA_STN<-trait.flex.anova(~STN, sp_mean_trait_value, 
                          weighted_mean_trait_plot, data=SLA)

#________________NO3

LDMC_NO3<-trait.flex.anova(~NO3,  sp_mean_trait_value, 
                           weighted_mean_trait_plot, data=LDMC)

LeafNP_NO3<-trait.flex.anova(~NO3, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LeafNP)

LNmass_NO3<-trait.flex.anova(~NO3, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LNmass)

LPmass_NO3<-trait.flex.anova(~NO3, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LPmass)

SLA_NO3<-trait.flex.anova(~NO3, sp_mean_trait_value, 
                          weighted_mean_trait_plot, data=SLA)

#__________________________NH4

LDMC_NH4<-trait.flex.anova(~NH4,  sp_mean_trait_value, 
                           weighted_mean_trait_plot, data=LDMC)

LeafNP_NH4<-trait.flex.anova(~NH4, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LeafNP)

LNmass_NH4<-trait.flex.anova(~NH4, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LNmass)

LPmass_NH4<-trait.flex.anova(~NH4, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LPmass)

SLA_NH4<-trait.flex.anova(~NH4, sp_mean_trait_value, 
                          weighted_mean_trait_plot, data=SLA)


#________________________CN ratio


LDMC_CNratio<-trait.flex.anova(~CNratio,  sp_mean_trait_value, 
                               weighted_mean_trait_plot, data=LDMC)

LeafNP_CNratio<-trait.flex.anova(~CNratio, sp_mean_trait_value, 
                                 weighted_mean_trait_plot, data=LeafNP)

LNmass_CNratio<-trait.flex.anova(~CNratio, sp_mean_trait_value, 
                                 weighted_mean_trait_plot, data=LNmass)

LPmass_CNratio<-trait.flex.anova(~CNratio, sp_mean_trait_value, 
                                 weighted_mean_trait_plot, data=LPmass)

SLA_CNratio<-trait.flex.anova(~CNratio, sp_mean_trait_value, 
                              weighted_mean_trait_plot, data=SLA)


#__________________STP

LDMC_STP<-trait.flex.anova(~STP,  sp_mean_trait_value, 
                           weighted_mean_trait_plot, data=LDMC)

LeafNP_STP<-trait.flex.anova(~STP, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LeafNP)

LNmass_STP<-trait.flex.anova(~STP, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LNmass)

LPmass_STP<-trait.flex.anova(~STP, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LPmass)

SLA_STP<-trait.flex.anova(~STP, sp_mean_trait_value, 
                          weighted_mean_trait_plot, data=SLA)

#________________________________SAP

LDMC_SAP<-trait.flex.anova(~SAP,  sp_mean_trait_value, 
                           weighted_mean_trait_plot, data=LDMC)

LeafNP_SAP<-trait.flex.anova(~SAP, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LeafNP)

LNmass_SAP<-trait.flex.anova(~SAP, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LNmass)

LPmass_SAP<-trait.flex.anova(~SAP, sp_mean_trait_value, 
                             weighted_mean_trait_plot, data=LPmass)

SLA_SAP<-trait.flex.anova(~SAP, sp_mean_trait_value, 
                          weighted_mean_trait_plot, data=SLA)



#__________
sink(here("Resultados","flexanova_results_traits_soils.csv"))

print("flexanova of weighted mean trait values per plot")


print("Clay weighted_mean_trait_plot")

print("LDMC:")
LDMC_Clay
print("LeafNP:")
LeafNP_Clay
print("LNmass:")
LNmass_Clay
print("LPmass:")
LPmass_Clay
print("SLA:")
SLA_Clay


print("pH weighted_mean_trait_plot")

print("LDMC:")
LDMC_pH
print("LeafNP:")
LeafNP_pH
print("LNmass:")
LNmass_pH
print("LPmass:")
LPmass_pH
print("SLA:")
SLA_pH

print("SOC weighted_mean_trait_plot")

print("LDMC:")
LDMC_SOC
print("LeafNP:")
LeafNP_SOC
print("LNmass:")
LNmass_SOC
print("LPmass:")
LPmass_SOC
print("SLA:")
SLA_SOC

print("STN weighted_mean_trait_plot")

print("LDMC:")
LDMC_STN
print("LeafNP:")
LeafNP_STN
print("LNmass:")
LNmass_STN
print("LPmass:")
LPmass_STN
print("SLA:")
SLA_STN

print("NO3 weighted_mean_trait_plot")

print("LDMC:")
LDMC_NO3
print("LeafNP:")
LeafNP_NO3
print("LNmass:")
LNmass_NO3
print("LPmass:")
LPmass_NO3
print("SLA:")
SLA_NO3

print("NH4 weighted_mean_trait_plot")

print("LDMC:")
LDMC_NH4
print("LeafNP:")
LeafNP_NH4
print("LNmass:")
LNmass_NH4
print("LPmass:")
LPmass_NH4
print("SLA:")
SLA_NH4


print("CNratio weighted_mean_trait_plot")

print("LDMC:")
LDMC_CNratio
print("LeafNP:")
LeafNP_CNratio
print("LNmass:")
LNmass_CNratio
print("LPmass:")
LPmass_CNratio
print("SLA:")
SLA_CNratio



print("STP weighted_mean_trait_plot")

print("LDMC:")
LDMC_STP
print("LeafNP:")
LeafNP_STP
print("LNmass:")
LNmass_STP
print("LPmass:")
LPmass_STP
print("SLA:")
SLA_STP

print("SAP weighted_mean_trait_plot")

print("LDMC:")
LDMC_SAP
print("LeafNP:")
LeafNP_SAP
print("LNmass:")
LNmass_SAP
print("LPmass:")
LPmass_SAP
print("SLA:")
SLA_SAP


sink()

