##########################################
#
#   L. Giraldo-Kalil 19 marzo 2021 
# 
# Relaciones pareadas de escalamiento entre rasgos funcionales de 
# cuatro especies de Damburneya
#
#
##########################################


library(here)
library(readxl)
library(smatr)

#Cargar archivo

datos <- read_excel(here("Datos","Finales", "Sp_trait_values.xlsx"))
colnames(datos)


str(datos)

#convertir en factores variables categoricas
tmp_factor <- c("species","Altitude", "Plot", "Id_Tree")
datos[tmp_factor] <- lapply(datos[tmp_factor], factor)


str(datos)
View(datos)

# Convartir datos en data.frame
datos<-as.data.frame(datos)
str(datos)



#####################################
######################################

# Hacer análisis pareados de correlaciones 
#con el paquete smatr usando escala logaritmica
#Se crean también gráficos para revisar el análisis

#_________LNmass

LNmass_LPmass= sma(LNmass ~ LPmass * species, log = "xy",
                   robust=T,
                   data = datos)
summary(LNmass_LPmass) 
plot(LNmass_LPmass)
plot(LNmass_LPmass, which="residual")

LNmass_SLA= sma(LNmass ~ SLA * species, log = "xy",
                   robust=T,
                   data = datos)
summary(LNmass_SLA)
plot(LNmass_SLA)
plot(LNmass_SLA, which="residual")


LNmass_LDMC= sma(LNmass ~ LDMC * species, log = "xy",
                 robust=T,
                 data = datos)
summary(LNmass_LDMC)
plot(LNmass_LDMC)
plot(LNmass_LDMC, which="residual")



#____________LPmass

LPmass_SLA= sma(LPmass ~ SLA * species, log = "xy",
                robust=T,
                data = datos)
summary(LPmass_SLA)
plot(LPmass_SLA)
plot(LPmass_SLA, which="residual")


LPmass_LDMC= sma(LPmass ~ LDMC * species, log = "xy",
                 robust=T,
                 data = datos)
summary(LPmass_LDMC)
plot(LPmass_LDMC)
plot(LPmass_LDMC, which="residual")


LPmass_LNmass= sma(LPmass ~ LNmass * species, log = "xy",
                   robust=T,
                   data = datos)
summary(LPmass_LNmass)
plot(LPmass_LNmass)
plot(LPmass_LNmass, which="residual")
#________________LeafN:P

LeafNP_SLA= sma(LeafNP ~ SLA * species, log = "xy",
               robust=T,
               data = datos)
summary(LeafNP_SLA)
plot(LeafNP_SLA)
plot(LeafNP_SLA, which="residual")


LeafNP_LDMC= sma(LeafNP ~ LDMC * species, log = "xy",
                 robust=T,
                 data = datos)
summary(LeafNP_LDMC)
plot(LeafNP_LDMC, which="residual")


LeafNP_LNmass= sma(LeafNP ~ LNmass * species, log = "xy",
                   robust=T,
                   data = datos)
summary(LeafNP_LNmass)
plot(LeafNP_LNmass)
plot(LeafNP_LNmass, which="residual")

LeafNP_LPmass= sma(LeafNP ~ LPmass * species, log = "xy",
                   robust=T,
                   data = datos)
summary(LeafNP_LPmass)
plot(LeafNP_LPmass)
plot(LeafNP_LPmass, which="residual")
#___________________SLA

SLA_LNmass= sma(SLA ~ LNmass * species, log = "xy",
                robust=T,
                data = datos)
summary(SLA_LNmass)
plot(SLA_LNmass)
plot(SLA_LNmass, which="residual")

SLA_LPmass= sma(SLA ~ LPmass * species, log = "xy",
                robust=T,
                data = datos)
summary(SLA_LPmass)
plot(SLA_LPmass)
plot(SLA_LPmass, which="residual")


SLA_LeafNP= sma(SLA ~ LeafNP * species, log = "xy",
                robust=T,
                data = datos)
summary(SLA_LeafNP)
plot(SLA_LeafNP)
plot(SLA_LeafNP, which="residual")

#_________________________________
LDMC_SLA= sma(LDMC ~ SLA * species, log = "xy",
                 robust=T,
                 data = datos)
summary(LDMC_SLA)
plot(LDMC_SLA)
plot(LDMC_SLA, which="residual")


LDMC_LNmass= sma(LDMC ~ LNmass * species, log = "xy",
                robust=T,
                data = datos)
summary(LDMC_LNmass)
plot(LDMC_LNmass)
plot(LDMC_LNmass, which="residual")

LDMC_LPmass= sma(LDMC ~ LPmass * species, log = "xy",
                robust=T,
                data = datos)
summary(LDMC_LPmass)
plot(LDMC_LPmass)
plot(LDMC_LPmass, which="residual")


LDMC_LeafNP= sma(LDMC ~ LeafNP * species, log = "xy",
                robust=T,
                data = datos)
summary(LDMC_LeafNP)
plot(LDMC_LeafNP)
plot(LDMC_LeafNP, which="residual")
#___________Adicionales


#_ EXPORTAR RESULTADOS

sink(here("Resultados","Scalingtraits_AppendixS4.csv"))

summary(LNmass_LPmass)
summary(LNmass_SLA)
summary(LNmass_LDMC)
summary(LPmass_SLA)
summary(LPmass_LDMC)
summary(LPmass_LNmass)
summary(LeafNP_SLA)
summary(LeafNP_LDMC)
summary(LeafNP_LNmass)
summary(LeafNP_LPmass)
summary(SLA_LNmass)
summary(SLA_LPmass)
summary(SLA_LeafNP)
summary(LDMC_LNmass)
summary(LDMC_LPmass)
summary(LDMC_LeafNP)
summary(LDMC_SLA)

sink()



