######################################################
#
#   L. Giraldo-Kalil        Modificado 01 abril 2021
# 
#   Script para hacer modelos mixtos con los rasgos funcionales y suelos de los
#   arboles de Damburneya muestreados en parcelas. 
#   Se basa en datos estandarizados para facilitar la interpretacion
#
#   Varias de las fuentes consultadas para crear este script pueden verse al final
#
######################################################


library(MASS)
library(car)
library(lme4)
library(dplyr)
library(lattice)
library(purrr) 
library(ggplot2)
library(patchwork) 
library(broom)
library(tidyr)
library(sjPlot)
library(lmerTest)
library(sjlabelled)
library(here)
library(readxl)

#Cargar archivo

rasgos <- read_excel(here("Datos","Finales", "Sp_trait_values.xlsx"))
colnames(rasgos)

#convertir en factores columnas categoricas 

tmp_factor <- colnames(rasgos)[1:12]

#convertir en caracteres columnas numericas ordinales o de datos diferentes a los rasgos

rasgos[tmp_factor] <- lapply(rasgos[tmp_factor], factor)


#Excluir columnas que no se usaran

rasgos<-rasgos[,c("species","Altitude","Plot","SLA","LDMC",
                  "LNmass","LPmass", "LeafNP" )] #selecciono variables de interés
colnames(rasgos)



#funcion para estandarizacion de los rasgos

estandarizalo<- function(x) {
  if (is.numeric (x)) { return((x- mean(x)) /sd(x))
  } else {x}}


#Obtener datos de rasgos estandarizados

datos<-data.frame(lapply(rasgos,estandarizalo))


#View(estandarizado)

colnames(datos)
str(datos)
nrow(datos)


#summary(datos)

#defino mi funcion para aplicar el modelo
#en este caso, especie y parcela son factores aleatorios, y altitud es fijo

modelo_fun = function(response) {
  form = paste(response, "~ Altitude + (1|species) + (1|Plot)") 
  lmer(as.formula(form), REML=TRUE, data = datos)
}
anova(modelo_fun(response="LDMC"))
summary(modelo_fun(response="LDMC")) #si quiero aplicar el modelo a una variable
ranova(modelo_fun(response="LDMC"))

#Defino las variables que usare y a las cuales les aplicare el loop


#Descomposicion de varianza
b<-modelo_fun((response="LDMC"))
Var_Random_effect <- as.numeric(VarCorr(b))
Var_Random_effect
Var_Residual <- attr(VarCorr(b), "sc")^2
Var_Residual
Var_Fix_effect <- var(predict(lm(LPmass~Altitude, datos))) 
Var_Fix_effect

colnames(datos)

#Uso sus nombres para la definicion de variables/modelos
vars = names(datos)[c(4:8)]
vars = set_names(vars)
vars

models = vars %>%
  map(modelo_fun)

#Reviso los modelos construidos
#View(models)


resumen<-lapply(models,summary)
sink(here("Resultados","mixed_summary_Appendix_S6.txt"))
print(resumen)
sink()

randeffects<-lapply(models,rand) #esto es igual a solicitar (ranova)
sink(here("Resultados","randeffects_mixed_lm_Appendix_S6.txt"))
print(randeffects)
sink()

pruebanova<-lapply(models,anova)
sink(here("Resultados","anova_mixed_lm_Appendix_S6.txt"))
print(pruebanova)
sink()


############## Ahora para efectos fijos : cambiar el metodo a
#ML en vez de REML

modelo_fun_ML = function(response) {
  form = paste(response, "~ Altitude + (1|species) + (1|Plot)") 
  lmer(as.formula(form), REML=FALSE, data = datos)
}


models = vars %>%
  map(modelo_fun_ML)
resumen<-lapply(models,summary)
sink(here("Resultados","summary_fix_ML_Appendix_S5.txt"))
print(resumen)
sink()

randeffects<-lapply(models,rand) #esto es igual a solicitar (ranova)
sink(here("Resultados","randeffects_mixed_ML_AppendixS5.txt"))
print(randeffects)
sink()

pruebanova<-lapply(models,anova)
sink(here("Resultados","anovamods_mixed_ML_AppendixS5.txt"))
print(pruebanova)
sink()


# .-.-.-.-.-.-.-.-.-.-.-.-..-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.--.
# Fuentes
#  Una guia muy util:
  #   https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
  #   Revisar tambi?n: Harrison 2018, PeerJ
  #
  #   PAra archivos de salida: Estas son varias opciones, pero a la final us? "broom"
  #   https://strengejacke.github.io/sjPlot/articles/tab_mixed.html
  #   https://mran.microsoft.com/snapshot/2017-04-22/web/packages/sjPlot/vignettes/sjplmer.html
  #
  #   Para lograr mi loop maravilloso, me base en este script maravilloso
  #   Publicado por Ariel Muldoon: 
  #   https://aosmith.rbind.io/2019/07/22/automate-model-fitting-with-loops/
  #   https://aosmith.rbind.io/2019/06/24/function-for-model-fitting/
#
#   Para reporte de valores p y grados de libertad:
#   https://cran.r-project.org/web/packages/lmerTest/index.html
