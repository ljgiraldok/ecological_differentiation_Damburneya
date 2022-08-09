###################################################################
#   23 Septiembre 2020  Laura Giraldo
#
#   Script para reorganizar los valores de los rasgos por parcela
#   y especie usando un formato largo de los datos, e incluyendo
#   los valores de suelo
#
###################################################################


# Cargar librerias
library(here)
library(tidyr)
library(dplyr)
library(readxl)
library(xlsx)


# Cargar archivo

datos <- read_excel(here("Datos","Finales", "Sp_trait_values.xlsx"))
colnames(rasgos)
        

#saco primero un resumen de las abundancias
abund <- datos %>% group_by(species, Plot) %>%
  select("species", "Plot",
         "sp_plot_abundance", "total_plot_abundance", "prop_sp_ab_plot", 
         "abs_abund_sp", "rel_abund") %>%
  summarize_if(is.numeric, mean)

View(abund)
abund$tmp_key <- paste(abund$Plot,"_",abund$species)
View(abund)





# Excluir columnas que no son necesarias
colnames(datos)

datos <- datos[ , !(names(datos) %in% c("Id", "prop_sp_ab_plot", "Elevation_corrected",
                                        "abs_abund_sp", "rel_abund",
                                        "DBH","Height","LFM","LDW",
                                        "SoilDepth","Id_Tree","Locality",
                                        "Collector_number","mean_plot_elev",
                                        "Genus","Clade" ,"ID_GPS_tmp", "Latitude_corrected",
                                        "Longitude_corrected","SoilDepth",
                                        "Soil_NO3NH4ratio"))]
colnames(datos)
dim(datos)

# Formato largo


datos_tmp <- datos%>%                               # Summary by group using dplyr
  group_by(species, Plot) %>% 
  summarize(LDMC = mean(LDMC),
            LA=mean(LA),
            SLA=mean(SLA),
            LNmass=mean(LNmass),
            LPmass=mean(LPmass),
            LeafNP=mean(LeafNP) )

View(datos_tmp)
datos2 <- gather_(datos_tmp,
        key_col = "Trait",
        value_col = "Trait_value",
        gather_cols = c("LDMC","LA","SLA","LNmass",
                        "LPmass","LeafNP")
        )

dim(datos2)
View(datos2)


#write.xlsx(datos2, here("Datos","Mean_trait_values.xlsx"))

