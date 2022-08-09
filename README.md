# ecological_differentiation_Damburneya
A Spanish version of this information can be found below                                                                    
Una versión en español de esta información está disponible abajo, despues de la versión en inglés


This repository contains the code used to produce the statistical analyses, figures, tables and supplementary Data of the manuscript "Giraldo-Kalil et al. 2022. Patterns of leaf trait variation underlie ecological differences among sympatric tree species of Damburneya in a tropical rainforest (Am. J. Bot. in press)."


Data are available at:


Please, cite the data properly if you use them.

To replicate the analyses and plots, users should create a project directory to store results. It is highly recommendend to create an R project in this directory, and use the "here" package to automate the paths for saving files there.

For that, create the a folder named "Datos" in the project directory and store there the data.
Other folders named "Figuras" and "Resultados" must be created in the project directory.

A detailed descriptions of scripts function and data required is provided in the table below


Scripts' instructions are in Spanish, my first language.
If you have any doubts, I'll be happy to help.


| Script/Código | Required data/Datos requeridos | Description (English) | Descripción (Español) |
| :---: | :---: | :---: | :---: |
| Data_plot_summary.R | Sp_trait_values.xlsx | Summarizes trait information by plot and organizes it in a longformat. This was necessary to build the data base "Mean_trait_values.xlsx" used to performlinear models between soil properties and mean species plot trait values | Resume la información de los rasgos por parcela y la organiza en un formato largo. Esto fue necesario para construir la base de datos "Mean_trait_values.xlsx" utilizada para realizar modelos lineales entre las propiedades del suelo y los valores medios de los rasgos de las parcelas de las especies |
| flexanova_plots_traits_soils.R | weigthed_trait_intra_intersp_var.xlsx | Creates the plot shown in Appendix S9 of the intra and interspecific contribution to local trait variance | Crea el gráfico mostrado en el Apéndice S9 de la contribución intra e interespecífica a la varianza del rasgo local en respuesta a las propiedades del suelo |
| flexanova_weighted_means.R | Mean_trait_values.xlsx | Performs the analysis of variance of traits in response to plot edaphic properties. Generates the data contained in the file "weighted_trait_intra_intersp_var.xlsx" necessary for the Appendix S9 | Realiza el análisis de la varianza de los rasgos en respuesta a las propiedades edáficas de la parcela. Genera los datos contenidos en el archivo "weighted_trait_intra_intersp_var.xlsx" necesarios para el Apéndice S9 |
| Mixedmodel_Appendices_S5_S6.R | Sp_trait_values.xlsx | Creates linear mixed models of trait variation according to altitude, plot and species. Appencides S5 and S6. | Crea modelos lineales de la variación delos rasgos según la altitud, la parcela y la especie. Apendices S5 y S6 |
| PCA_analysis_plots.R | Sp_trait_values.xlsx | PCA analysis and plots. Table 1 and Fig.1 | Análisis de componentes principales y sus respectivos gráficos: Tabla 1 y Fig.1 |
| trait_scaling_AppendixS4.R | Sp_trait_values.xlsx | Scaled bivariate trait correlations. Appendix S4 | Correlaciones pareadas para analizar el escalamiento de los rasgos foliares. Apendice S4 |
| variance_partition_plots.R | Percentage of explained variance obtained with the scriptMixedmodel_Appendices_S5_S6.R | Creates Fig. 2 showing variance contribution of variance factors included in the mixed models created with the script Mixedmodel_Appendices_S5_S6.R | Crea la Fig. 2 que muestra la contribución de los factores de varianza incluidos en los modelos mixtos creados con el script Mixedmodel_Appendices_S5_S6.R |
| violin_plots.R | Sp_trait_values.xlsx | Creates the violin plots of traits according to altitude shown in  Appendix S3| Crea las graficas de violín de los rasgos según la altitud que se muestran en el apendice S3 |
| Mean_plot_trait_soils_regs.R | Mean_trait_values.xlsx | Creates the plots of Fig.3, Fig4, and the appendices S7 and S8 showing the average plot trait values in response to local soil properties | Crea los gráficos de la Fig.3, Fig4, y los apéndices S7 y S8 mostrando los valores medios de los rasgos de la parcela en respuesta a las propiedades locales del suelo |

# En español: Diferenciación ecológica Damburneya

Este repositorio contiene el codigo empleado para llevar a cabo los análisis estadísticos, figuras, tablas y material suplementario del manuscrito titulado 
"Giraldo-Kalil et al. 2022. Patterns of leaf trait variation underlie ecological differences among sympatric tree species of Damburneya in a tropical rainforest (Am. J. Bot. en prensa)"

Los datos están disponibles en: 

Por favor, cita adecuadamente los datos si los usas.

Para replicar los analisis y gráficos, los usuarios deben crear un directorio para almacenar los resultados y scripts del proyecto.
Para ello, es necesario crear allí una carpeta titulada "Datos" y almacenar alli los datos

Otras carpetas tituladas "Figuras" y "Resultados" deben crearse en el directorio del proyecto.

Una descripción detallada de los scripts (codigos) empleados, su funcion y los datos requeridos se encuentra en la tabla de arriba.

Las instrucciones de los scripts estan escritas en español, mi lengua materna.
Si tienes dudas, podré ayudarte con gusto







