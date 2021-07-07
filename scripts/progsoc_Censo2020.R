######################################################################
# Cálculo de incidencia de programas sociales
# Datos: Censo 2020
#
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################

##############
#Configuración----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, readr, readxl, ggplot2, shiny, tidyverse, knitr,
       foreign,expss,data.table, srvyr, dineq, datapasta, scales,  ggpubr  )

theme_g <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Verdana",
                          color = "#939486"),
      panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5F5F3",
                                     color = NA),
      panel.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
      legend.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
}

#Función para determinar theme de las gráficas----
theme_mapII <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Verdana",
                          color = "#939486"),
      # remove all axes
      #axis.line = element_blank(),
      #axis.text.x = element_blank(),
      #axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5F5F3",
                                     color = NA),
      panel.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
      legend.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9,hjust = 1,
                                 color = "#939486"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "#4B4C47"),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "#939486",
                                   # margin = margin(b = -0.1,
                                   #                 t = -0.1,
                                   #                 l = 2,
                                   #                 unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}



#Importar datos
# Viviendas00 <- read_csv("~/Documents/Encuestas/Censo 2020/Ampliado/Censo2020_CA_eum_csv/Viviendas00.CSV")
# 
# glimpse(Viviendas00)
# 
# municipios <- Viviendas00 %>% 
#   mutate(
#     prog_soc= case_when((INGR_AYUGOB==5) ~ 1,
#                            TRUE ~ 0)
#   ) %>% 
#   as_survey(weights = c(FACTOR)) %>%
#   group_by(ENT,MUN) %>% 
#   summarize(prog_soc = survey_mean(prog_soc, na.rm = T),
#             ing_lab = survey_mean(INGTRHOG, na.rm = T),
#               tot_int = survey_mean(NUMPERS, na.rm = T)
#             )

#Importar otros datos
municipios_prog_soc <- read_dta("~/Documents/Encuestas/Censo 2020/Ampliado/Censo2020_CA_eum_csv/municipios_prog_soc.dta")
glimpse(municipios_prog_soc)

summary(municipios_prog_soc$ing_lab_pc)

g1 <-
  municipios_prog_soc %>% 
  filter(ingtrhog<364000) %>% 
  ggplot(aes(x=ing_lab_pc,y=prog_soc))+
  geom_point(aes(),
             alpha = 0.35)+
  scale_colour_viridis_d()+
  geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95)+
  stat_regline_equation(label.y = .2, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = .1, aes(label = ..rr.label..)) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent)+
  scale_x_continuous("Ingreso laboral por persona, promedio según municipios",
                     # breaks = seq(0,500000,5000),
    trans = "log10")+
  labs (title = "Incidencia de programas sociales según ingresos",
  subtitle = "Municipios en México, 2020",
  caption = "Fuente: Elaboración propia con datos del Censo de Pob. y Vivienda 2020.")
g1

ggsave(g1, filename = "1 Ingresos prom y Beneficiarios.png", path= "www/plots/",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")





