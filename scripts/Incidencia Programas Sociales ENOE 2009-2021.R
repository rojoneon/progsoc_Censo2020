######################################################################
# Cálculo de incidencia de programas sociales
# Datos: ENOE 2009-2021
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



prog_soc_09_21 <-
tibble::tribble(
        ~Veintil, ~`2009`, ~`2010`, ~`2011`, ~`2012`, ~`2013`, ~`2014`, ~`2015`, ~`2016`, ~`2017`, ~`2018`, ~`2019`, ~`2020`, ~`2021`,
  "S.I.",    0.27,    0.26,    0.27,    0.27,    0.28,    0.29,    0.32,     0.3,    0.29,    0.27,    0.27,    0.34,    0.34,
             "1",    0.62,    0.62,    0.63,    0.63,    0.63,    0.64,    0.65,    0.68,    0.65,    0.63,     0.6,    0.47,    0.51,
             "2",    0.46,    0.45,    0.48,    0.48,    0.48,    0.52,    0.54,    0.51,    0.51,     0.5,    0.47,    0.42,    0.42,
             "3",     0.4,    0.41,     0.4,    0.43,    0.44,    0.44,    0.47,     0.5,    0.45,    0.44,    0.38,    0.39,    0.39,
             "4",    0.35,    0.34,    0.35,    0.37,    0.39,     0.4,    0.44,    0.41,    0.43,    0.39,    0.35,    0.33,     0.4,
             "5",    0.31,    0.33,    0.32,    0.32,    0.33,    0.35,    0.42,    0.39,    0.35,    0.35,    0.32,    0.33,    0.35,
             "6",    0.26,    0.26,    0.32,    0.31,    0.33,    0.34,    0.35,    0.36,    0.38,    0.33,    0.32,    0.33,    0.33,
             "7",    0.26,    0.26,    0.24,    0.28,    0.29,    0.31,    0.36,    0.32,    0.31,    0.33,     0.3,    0.29,     0.3,
             "8",    0.22,    0.26,    0.25,    0.27,    0.28,    0.28,    0.32,    0.32,    0.31,    0.32,    0.28,     0.3,    0.34,
             "9",    0.22,    0.22,    0.22,    0.23,    0.25,    0.28,    0.32,    0.32,    0.28,    0.26,     0.3,    0.28,     0.3,
            "10",     0.2,    0.19,    0.21,    0.21,    0.24,    0.24,    0.31,    0.27,    0.29,    0.29,    0.25,    0.25,    0.32,
            "11",    0.19,     0.2,    0.22,    0.19,     0.2,    0.23,    0.27,    0.28,    0.26,    0.23,    0.22,    0.25,    0.26,
            "12",    0.16,    0.16,    0.19,     0.2,    0.21,    0.21,    0.24,    0.23,    0.23,    0.25,    0.22,    0.24,    0.27,
            "13",    0.16,    0.16,    0.16,    0.16,    0.18,     0.2,    0.24,    0.24,    0.22,    0.22,    0.18,    0.22,    0.26,
            "14",    0.12,    0.14,    0.15,    0.16,    0.19,    0.19,    0.23,    0.19,     0.2,    0.19,    0.17,    0.21,    0.21,
            "15",    0.12,    0.13,    0.13,    0.12,    0.15,    0.16,     0.2,     0.2,    0.17,    0.19,    0.18,     0.2,     0.2,
            "16",     0.1,    0.12,    0.11,    0.11,    0.13,    0.14,    0.16,    0.17,    0.16,    0.14,    0.15,    0.18,    0.22,
            "17",    0.07,    0.09,    0.09,    0.09,     0.1,    0.11,    0.15,    0.12,    0.13,    0.14,    0.13,    0.16,    0.19,
            "18",    0.07,    0.07,    0.07,    0.07,    0.09,    0.09,    0.11,     0.1,    0.11,    0.09,     0.1,    0.14,    0.13,
            "19",    0.06,    0.05,    0.07,    0.07,    0.08,    0.07,    0.08,    0.08,    0.08,    0.07,    0.07,    0.12,    0.13,
            "20",    0.04,    0.05,    0.05,    0.05,    0.06,    0.05,    0.06,    0.05,    0.06,    0.05,    0.05,    0.08,    0.08
  )



glimpse(prog_soc_09_21)

prog_soc<- prog_soc_09_21 %>% 
  mutate(
    Veintil_f= factor(Veintil),
     Veintil_re=   fct_relevel(Veintil_f, c("S.I.", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","11", "12",  "13",  "14",  "15",  "16",  "17",  "18",  "19",  "20" )))
glimpse(prog_soc)
table(prog_soc$Veintil_re)
    
g2021<-
prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2021`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2021),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2021, filename = "2021 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")

g2020<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2020`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2020),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2020, filename = "2020 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")

g2019<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2019`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2019),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2019, filename = "2019 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")

g2018<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2018`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2018),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2018, filename = "2018 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")


g2017<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2017`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2017),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2017, filename = "2017 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")

g2016<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2016`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2016),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2016, filename = "2016 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")

g2015<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2015`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2015),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2015, filename = "2015 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")

g2014<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2014`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2014),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2014, filename = "2014 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")

g2013<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2013`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2013),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2013, filename = "2013 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")

g2012<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2012`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2012),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2012, filename = "2012 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")


g2011<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2011`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2011),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2011, filename = "2011 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")


g2010<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2010`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2010),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2010, filename = "2010 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")


g2009<-
  prog_soc %>% 
  ggplot(aes(x=Veintil_re,y=`2009`))+
  geom_line(group = 1) +
  theme_mapII() +
  scale_y_continuous("Porcentaje de hogares beneficiarios de programas sociales",
                     labels = scales::percent,
                     breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete("Veintiles de ingreso lab. + Hogares sin ingresos laborales") +
  labs (title = "Incidencia de programas sociales (2009),",
        subtitle = "según veintiles de ingreso",
        caption = "Nota: S.I.= Sin ingresos
        Fuente: Elaboración propia con datos de ENOE.")
ggsave(g2009, filename = "2009 Beneficiarios-Veintiles.png", path= "www/plots/enoe",
       dpi = 320, width = 6, height = 6,
       bg = "transparent")










