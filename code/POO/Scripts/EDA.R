library(readr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(nortest)
library(car)
library(DescTools)
library(WRS2)
library(MASS)

data_obc = read_csv("Data/obc/dataset.csv")
data_r_ls = read_csv("Data/r_ls/dataset.csv")
data_obc_ls = read_csv("Data/obc_ls/dataset.csv")

obp_algorithms <- c("SOP", "RANDOM", "FCFS", "G01", "G02", "G03")
prp_algorithms <- c("LGAP", "SSHAPE")

ls_algorithms <- c("LS_1x0", "LS_1x1", "LS_1x2", "LS_2x2")
orders <- c('20', '30', '40', '50', '60', '70', '80', '90', '100')
capacity <- c('30', '45', '60', '75')
experiments <- c('WITH_ENCAPSULATION', 'WITHOUT_ENCAPSULATION')

data_obc <- data_obc %>% 
  mutate(obp_algorithm = parse_factor(as.character(obp_algorithm), levels = obp_algorithms),
         prp_algorithm = parse_factor(as.character(prp_algorithm), levels = prp_algorithms),
         num_orders = parse_factor(as.character(num_orders), levels = orders),
         capacity_device = parse_factor(as.character(capacity_device), levels = capacity),
         type_experiment = parse_factor(as.character(type_experiment), levels = experiments)
  )

data_r_ls <- data_r_ls %>% 
  mutate(obp_algorithm = parse_factor(as.character(obp_algorithm), levels = c("RANDOM")),
         prp_algorithm = parse_factor(as.character(prp_algorithm), levels = prp_algorithms),
         ls_algorithm = parse_factor(as.character(ls_algorithm), levels = ls_algorithms),
         num_orders = parse_factor(as.character(num_orders), levels = orders),
         capacity_device = parse_factor(as.character(capacity_device), levels = capacity),
         type_experiment = parse_factor(as.character(type_experiment), levels = experiments)
  )

data_obc_ls <- data_obc_ls %>% 
  mutate(obp_algorithm = parse_factor(as.character(obp_algorithm), levels = c("G01", "G02", "G03")),
         prp_algorithm = parse_factor(as.character(prp_algorithm), levels = prp_algorithms),
         ls_algorithm = parse_factor(as.character(ls_algorithm), levels = ls_algorithms),
         num_orders = parse_factor(as.character(num_orders), levels = orders),
         capacity_device = parse_factor(as.character(capacity_device), levels = capacity),
         type_experiment = parse_factor(as.character(type_experiment), levels = experiments)
  )

glimpse(data_obc)
glimpse(data_r_ls)
glimpse(data_obc_ls)

data_obc <- as_tibble(data_obc)
data_obc %>% 
  group_by(type_experiment, obp_algorithm) %>% 
  summarise_at(
    vars(time),
    funs(
      MEDIA = mean(., na.rm=TRUE),
      MEDIA_ACOTADA = mean(., na.rm=TRUE, trim=0.05),
      VARIANZA = var(., na.rm=TRUE)
    )
  ) %>% 
  View()

data_r_ls <- as_tibble(data_r_ls)
data_r_ls %>% 
  group_by(type_experiment, ls_algorithm) %>% 
  summarise_at(
    vars(time),
    funs(
      MEDIA = mean(., na.rm=TRUE),
      MEDIA_ACOTADA = mean(., na.rm=TRUE, trim=0.05),
      VARIANZA = var(., na.rm=TRUE)
    )
  ) %>% 
  View()

data_obc_ls <- as_tibble(data_obc_ls)
data_obc_ls %>% 
  group_by(type_experiment, ls_algorithm) %>% 
  summarise_at(
    vars(time),
    funs(
      MEDIA = mean(., na.rm=TRUE),
      MEDIA_ACOTADA = mean(., na.rm=TRUE, trim=0.05),
      VARIANZA = var(., na.rm=TRUE)
    )
  ) %>% 
  View()

type_experiment.labs <- c('Con Encapsulamiento', 'Sin Encapsulamiento')
names(type_experiment.labs) <- c('WITH_ENCAPSULATION', 'WITHOUT_ENCAPSULATION')

source("Scripts/funciones.R")

grafico <- grafico.OBP_algorithms(data_obc, obp_algorithm, time, type_experiment, type_experiment.labs)
ggsave("grafico.OBP_algorithms.png", grafico, units="in", width=10, height=5, dpi=1200)

grafico <- grafico.PRP_algorithms(data_obc, prp_algorithms, time, type_experiment, type_experiment.labs)
ggsave("grafico.PRP_algorithms.png", grafico, units="in", width=10, height=5, dpi=1200)

grafico <- grafico.densidad(data_obc)
ggsave("grafico.OBP_PRP_densidad.png", grafico, units="in", width=10, height=5, dpi=1200)

# Verificación del supuesto de Normalidad
lillie.test(data_obc$time)
# Supuesto de Homocedastecidad
bartlett.test(data=data_obc, time ~ type_experiment)
# Prueba ANOVA Robusta
t1way(time ~ type_experiment, data = data_obc)
lincon(time ~ type_experiment, data = data_obc)

data_obc_ce <- data_obc %>% filter(type_experiment == 'WITH_ENCAPSULATION') %>% select(time)
data_obc_se <- data_obc %>% filter(type_experiment == 'WITHOUT_ENCAPSULATION') %>% select(time)
yuend(data_obc_ce, data_obc_se)
# Se rechaza la Ho; por lo tanto, las medias no son iguales; 
# además, se puede inferir que estadísticamente 
# la media de los tiempos de ejecución con encapsulamiento es mayor
# a los tiempos de ejecución sin encapsulamiento; por lo tanto, 
# si incide la aplicación del paradigma OO en la ejecución de este tipo de algoritmo.


grafico <- grafico.LS_algorithms(data_r_ls, ls_algorithms, time, type_experiment, type_experiment.labs)
ggsave("grafico.R_LS_algorithms.png", grafico, units="in", width=10, height=5, dpi=1200)

grafico <- grafico.PRP_algorithms(data_r_ls, prp_algorithms, time, type_experiment, type_experiment.labs)
ggsave("grafico.R_LS_PRP_algorithms.png", grafico, units="in", width=10, height=5, dpi=1200)

grafico <- grafico.OBP_densidad(data_r_ls)
ggsave("grafico.R_LS_densidad.png", grafico, units="in", width=10, height=5, dpi=1200)

# Verificación del supuesto de Normalidad
lillie.test(data_r_ls$time)
# Supuesto de Homocedastecidad
bartlett.test(data=data_r_ls, time ~ type_experiment)
# Prueba ANOVA Robusta
t1way(time ~ type_experiment, data = data_r_ls)
lincon(time ~ type_experiment, data = data_r_ls)

data_r_ls <- as_tibble(data_r_ls)
data_r_ls_ce <- data_r_ls %>% filter(type_experiment == 'WITH_ENCAPSULATION') %>% select(time)
data_r_ls_se <- data_r_ls %>% filter(type_experiment == 'WITHOUT_ENCAPSULATION') %>% select(time)
yuend(data_r_ls_ce, data_r_ls_se)

grafico <- grafico.LS_algorithms(data_obc_ls, ls_algorithm, time, type_experiment, type_experiment.labs)
ggsave("grafico.OBC_LS_algorithms.png", grafico, units="in", width=10, height=5, dpi=1200)

grafico <- grafico.PRP_algorithms(data_obc_ls, prp_algorithms, time, type_experiment, type_experiment.labs)
ggsave("grafico.OBC_LS_PRP_algorithms.png", grafico, units="in", width=10, height=5, dpi=1200)

grafico <- grafico.OBP_densidad(data_obc_ls)
ggsave("grafico.OBC_LS_densidad.png", grafico, units="in", width=10, height=5, dpi=1200)

# Verificación del supuesto de Normalidad
lillie.test(data_obc_ls$time)
# Supuesto de Homocedastecidad
bartlett.test(data=data_obc_ls, time ~ type_experiment)
# Prueba ANOVA Robusta
t1way(time ~ type_experiment, data = data_obc_ls)
lincon(time ~ type_experiment, data = data_obc_ls)

data_obc_ls <- as_tibble(data_obc_ls)
data_obc_ls_ce <- data_obc_ls %>% 
  filter(type_experiment == 'WITH_ENCAPSULATION') %>% 
  select(., time)
data_obc_ls_se <- data_obc_ls %>% filter(type_experiment == 'WITHOUT_ENCAPSULATION') %>% select(time)
yuend(data_obc_ls_ce, data_obc_ls_se)
