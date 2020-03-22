rm(list = ls())
library(tidyverse)
library(spatialEco)
library(sf)
library(mapview)
library(ggplot2)

#'READING VECTORIAL DATA
anp <- read_sf('data/vector/anp/ANP.geojson') %>% dplyr::select('anp_nomb')
fires <- read_sf('data/vector/fire_register/FiresRegister_2000_Dic2019.geojson') %>%
  dplyr::filter('ANO' != 2019)

df <- st_intersection(fires, anp)

x <- df %>% as_tibble() %>% select(anp_nomb, ANO, MES) %>% mutate(nfires = 1) %>% 
  group_by(anp_nomb) %>% summarise(nfires = sum(nfires))

ggplot(x, aes(anp_nomb, nfires)) +
  geom_bar(stat = "identity", fill = "gray", colour = "black") +
  theme_bw() + ylab('Number of fires') + xlab('Protected natural area') +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20))