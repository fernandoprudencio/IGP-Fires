rm(list = ls())
library(tidyverse)
library(spatialEco)
library(sf)
library(mapview)
library(ggplot2)

#'CARGANDO DATOS VECTORIALES
anp <- read_sf('data/vector/anp/ANP.geojson') %>%
  dplyr::select(anp_cate, anp_nomb)

fires <- 
  read_sf('data/vector/fire_register/FiresRegister_2000_Dic2019.geojson') %>%
  dplyr::filter('ANO' != 2019)

#'INTERSECTANDO DATOS VECTORIALES
vctr.inter <- st_intersection(fires, anp)

#'CREANDO TIBBLE PARA EL PLOTEO
df <- vctr.inter %>% as_tibble() %>% select(anp_cate, anp_nomb, ANO, MES) %>%
  mutate(nfires = 1) %>% group_by(anp_cate, anp_nomb) %>%
  summarise(nfires = sum(nfires)) %>%
  mutate(nom = sprintf('%1$s %2$s', anp_cate, anp_nomb)) %>%
  arrange(anp_cate, nfires) %>% ungroup() %>%
  mutate(sortfield = sprintf('%02d', 1:23))

#'PLOTEO DE GRAFICO DE BARRAS, NUMERO DE INCENDIOS POR ANP
plot <- ggplot(df, aes(sortfield, nfires, fill = anp_cate)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_fill_brewer(palette="Pastel1") +
  theme_bw() + ylab('Number of fires') + xlab('') + labs(fill = 'ANP') +
  theme(legend.title  = element_text(size = 20),
        legend.text   = element_text(size = 15),
        plot.title    = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text.x   = element_text(size = 16, angle = 90, hjust = 1),
        axis.text.y   = element_text(size = 16),
        axis.title.x  = element_text(size = 15),
        axis.title.y  = element_text(size = 15)) +
  scale_x_discrete(label = df$nom) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20))
plot
ggsave(plot, filename = 'exports/Fires_into_ANP_v1.png',
       width = 30, height = 25, units = "cm", dpi = 1000)