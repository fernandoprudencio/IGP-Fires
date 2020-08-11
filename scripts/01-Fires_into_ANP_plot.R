#' @title
#' BarGraph of fires into ANP
#' 
#' @description
#' Este script01 plotea un grafico de barras acerca de la camtidad
#'   de incendios ocurridos en areas Naturales Protegidas "ANP"
#'   
#' @author Fernando Prudencio
#' 
#' @data
#' 'anp', area naturales protegidas
#' 'fires', registro nacional de incendios 2000-2019
#' 'k.omit.yrs', anos omitidos

rm(list = ls())

#' Instalando paquetes
pkg <- c("tidyverse", "sf", "ggplot2")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' Cargando paquetes
library(tidyverse)
library(sf)
library(ggplot2)

#' Anos a omitir en el registro de incendios
#'  de no omitir, esribir NULL
k.omit.yrs <- c(2019)

#' lectura de datos vectoriales
anp <- st_read("data/vector/anp/ANP.gpkg",
    layer = "ANP_update_2020", quiet = T, as_tibble = T
  ) %>%
  dplyr::select(anp_cate, anp_nomb)

fires <-
  st_read("data/vector/fire_register/GPKG_FIRES_REGISTER.gpkg",
    layer = "from_2000_dic2019", quiet = T, as_tibble = T
  ) %>%
  dplyr::filter(!ANO %in% c(k.omit.yrs))

#' Intersectando datos vectoriales
vctr.inter <- st_intersection(fires, anp)

#' Creando tibble para el ploteo
df <- vctr.inter %>%
  as_tibble() %>%
  select(anp_cate, anp_nomb) %>%
  mutate(nfires = 1) %>%
  group_by(anp_cate, anp_nomb) %>%
  summarise(nfires = sum(nfires)) %>%
  mutate(nom = sprintf("%1$s %2$s", anp_cate, anp_nomb)) %>%
  arrange(anp_cate, nfires) %>%
  ungroup() %>%
  mutate(sortfield = sprintf("%02d", 1:length(nfires)))

#' Ploteo de grafico de berras, numero de incendios por ANP
plot <- ggplot(df, aes(x = sortfield, y = nfires, fill = anp_cate)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  labs(x = "", y = "Number of fires", fill = "ANP") +
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 16, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
  scale_x_discrete(label = df$anp_nomb) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30))

ggsave(plot,
  filename = "exports/Fires_into_ANP.png",
  width = 30, height = 25, units = "cm", dpi = 1000
)