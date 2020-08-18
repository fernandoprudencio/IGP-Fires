#' @title
#' BarGraph of fires within ANP
#' 
#' @description
#' This script plots a bar graph about the number of fires that occurred in
#'   Natural Protected Areas "ANP
#'   
#' @author Fernando Prudencio
#' 
#' @data
#' 'anp', area naturales protegidas
#' 'fires', registro nacional de incendios 2000-2019
#' 'k.omit.yrs', anos omitidos

rm(list = ls())

#' INSTALL PACKAGES
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

#' LOAD LIBRARIES
library(tidyverse)
library(sf)
library(ggplot2)

#' Anos a omitir en el registro de incendios
#'  de no omitir, esribir NULL
k.omit.yrs <- c(2019)

#' LOAD VECTORIAL DATA
anp <- st_read("data/vector/anp.gpkg",
    layer = "ANP_update_2020", quiet = T, as_tibble = T
  ) %>%
  dplyr::select(anp_cate, anp_nomb)

fires <-
  st_read("data/vector/fire_register.gpkg",
    layer = "from_2000_dic2019", quiet = T, as_tibble = T
  )# %>%
  #dplyr::filter(!ANO %in% c(k.omit.yrs))

#' INTERSECT VECTORIAL DATA
vctr.inter <- st_intersection(fires, anp)

#' BUILD DATAFRAME TO PLOT
df <- vctr.inter %>%
  as_tibble() %>%
  select(anp_cate, anp_nomb) %>%
  group_by(anp_cate, anp_nomb) %>%
  summarise(nfires = n()) %>%
  mutate(nam = sprintf("%1$s %2$s", anp_cate, anp_nomb)) %>%
  arrange(anp_cate, nfires) %>%
  ungroup() %>%
  mutate(sortfield = sprintf("%02d", 1:length(nfires)))

#' Ploteo de grafico de berras, numero de incendios por ANP
plot <- ggplot(df, aes(x = sortfield, y = nfires, fill = anp_cate)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  labs(y = "Número de incendios", fill = "ANP") +
  scale_x_discrete(label = df$anp_nomb) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.position = c(0.87, 0.71),
    legend.margin = margin(3, 7, 7, 7),
    legend.title = element_text(size = 20, family = "ubuntu"),
    legend.text = element_text(size = 15, family = "ubuntu"),
    legend.key.width = unit(.9, "cm"),
    legend.key.height = unit(.4, "cm"),
    plot.title = element_text(size = 20, hjust = 0.5, family = "ubuntu"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, family = "ubuntu"),
    axis.text.x = element_text(
      size = 16, angle = 90, hjust = 1, family = "ubuntu", vjust = 0
    ),
    axis.text.y = element_text(size = 16, family = "ubuntu"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 15, family = "ubuntu"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    
    panel.grid = element_blank()
  )
plot

ggsave(plot,
  filename = "exports/Fires_into_ANP.png",
  width = 30, height = 25, units = "cm", dpi = 1000
)