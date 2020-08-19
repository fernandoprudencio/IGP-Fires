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
library(extrafont)

#' OMIT YEARS WITHIN FIRES RECORD
#'  OR WRITE NULL
k.omit.yrs <-  NULL#c(2019)

#' LOAD VECTORIAL DATA
anp <- st_read("data/vector/anp.gpkg",
    layer = "ANP_update_2020", quiet = T, as_tibble = T
  ) %>%
  mutate(
    anp_cate = as.character(anp_cate),
    anp_nomb = as.character(anp_nomb)
  ) %>%
  dplyr::select(anp_cate, anp_nomb)

fires <-
  st_read("data/vector/fire_register.gpkg",
    layer = "from_2000_dic2019", quiet = T, as_tibble = T
  ) %>%
  dplyr::filter(!ANO %in% c(k.omit.yrs))

#' INTERSECT VECTORIAL DATA
vctr.inter <- st_intersection(fires, anp)

#' ESTABLISH COLORS
df.col <- read_csv("data/table/anp_colors.csv") %>%
  mutate(col = rgb(r, g, b, maxColorValue = 100)) %>%
  filter(desc %in%  unique(anp$anp_cate))

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

plot(1:10, 1:10, type = "l", col = "red")

#' PLOT BARGRAPH
plot <- ggplot(df, aes(x = sortfield, y = nfires, fill = anp_cate)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_fill_manual(values = df.col$col) +
  theme_bw() +
  labs(
    title = "Registro nacional de incendios en Áreas Naturales Protegidas",
    subtitle = "2000-2019",
    y = "Número de incendios", fill = "ANP"
  ) +
  scale_x_discrete(label = df$anp_nomb) +
  scale_y_continuous(
    breaks = seq(0, 30, 5), limits = c(0, 30), expand = c(0, .5)
  ) +
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.position = c(.75, .42),
    legend.margin = margin(3, 7, 7, 7),
    legend.spacing.y = unit(.8, "lines"),
    legend.title = element_text(size = 20, face = "bold", family = "Source Sans Pro"),
    legend.text = element_text(size = 17, family = "Source Sans Pro"),
    legend.key.width = unit(.9, "cm"),
    legend.key.height = unit(.9, "cm"),
    plot.title = element_text(
      size = 18, face = "bold", hjust = .5, family = "Source Sans Pro"
    ),
    plot.subtitle = element_text(
      size = 16, hjust = .5, family = "Source Sans Pro"
    ),
    axis.text.x = element_text(
      size = 17, angle = 0, hjust = 1, family = "ubuntu", vjust = 0
    ),
    axis.text.y = element_text(size = 20, family = "Source Sans Pro"),
    axis.title.x = element_text(
      size = 20, vjust = -.2, family = "Source Sans Pro"
    ),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid = element_blank()
  ) + coord_flip()

#' SAVE PLOT
ggsave(plot,
  filename = "exports/Fires_into_ANP.png",
  width = 35, height = 25, units = "cm", dpi = 500
)