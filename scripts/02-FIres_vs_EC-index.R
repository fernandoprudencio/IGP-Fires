#' @title
#' monthly record of fires vs E index and C index
#'
#' @description
#' This script plots the fires record vs. historic series of C index and
#'   E index from 2000 to 2018
#'   
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "sf", "griExtra", "tidyverse", "ggthemes", "grid", "gridExtra"
)

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' LOAD PACKAGES
library(sf)
library(grid)
library(ggthemes)
library(gridExtra)
library(tidyverse)
library(scales)

#' CHANGE TO ENGLISH LANGUAGE
Sys.setlocale(category = "LC_ALL", locale = "english")

#' INDICES
ec.index <- read.csv(
  "data/table/EC_Index/EC_index.csv",
  header = T, sep = ";"
) %>%
  as_tibble() %>%
  mutate(date = sprintf("%s-%s-01", yy, mm) %>% as.Date()) %>%
  dplyr::select(date, E, C)

#' INCENDIOS
sf.fires <- st_read(
  dsn = "data/vector/fire_register.gpkg",
  layer = "from_2000_dic2019", quiet = T, as_tibble = T
) %>%
  as_tibble() %>%
  mutate(date = sprintf("%s-%s-01", ANO, MES) %>% as.Date()) %>%
  group_by(date) %>%
  summarise(nfires = n()) %>%
  arrange(date)

#' BUILD DATAFRAME TO PLOT GRAPH
df <- ec.index %>%
  left_join(sf.fires, by = "date") %>%
  filter(date >= "2000-01-01" & date <= "2018-12-01") %>%
  mutate(nfires = ifelse(is.na(nfires), 0, nfires)) %>%
  gather(key = "type", value = "value", -date, -nfires)

#' HOMOGENIZE THE PRIMARY AND SECONDARY AXIS
ylim.prim <- c(
  min(df$nfires),
  max(df$nfires)
)

ylim.sec <- c(
  min(df$value),
  max(df$value)
)

#' CONVERSION COEFFICIENT
n <- diff(ylim.prim) / diff(ylim.sec)
m <- ylim.prim[1] - n * (ylim.sec[1])

#' PLOT
plt <- ggplot(df, aes(x = date, y = nfires/2)) +
  geom_bar(stat = "identity", fill = "gray", colour = "black") +
  labs(y = list("number of fires", "vdsvsd"), tag = "a)") +
  geom_line(
    aes(
      y = m + value * n,
      linetype = type,
      color = type,
      size = type
      )
  ) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = c("C index", "E index")
  ) +
  scale_color_manual(
    values = c("black", "deepskyblue3"),
    labels = c("C index", "E index")
  ) +
  scale_size_manual(
    values = c(.5, .5),
    labels = c("C index", "E index")
  ) +
  geom_hline(
    yintercept = m + 0 * n,
    linetype = "dashed", color = "gray", size = .8
  ) +
  scale_x_date(
    labels = date_format("%Y"),
    breaks = "1 year", expand = c(0, 0)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ (. - m) / n, name = "index" ),
    expand = c(0, 0)
  ) +
  theme_bw() +
  annotate(
    "rect",
    xmin = as.Date("2016-06-01"),
    xmax = as.Date("2016-12-01"),
    ymin = 0, ymax = 500,
    alpha = 0.3, fill = "gray"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2010-07-01"),
    xmax = as.Date("2010-12-01"),
    ymin = 0, ymax = 500,
    alpha = 0.3, fill = "gray"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2005-06-01"),
    xmax = as.Date("2005-12-01"),
    ymin = 0, ymax = 500,
    alpha = 0.3, fill = "gray"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2000-10-01"),
    xmax = as.Date("2001-01-01"),
    ymin = 0, ymax = 500,
    alpha = 0.3, fill = "gray"
  ) +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.margin = margin(3, 7, 7, 7),
    legend.key.width = unit(.9, "cm"),
    legend.key.height = unit(.4, "cm"),
    legend.position = c(0.68, 0.83),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, family = "Source Sans Pro"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 15, family = "Source Sans Pro"
    ),
    axis.text.x = element_text(
      size = 11, hjust = -.2, angle = 0, family = "Source Sans Pro"
    ),
    axis.text.y = element_text(
      size = 11, family = "Source Sans Pro"
    ),
    panel.grid = element_blank(),
    plot.tag = element_text(size = 25, family = "Source Sans Pro")
  )

# SAVE PLOT
ggsave(
  plt,
  filename = "exports/fire_vs_EC-index.png",
  width = 28, height = 7, units = "cm", dpi = 1000
)