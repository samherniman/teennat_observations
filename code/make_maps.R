library(rinat)
library(sf)
library(tidyverse)

ppw_sf <- read_sf(here::here("data", "ppw_boundary_poly.shp"))

ppw_obs <- download_ppw_observations(
  ppw_sf,
  project_name = "teennat-at-pepperwood-santa-rosa-ca"
  )

saveRDS(ppw_obs, file = here::here("data", "tn_obs_sf.rds"))

ggplot(data = ppw_obs) +
  geom_sf(
    data     = ppw_sf,
    color = "grey90",
    size = 1,
    fill   = "transparent" # remove boxes around each "pixel"
    # aes(fill = factor(layer))
  ) +
  geom_sf(
    data = ppw_obs,
    color = "darkolivegreen3"
    # aes(color = iconic_taxon_name)
  ) +
  # facet_wrap(~iconic_taxon_name) +
  labs(x = "", y = "") + # label axes
  # remove distracting lines and colours
  theme(
    legend.title = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(colour = "white", fill = "white"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "white"),
    # axis.text.x      = element_text(angle  = 15),
    axis.text=element_blank(),
    axis.ticks=element_blank()
  )
