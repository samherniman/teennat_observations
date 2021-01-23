# This is code used for downloading and cleaning the observations 
# before they are ready for the shiny app

# Load libraries
library(rinat)
library(sf)
library(tidyverse)

# Read in Pepperwood property boundary
ppw_sf <- read_sf(here::here("data", "ppw_boundary_poly.shp"))

# Download and clean TeenNat observations
ppw_obs <- download_ppw_observations(
  ppw_sf,
  project_name = "teennat-at-pepperwood-santa-rosa-ca"
  ) %>% 
  mutate(
      latitude  = as.numeric(latitude),
      longitude = as.numeric(longitude)
  ) %>%
      # filter(quality_grade == "research") %>%
      filter(positional_accuracy < 100) %>%
      mutate(species_guess = gsub("'", '', species_guess)) %>%
      dplyr::select(-user_login) %>%
      janitor::remove_empty(c("rows", "cols")) %>%
      janitor::remove_constant(na.rm = TRUE, quiet = FALSE) %>%
      mutate(species_guess = stringr::str_to_sentence(species_guess))

# Save observations
# saveRDS(ppw_obs, file = here::here("data", "tn_obs_sf2.rds"))
# vroom::vroom_write(ppw_obs, 
                   # delim = ",",
                   # append = FALSE,
                   # path = here::here("data", "tn_obs_sf2.csv")
                   # )

# Test that everything is alright by making a map
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
