#

library(shiny)
library(dplyr)
library(tidyverse)
library(ggiraph)
library(sf)
library(patchwork)
library(glue)

# specify a custom color palette with realistic looking colours
cols <- c("0" = alpha("#c9fc97", 0.3), # grassland
          "1" = alpha("#665205", 0.4), # chaparral
          "2" = alpha("#688d68", 0.5), # hardwood
          "3" = alpha("#014601", 0.4)  # conifer
)

ppw_boundary <- st_read(here::here("data", "ppw_boundary_poly.shp"))
ppw_habitats <- st_read(here::here("data", "dissolved_habitats_ppw.shp")) 

# ppw_habitats$area <- st_area(ppw_habitats)
# grouped_sf2 <- 
#     ppw_habitats %>%
#     group_by(z80) %>% 
#     summarise(area = sum(area)) %>% 
#     st_simplify()
# st_write(
#     grouped_sf,
#     here::here("data", "dissolved_habitats_ppw.shp"),
#     append = FALSE,
#     overwrite = TRUE
#     )

# data2 <- data %>% 
# mutate(
#     latitude  = as.numeric(latitude),
#     longitude = as.numeric(longitude)
# ) %>% 
#     filter(quality_grade == "research") %>% 
#     filter(positional_accuracy < 100) %>% 
#     mutate(species_guess = gsub("'", '', species_guess)) %>% 
#     dplyr::select(-user_login) %>% 
#     remove_empty(c("rows", "cols")) %>%
#     remove_constant(na.rm = TRUE, quiet = FALSE) %>%
#     mutate(species_guess = stringr::str_to_sentence(species_guess))

data <- readRDS(here::here("data", "tn_obs_sf2.rds")) %>% 
  dplyr::filter(iconic_taxon_name != "NA")

# # ppw_map <- 
#     ggplot(data = data) +
#     # geom_sf_interactive(aes(tooltip = species_guess)) +
#     geom_sf()+
#     geom_sf(data = ppw_boundary, fill = "transparent")
# 
# # girafe(ggobj = ppw_map)

# Define UI for application that draws a histogram
ui <- 
  fluidPage(
  # fillPage(
  
  # Application title
  titlePanel("TeenNat observations"),
  
  # Sidebar with a slider input for number of bins 
  # fillPage(
  # fixedPage(
  sidebarLayout(
  # flowLayout(
    sidebarPanel(
      "Each summer, a group of intrepid teenagers set out to catalog the fungi, plants, and animals of Pepperwood Preserve.
      Equipped with cameras, GPSs and iron wills they submit their findings to iNaturalist.org and produce a photography exhibition.
      Below, you can choose different taxa and explore when and where they were recorded across the landscape.",
      selectInput(inputId = "taxa",
                  # width = '10%',
                  # multiple = TRUE,
                  label = "What taxa do you want?",
                  choices = unique(data$iconic_taxon_name),
                  selected = "Amphibia")
      
    ),
    
    # Show the map
    # fillCol(
    mainPanel(
      girafeOutput("ppw_map")
    )
    
  # )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  species_df <- reactive({
    data %>% 
      dplyr::filter(iconic_taxon_name == input$taxa)
  })
  
  output$ppw_map <- renderGirafe({
    ppw_map <-
      ggplot(data = species_df()) +
      # habitats background
      geom_sf(
        data = ppw_habitats, 
        # alpha = 0.1, 
        colour   = "transparent", # remove boxes around each "pixel"
        aes(fill = factor(z80))
      ) +
      # inat obs
      geom_sf_interactive(aes(
        tooltip = species_guess, 
        data_id = species_guess
        # color   = species_guess
      )) +
      # ppw boundary
      geom_sf(data = ppw_boundary, fill = "transparent") +
      
      # legend parameters and fill colour
      scale_fill_manual(values = cols,
                        name   = "Land cover",
                        limits = c("0", "1", "2", "3"),
                        labels = c("Grassland", "Chaparral", "Hardwood", "Conifer")
      ) +
      theme_classic() +
      labs(
        title = glue("Location of {species_df()$iconic_taxon_name[1]} observations"),
        subtitle = glue("{nrow(species_df())} observations of {length(unique(species_df()$species_guess))} species"),
        x = NULL, y = NULL) +
      # labs(x = "Longitude", y = "Latitude") + # label axes
      # remove distracting lines and colours
      theme(
        panel.background = element_rect(colour = "white", fill = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        axis.text = element_text(size = 7),
        axis.text.x      = element_text(angle  = 15),
        legend.position  = "top", # c(0.65, 0.81),
        
        legend.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        # panel.spacing.y = unit(10000, "pt"),
        # legend.box.margin = 
        legend.title     = element_text(size = rel(0.5)),
        legend.text      = element_text(size = rel(0.5)),
        legend.title.align = 0,
        legend.text.align = 0,
        legend.justification = "left"
      )
    
    years_plot <- ggplot(data = species_df()) +
      geom_bar_interactive(
        aes(
          x = factor(
            lubridate::year(observed_on)
            # levels = rev(levels(factor(lubridate::year(observed_on))))
            ),
          fill = species_guess,
          tooltip = species_guess, 
          data_id = species_guess
        ),
        position = "stack"
      ) +
      labs(x = "Observations per year", y = NULL) +
      # coord_flip() +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x     = element_text(angle  = 30, size = 7)) +
      scico::scale_fill_scico_d(palette = "roma")
    
    grade_plot <- ggplot(data = species_df()) +
      geom_bar_interactive(
        aes(
          x = quality_grade,
          fill = species_guess,
          tooltip = species_guess, 
          data_id = species_guess
        ),
        position = "stack"
      ) +
      labs(x = "Quality grade", y = NULL) +
      # coord_flip() +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x     = element_text(angle  = 30, size = 7)) +
      scico::scale_fill_scico_d(palette = "roma")
    
    girafe(
      code = 
        print(((years_plot/grade_plot) | ppw_map) 
              + plot_layout(ncol = 2, widths = c(1,2))),
           options = list(
             opts_tooltip(
              offx = 20
              ),
             opts_hover_inv(css = "opacity:0.1;"),
             # opts_hover(css = "fill:red;stroke:gray;")
             opts_selection(css = "fill:red;stroke"),
             opts_selection(type = "single", only_shiny = FALSE)
           ))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
