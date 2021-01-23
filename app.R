#
# Please contact Sam Herniman if you have issues with this code
# 
# This file holds the ui and server code for a shiny app
# that produces an interactive map of iNaturalist.org ovservations
# from the TeenNat programme at Pepperwood Preserve, California

# Load packages
library(shiny)
library(dplyr)
library(tidyverse)
library(ggiraph)
library(sf)
library(patchwork)
library(glue)
library(gdtools)

# specify a custom color palette with realistic looking colours
cols <- c("0" = alpha("#c9fc97", 0.3), # grassland
          "1" = alpha("#665205", 0.4), # chaparral
          "2" = alpha("#688d68", 0.5), # hardwood
          "3" = alpha("#014601", 0.4)  # conifer
)

# Load spatial layers
# ppw_boundary is the property boundary of Pepperwood
ppw_boundary <- st_read("data/ppw_boundary_poly.shp") 
# Have to set the crs because shinyapps.io use an older version of GDAL
ppw_boundary <- st_set_crs(ppw_boundary, "+proj=longlat +datum=WGS84 +no_defs")

# ppw_habitats is a vectorized classification of lidar data from the Sonoma Veg map
ppw_habitats <- st_read("data/dissolved_habitats_ppw3.shp")
ppw_habitats <- st_set_crs(ppw_habitats, "+proj=longlat +datum=WGS84 +no_defs")

# data is a simple features layer of the iNaturalist observations
# usernames have been removed
data <- readRDS("data/tn_obs_sf2.rds")
data <- st_set_crs(data, "+proj=longlat +datum=WGS84 +no_defs")

# Define the UI for the application
ui <- 
    fluidPage(
      # Application title
        titlePanel("TeenNat observations over time and space"),
      # Blurb about the map and input for choosing taxa
        sidebarLayout(
            sidebarPanel(
              # Describe the project
                "The yearly TeenNat program at Pepperwood Preserve produces successes in education, science and management.
                However, many of these successes are hard to measure, like giving interns the confidence to explore wild areas or improve their photography.
                Others are much easier to quantify, like the number and quality of observations submitted to iNaturalist.org. 
                This map illustrates these contributions to iNat by displaying the location, quality grade, and year of each observation.
                An observation is given a quality grade of research if two thirds of the community identifications of an observation agree.  
                The basemap is a simple classification of the Sonoma Veg Map lidar dataset. 
                Below, you can choose different taxa and explore when and where they were recorded across the landscape.",
              # Produce a menu to choose taxa
                selectInput(inputId = "taxa",
                            label = "What taxa do you want to look at?",
                            choices = unique(data$iconic_taxon_name),
                            selected = "Amphibia")
                
            ),
            
            # Show the map
            mainPanel(
                girafeOutput("ppw_map")
            )
        ))


# Define server logic required to draw the map and graphs
server <- function(input, output) {
    
  # Narrow the data frame of observations to the chosen taxon
    species_df <- reactive({
        data %>% 
            dplyr::filter(iconic_taxon_name == input$taxa)
    })
  # Produce the plots using ggiraph
    output$ppw_map <- renderGirafe({
      
      # Create map
        ppw_map <-
            ggplot(data = species_df()) + # Data comes from the narrowed observations
            
          # Background layer of Peppwerwood habitats
            geom_sf(
                data = ppw_habitats, 
                colour   = "transparent", # remove boxes around each "pixel"
                aes(fill = factor(z80))   # fill with the habitat classes
            ) +
            
          # Interactive points for iNat observations
            geom_sf_interactive(aes(
                tooltip = species_guess,  # tooltip of the common name
                data_id = species_guess   # data_id allows highlighting of all observations of a species
            )) +
            
          # Pepperwood property boundary
            geom_sf(data = ppw_boundary, fill = "transparent") +
            
          # Specify background colors to match pal defined above
            scale_fill_manual(values = cols,
                              name   = "Land cover",
                              limits = c("0", "1", "2", "3"),
                              labels = c("Grassland", "Chaparral", "Hardwood", "Conifer")
            ) +
            theme_classic() +
            labs(
              # Produce map title and subtitle
                title = glue("Location of {species_df()$iconic_taxon_name[1]} observations"),
                subtitle = glue("{nrow(species_df())} observations of {length(unique(species_df()$species_guess))} species"),
                x = NULL, y = NULL) +
          
            # remove distracting lines and colours
            theme(
                panel.background     = element_rect(colour = "white", fill = "white"),
                panel.grid.major     = element_line(colour = "white"),
                panel.grid.minor     = element_line(colour = "white"),
                axis.text            = element_text(size = 7),
                axis.text.x          = element_text(angle  = 15),
                legend.position      = "top",
                legend.margin        = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"),
                legend.box.margin    = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
                legend.title         = element_text(size = rel(0.5)),
                legend.text          = element_text(size = rel(0.5)),
                legend.title.align   = 0,
                legend.text.align    = 0,
                legend.justification = "left"
            )
        
        # Create barplot of observations per year
        years_plot <- 
          ggplot(data = species_df()) +
            geom_bar_interactive(
                aes(
                  # Turn each year into a factor
                    x = factor(lubridate::year(observed_on)),
                  # Colour by species
                    fill = species_guess,
                  # Highlight species on all figures
                    tooltip = species_guess, 
                    data_id = species_guess
                ),
                position = "stack"
            ) +
          # Make it pretty
            labs(x = "Observations per year", y = NULL) +
            theme_minimal() +
            theme(
                legend.position = "none",
                axis.text.x     = element_text(angle  = 30, size = 7)
                ) +
          # Lovely colour palette
            scico::scale_fill_scico_d(palette = "roma")
        
        # Produce plot of quality grade
        grade_plot <- 
          ggplot(data = species_df()) +
            geom_bar_interactive(
                aes(
                    x = quality_grade,
                  # Colour by species
                    fill = species_guess,
                  # Highlight species on all figures
                    tooltip = species_guess, 
                    data_id = species_guess
                ),
                position = "stack"
            ) +
          # Make it pretty
            labs(x = "Quality grade", y = NULL) +
            theme_minimal() +
            theme(
                legend.position = "none",
                axis.text.x     = element_text(angle  = 30, size = 7)) +
          # Lovely colour palette
            scico::scale_fill_scico_d(palette = "roma")
        
        # Put all three plots together
        girafe(
            code = 
                print(
                  # Patchwork combines all three figures
                  ((years_plot/grade_plot) | ppw_map) 
                      + plot_layout(ncol = 2, widths = c(1,2))
                  ),
            # Tooltip otions and highlighting
            options = list(
                opts_tooltip(
                    offx = 20 # move tooltip to the right a little
                ),
                # Turn each point transparent if hovering over one
                opts_hover_inv(css = "opacity:0.1;"),
                # Turn selected species red
                opts_selection(css = "fill:red;stroke"),
                opts_selection(type = "single", only_shiny = FALSE)
            ))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
