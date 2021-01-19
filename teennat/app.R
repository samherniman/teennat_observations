#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(ggiraph)
library(sf)

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
#     # remove the column of all "Yes" values
#     remove_constant(na.rm = TRUE, quiet = FALSE) %>%
#     mutate(species_guess = stringr::str_to_sentence(species_guess))

data <- readRDS(here::here("data", "tn_obs_sf.rds"))

# # ppw_map <- 
#     ggplot(data = data) +
#     # geom_sf_interactive(aes(tooltip = species_guess)) +
#     geom_sf()+
#     geom_sf(data = ppw_boundary, fill = "transparent")
# 
# # girafe(ggobj = ppw_map)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("TeenNat observations"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "taxa",
                        label = "What taxa do you want?",
                        choices = unique(data$iconic_taxon_name),
                        selected = "Amphibia"),
            girafeOutput("years_plot")
            
        ),

        # Show the map
        mainPanel(
            girafeOutput("ppw_map")
        )
            
        )
    )


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
            labs(x = "Longitude", y = "Latitude") + # label axes
            # remove distracting lines and colours
            theme(
                panel.background = element_rect(colour = "white", fill = "white"),
                panel.grid.major = element_line(colour = "white"),
                panel.grid.minor = element_line(colour = "white"),
                axis.text.x      = element_text(angle  = 15),
                legend.position = "top"
            )

        girafe(ggobj = ppw_map,
               options = list(
                   opts_hover_inv(css = "opacity:0.1;"),
                   # opts_hover(css = "fill:red;stroke:gray;")
                   # opts_selection(css = "fill:red;stroke")
                   opts_selection(type = "single", only_shiny = FALSE)
               ))
    })

    output$years_plot <- renderGirafe({
        years_plot <- ggplot(data = species_df()) +
            geom_bar_interactive(
                aes(
                    x = factor(lubridate::year(observed_on)),
                    fill = species_guess,
                    tooltip = species_guess, 
                    data_id = species_guess
                    ),
                position = "stack"
                ) +
            
            labs(x = NULL, y = NULL) +
            coord_flip() +
            theme_minimal() +
            theme(legend.position = "none") +
            scico::scale_fill_scico_d(palette = "roma")
        girafe(ggobj = years_plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
