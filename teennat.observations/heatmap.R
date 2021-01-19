#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

data <- readRDS(here::here("data", "tn_obs_sf.rds")) %>% 
    mutate(
        latitude  = as.numeric(latitude),
        longitude = as.numeric(longitude)
        ) %>% 
    dplyr::filter(positional_accuracy < 100)

# Define UI for application
ui <- fluidPage(
    mainPanel( 
        leafletOutput(outputId = "mymap"), 
        absolutePanel(top = 60, left = 20, 
                      checkboxInput("markers", "Species", FALSE),
                      checkboxInput("heat", "Heatmap", FALSE)
        )
    ))

# Define server logic 
server <- function(input, output, session) {

    # define colour pals
    pal <- colorNumeric(
        palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
        domain = data$positional_accuracy)
    
    pal2 <- colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = data$species_guess
    )
    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet(data) %>% 
            setView(lng = -122.71285, lat = 38.5853, zoom = 13)  %>% # setting the view over ~ center of North America
            addTiles() %>% 
            addCircles(data = data, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~positional_accuracy, popup = ~as.character(positional_accuracy), label = ~as.character(paste0("Accuracy: ", sep = " ", positional_accuracy)), color = ~pal(positional_accuracy), fillOpacity = 0.5)
    })
    
    # observe boxes make checkboxes
    
    observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$markers) {
        proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(species_guess), fillOpacity = 0.2,      label = ~as.character(paste0("Accuracy: ", sep = " ", positional_accuracy))) %>%
            addLegend("bottomright", pal = pal2, values = data$species_guess,
                      title = "Species",
                      opacity = 1)}
    else {
        proxy %>% clearMarkers() %>% clearControls()
    }
})

observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$heat) {
        proxy %>%  addHeatmap(
            lng=~longitude, 
            lat=~latitude, 
            # intensity = ~positional_accuracy, 
            blur =  10, 
            max = 0.05, 
            radius = 15
            # gradient = colorNumeric(palette = "magma")
            )
    }
    else{
        proxy %>% clearHeatmap()
    }


})

}
    


# Run the application 
shinyApp(ui = ui, server = server)
