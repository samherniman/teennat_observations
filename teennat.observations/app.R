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

# Define UI for application that draws a histogram
ui <- fluidPage(
    mainPanel( 
        #this will create a space for us to display our map
        leafletOutput(outputId = "mymap"), #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
        absolutePanel(top = 60, left = 20, 
                      checkboxInput("markers", "Species", FALSE),
                      checkboxInput("heat", "Heatmap", FALSE)
        )
    ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    #define the color pallate for the magnitidue of the earthquake
    pal <- colorNumeric(
        palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
        domain = data$positional_accuracy)
    #define the color of for the depth of the earquakes
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
    
    #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.  
    
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
