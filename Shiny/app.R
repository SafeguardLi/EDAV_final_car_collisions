## log
# need to allow different combination of inputs
# could show 


library(shiny)

# read dataset
source("C:/Users/54236/OneDrive/Documents/EDAV_final_car_collisions/Shiny/helper.R")

ui <- fluidPage(
  
  # App title
  titlePanel("Vehicle Collision in NYC"),
  
  # Craete side bar layout
  sidebarLayout(
    # main panel for output
    mainPanel(
      # output
      mapdeckOutput(outputId = "map")
    ),
    
    # Hover panel for inputs
    absolutePanel(
      top = 50,
      left = 50,
      width = 200,
      height = 200,
      draggable = TRUE,
      
      # Input: select
      selectInput("year", 
                  label = "Choose Accidents in a Year",
                  choices = c("2013","2014","2015","2016","2017", "2018","All"),
                  selected = "All"),
      selectInput("factor", 
                  label = "Choose Contributing Factor",
                  choices = c("Alcohol involvement", "Backing unsafely" ,"Driver inattention/distraction","Cell-phone","Passing or lane usage improper","Unsafe speed","All"),
                  selected = "All"),
      radioButtons("radius", label = h3("Hexagon Radius"),
                   choices = list("100" = 100, "300" = 300, "500" = 500), 
                   selected = 300),
      
    )
    
  )
)

server <- function(input, output){
  
  output$map <- renderMapdeck({
    mapdeck(token  = key, style = mapdeck_style('dark'), pitch =45,zoom = 5, location = c(-73.93,40.73))
  })
  
  observeEvent({input$year},{
    if ( input$year == "2017" ) {
      mapdeck_update(map_id = "map")%>%add_hexagon(data = victim_dist2017,                                                                                             
                                                   lat = "lat", 
                                                   lon = "lon",
                                                   elevation_scale = 10, 
                                                   layer_id = "hexagon_layer",
                                                   radius = 300,
                                                   update_view = FALSE)
    } else if(input$year == "2018") {
      mapdeck_update(map_id = "map")%>%add_hexagon(data = victim_dist2018,                                                                                             
                                                   lat = "lat", 
                                                   lon = "lon",
                                                   elevation_scale = 10, 
                                                   layer_id = "hexagon_layer",
                                                   radius = 300,
                                                   update_view = FALSE)
    } else if(input$year == "All"){
      mapdeck_update(map_id = "map")%>%add_hexagon(data = victim_dist,                                                                                             
                                                   lat = "lat", 
                                                   lon = "lon",
                                                   elevation_scale = 10, 
                                                   layer_id = "hexagon_layer",
                                                   radius = 300,
                                                   update_view = FALSE)
    }
    
  })
  
  
}

shinyApp(ui = ui, server = server)
