library(shiny)
# read dataset
source("helper.R")

ui <- navbarPage("Spatial Distribution of Vehicle Collision in NYC",
                       
                 tabPanel("Section 1: Accident Severity",
                                fluidRow(
                                  column(12,
                                         h3("Accidents Severity Distribution(2016-2018)"),
                                         fluidRow(
                                           column(4,
                                                  p('Hexagon Widths: <1km'),
                                                  h4("Instruction"),
                                                  p("In this section, you can explore the geographcial distribution of vehicle accidents by selecting year and serverity of accidents(number of collisions/deaths/injuries) in different regions.")
                                                  ),
                                           column(4,
                                                  selectInput("year", 
                                                              label = "Select Time",
                                                              choices = c("2016","2017", "2018","All"),
                                                              selected = "All"),
                                                  selectInput("loc", label = "Select Region",
                                                              choices = c("New York", "Brooklyn","Bronx","Queens","Staten Island","Manhattan"), 
                                                              selected = "New York"),
                                                  selectInput("type", label = "Select Collisions/Injuries/Deaths",
                                                              choices = c("Collisions", "Injuries","Deaths"), 
                                                              selected = "Collisions")
                                           ),
                                           column(4,
                                                  h4("Observations"), 
                                                  p("1. The distributions of accidents in different time share a similar pattern;"),
                                                  p("2. The distribution of injuries is similar to the distribution of collisions, while the distribution of deaths is much more uniform.")
                                                  )
                                         ))),
                                plotOutput(outputId = "Servmap",width = "100%", height = "600px"),
                          
                                tags$div(id="cite",
                                         'Data compiled for ', tags$em('Motor Vehicle Collisions - Crashes'),' form NYC Open Data')
                                ),
                       tabPanel("Section 2: Accident Contributing Factors",
                                fluidRow(
                                  column(12,
                                         h3("Contributing Factors Distribution(2016-2018)"),
                                         fluidRow(
                                           column(4,
                                                  p('Hexagon Widths: <0.5km'),
                                                  h4("Instruction"),
                                                  p("In this section, you can explore the geographcial distribution of several important contributing factors of vehicle accidents in different regions.")
                                                  ),
                                           column(4, 
                                                  selectInput("loc2", label = "Select Region",
                                                              choices = c("New York", "Brooklyn","Bronx","Queens","Staten Island","Manhattan"), 
                                                              selected = "New York"),
                                                  selectInput("fact", label = "Select Contributing Factors",
                                                              choices = c("All","ALCOHOL INVOLVEMENT","BACKING UNSAFELY","DRIVER INATTENTION/DISTRACTION", "FAILURE TO YIELD RIGHT-OF-WAY", "FOLLOWING TOO CLOSELY"), 
                                                              selected = "All")
                                                  ),
                                           column(4, 
                                                  h4("Obeservation"),
                                                  p(textOutput(outputId = "text"))
                                                  )
                                         )
                                         )
                                ),
                                plotOutput(outputId = "Facmap",width = "100%", height = "600px"),
                                tags$div(id="cite",
                                         'Data compiled for ', tags$em('Motor Vehicle Collisions - Crashes'),' form NYC Open Data')
                                )
)

##################### Server #####################  

server <- function(input, output){

  output$Servmap =renderPlot({
    # select location
    if(input$loc == "New York"){
      loc = newyork
    }
    else if(input$loc == "Brooklyn"){
      loc = brooklyn
    }
    else if(input$loc == "Bronx"){
      loc = bronx
    }
    else if(input$loc == "Queens"){
      loc = queens
    }
    else if(input$loc == "Staten Island"){
      loc = island
    }
    else if(input$loc == "Manhattan"){
      loc = manhattan
    }
    
    # select year and type
    if(input$year != 'All' && input$type == 'Collisions'){
      # for single year
      label = c("<200", "200-400", "400-600",
                "600-800", "800-1000",
                "1000-1200", "1200-1400",
                "1400-1600", ">1600")
      cutbin = c(0, 200, 400, 600, 800, 
                 1000, 1200, 1400, 1600, Inf)
    }
    else if(input$year != 'All' && input$type == 'Injuries') {
      # for all year
      label = c("<50", "50-100", "100-150",
        "150-200", "200-250",
        "250-300", "300-350",
        "350-400", ">400")
      cutbin = c(0, 50, 100, 150, 200,
                 250, 300, 350, 400, Inf)
    }
    else if(input$year != 'All' && input$type == 'Deaths'){
      label = c("<3","3-5",">5")
      cutbin = c(0, 3, 5, Inf)
    }
    else if(input$year == 'All' && input$type == 'Collisions'){
      label = c("<500", "500-1000", "1000-1500",
                "1500-2000", "2000-2500",
                "2500-3000", ">3000")
      cutbin = c(0, 500, 1000,
                 1500, 2000, 2500, 3000, Inf)
    }
    else if(input$year == 'All' && input$type == 'Injuries'){
      label = c("<100", "100-200", "200-300",
                "300-400", "400-500",
                "500-600", "600-700",
                "700-800", ">800")
      cutbin = c(0, 100, 200, 300, 400,
                 500, 600, 700, 800, Inf)
    }
    else if(input$year == 'All' && input$type == 'Deaths'){
      label = c("<3","3-5",">5")
      cutbin = c(0, 3, 5, Inf)
    }
    
    #select hurt type and draw plot
    if(input$type == "Collisions"){
      #select a year
      if(input$year == "All"){
        data = df2016_2018rm
      }
      else if(input$year == "2016"){
        data = data2016
      }
      else if(input$year == "2017"){
        data = data2017
      }
      else if(input$year == "2018"){
        data = data2018
      }
      loc + geom_hex(aes(x = LONGITUDE, y = LATITUDE,
                                         fill = cut(..count.., cutbin)), 
                                     colour = NA,
                                     data = data, 
                                     alpha = 0.75, 
                                     binwidth = c(0.01,0.01)) + scale_fill_brewer(palette = "OrRd",
                                                                          labels = label) + theme(legend.title=element_blank())
    }
    else if(input$type == "Injuries" ){
      if(input$year == "All"){
        data = df_pinnozero
      }
      else if(input$year == "2016"){
        data = df_pinnozero2016
      }
      else if(input$year == "2017"){
        data = df_pinnozero2017
      }
      else if(input$year == "2018"){
        data = df_pinnozero2018
      }
        loc + stat_summary_hex(aes(x = LONGITUDE, y = LATITUDE, z = NUMBER.OF.PERSONS.INJURED,
                                                   fill = cut(..value.., cutbin)), 
                                               colour = NA,
                                               fun = sum,
                                               data = data, 
                                               alpha = 0.75, 
                                               binwidth = c(0.01,0.01)) + scale_fill_brewer(palette = "OrRd",
                                                                                            labels = label) + theme(legend.title=element_blank())
    }
    else if(input$type == "Deaths"){
      if(input$year == "All"){
        data = df_pkillnozero
      }
      else if(input$year == "2016"){
        data = df_pkillnozero2016
      }
      else if(input$year == "2017"){
        data = df_pkillnozero2017
      }
      else if(input$year == "2018"){
        data = df_pkillnozero2018
      }
      loc + stat_summary_hex(aes(x = LONGITUDE, y = LATITUDE, z = NUMBER.OF.PERSONS.KILLED,
                                             fill = cut(..value.., cutbin)), 
                                             colour = NA,
                                             fun = sum,
                                             data = data, 
                                             alpha = 0.75, 
                                             binwidth = c(0.01,0.01)) + scale_fill_brewer(palette = "OrRd",
                                                                          labels = label) + theme(legend.title=element_blank())      
    }
    
  })
  
############################# The Second Panel############################
  output$Facmap =renderPlot({
    # select location
    if(input$loc2 == "New York"){
      loc = newyork
    }
    else if(input$loc2 == "Brooklyn"){
      loc = brooklyn
    }
    else if(input$loc2 == "Bronx"){
      loc = bronx
    }
    else if(input$loc2 == "Queens"){
      loc = queens
    }
    else if(input$loc2 == "Staten Island"){
      loc = island
    }
    else if(input$loc2 == "Manhattan"){
      loc = manhattan
    }
    
    #select hurt type and draw plot
    if(input$fact == "All"){
      label = c("<300", "300-600", "600-900",
                "900-1200", "1200-1500",
                "1500-1800","1800-2100",">2100")
      cutbin = c(0, 300, 600, 900,
                 1200, 1500, 1800, 2100, Inf)
      loc + geom_hex(aes(x = LONGITUDE, y = LATITUDE,
                                         fill = cut(..count.., cutbin)), 
                                     colour = NA,
                                     data = df2016_2018rm, 
                                     alpha = 0.75, 
                                     binwidth = c(0.005,0.005)) + scale_fill_brewer(palette = "OrRd",
                                                                                  labels = label) + theme(legend.title=element_blank())
    }
    else if(input$fact == "ALCOHOL INVOLVEMENT" ){
      label = c("<5", "5-10", "10-15",
                "15-20", "25-30", ">30")
      cutbin = c(0, 5, 10, 15, 20,
                 25, 30, Inf)
      loc + geom_hex(aes(x = LONGITUDE, y = LATITUDE,
                                               fill = cut(..count.., cutbin)), 
                                           colour = NA,
                                           data = alco, 
                                           alpha = 0.75, 
                                           binwidth = c(0.005,0.005)) + scale_fill_brewer(palette = "OrRd",
                                                                                          labels = label) + theme(legend.title=element_blank())
    }
    else if(input$fact == "BACKING UNSAFELY"){
      label = c("<25", "25-50", "50-75",
                "75-100", ">100")
      cutbin = c(0, 25, 50, 75, 100, Inf)
      loc + geom_hex(aes(x = LONGITUDE, y = LATITUDE,
                                               fill = cut(..count.., cutbin)), 
                                           colour = NA,
                                           data = back, 
                                           alpha = 0.75, 
                                           binwidth = c(0.005,0.005)) + scale_fill_brewer(palette = "OrRd",
                                                                                          labels = label) + theme(legend.title=element_blank())
    }
    else if(input$fact == "DRIVER INATTENTION/DISTRACTION"){
      label = c("<100", "100-200", "200-300",
                "300-400", "400-500",
                "500-600", "600-700",
                "700-800", ">800")
      cutbin = c(0, 100, 200, 300, 400,
                 500, 600, 700, 800, Inf)
      loc + geom_hex(aes(x = LONGITUDE, y = LATITUDE,
                                         fill = cut(..count.., cutbin)), 
                                     colour = NA,
                                     data = distr, 
                                     alpha = 0.75, 
                                     binwidth = c(0.005,0.005)) + scale_fill_brewer(palette = "OrRd",
                                                                                  labels = label) + theme(legend.title=element_blank())
    }
    else if(input$fact == "FAILURE TO YIELD RIGHT-OF-WAY"){
      label = c("<25", "25-50", "50-75",
                "75-100","100-125",">125")
      cutbin = c(0, 25, 50, 75, 100, 125, Inf)
      loc + geom_hex(aes(x = LONGITUDE, y = LATITUDE,
                                               fill = cut(..count.., cutbin)), 
                                           colour = NA,
                                           data = fail, 
                                           alpha = 0.75, 
                                           binwidth = c(0.005,0.005)) + scale_fill_brewer(palette = "OrRd",
                                                                                          labels = label) + theme(legend.title=element_blank())
    }
    else if(input$fact == "FOLLOWING TOO CLOSELY"){
      label = c("<25", "25-50", "50-75",
                "75-100","100-125",">125")
      cutbin = c(0, 25, 50, 75, 100, 125, Inf)
      loc + geom_hex(aes(x = LONGITUDE, y = LATITUDE,
                                               fill = cut(..count.., cutbin)), 
                                           colour = NA,
                                           data = foll, 
                                           alpha = 0.75, 
                                           binwidth = c(0.005,0.005)) + scale_fill_brewer(palette = "OrRd",
                                                                                          labels = label) + theme(legend.title=element_blank())
    }
})
  output$text = renderText({
    if(input$fact == "All"){
      "This is the graph of distribution of all kinds of collisions."
    }
    else if(input$fact == "ALCOHOL INVOLVEMENT" ){
      "Accidents caused by alcohol involvement show a number of clusters pattern, where most of clusters are location with high population density, e.g. uptown Manhattan, downtown Manhattan and Jackson Height."
    }
    else if(input$fact == "BACKING UNSAFELY"){
      "Accidents caused by backing unsafely has a concentrated clustering distribution on downtown Manhattan and southwestern Flusing."
    }
    else if(input$fact == "DRIVER INATTENTION/DISTRACTION"){
      "Accidents caused by driver inattention/distraction shares a similar distribution pattern with that of all collisions, which concentrates in downtown Manhattan."
    }
    else if(input$fact == "FAILURE TO YIELD RIGHT-OF-WAY"){
      "Accidents caused by failure to yield right-of-way shows several clusters: downtown Manhattan, southern Flusing and northern Brooklyn."
    }
    else if(input$fact == "FOLLOWING TOO CLOSELY"){
      "Accidents caused by following too closely has a strong distribution pattern with roads: most of the accidents happens on these roads, such as 678 and 495."
    }
  })
  
}

shinyApp(ui = ui, server = server)
