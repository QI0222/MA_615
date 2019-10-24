#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Porblem1: Make a map of the world which you can use to experiment with various projections from the coord_map function.  Use a pulldown menu for users to select the projection to be used for mapping.
library(shiny)
pacman::p_load(ggmap,maptools,maps,evaluate,mapproj)
mp1 <- ggplot(mapWorld, aes(x=long, y=lat, group=group))+
    geom_polygon(fill="white", color="black") +
    coord_map(xlim=c(-180,180), ylim=c(-60, 90))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("World Map"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Projections",
                        "Choose a Projection:",
                        c("cylindrical",
                          "mercator",
                          "sinusoidal",
                          "gnomonic",
                          "rectangular",
                          "cylequalarea",
                          "cylindrical",
                          "mollweide",
                          "gilbert",
                          "azequidistant",
                          "orthographic",
                          "laue",
                          "globular"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3(textOutput("caption")),
            plotOutput("Project")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Project <- renderPlot({
        # generate bins based on input$bins from ui.R
        if (input$Projections  %in% c("rectangular","cylequalarea")) {
            mp1 + coord_map(input$Projections,parameters = 0,xlim=c(-180,180), ylim=c(-60, 90))
        }  else {
            mp1 + coord_map(input$Projections,xlim=c(-180,180), ylim=c(-60, 90))
        }
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

#Problem2: Using geospatial data from Analyze Boston produce a topical website that tells a story and includes interactive widgets for users to explore the data.
pacman::p_load(shiny,leaflet,tidyverse,leaflet.extras)
#import data
data1<-read.csv("Traffic_Signals.csv")
data1<-na.omit(data1)
#categorize the count data
summary(data1$Count_)
data1$Count_level<-ifelse(data1$Count_<=208,"level1",
                     ifelse(data1$Count_<=419&data1$Count_>208,"level2",
                            ifelse(data1$Count_<=631&data1$Count_>419, "level3",
                                          "level4")))
ui<-fluidPage(
    mainPanel(
        leafletOutput(outputId = "mymap"),
        absolutePanel(top=60, left=20,
                      checkboxInput("markers","Trafficlevel",FALSE))
    )
)

server<-function(input,output,session){
    pal2<-colorFactor(
        palette = c('gold','orange','dark orange','orange red','red'),
        domain = data1$Count_Level
    )
    #create the map
    output$mymap<-renderLeaflet({
        leaflet(data1) %>% 
            addTiles() %>% 
            addMarkers(lng=	-71.13221,lat=42.36327,popup = "most traffic") %>% 
            setView(lng=-70,lat=42,zoom=8) %>% 
            addTiles() %>% 
            addCircles(data=data1, lat=~Y, lng=~X, weight = 1,fillOpacity = 0.5)
    })
    observe({
        proxy <- leafletProxy("mymap", data = data1)
        proxy %>% clearMarkers()
        if (input$markers) {
            proxy %>%  addHeatmap(lng=~X, lat=~Y, blur =  10, max = 0.05, radius = 15) 
        }
        else{
            proxy %>% clearHeatmap()
        }
        
        
    })
    
}

shinyApp(ui,server)
