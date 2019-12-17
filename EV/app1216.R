#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("EV1215.R")
library(shiny)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "EV Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Project Overview",tabName = "overview",icon = icon("dashboard")),
            menuItem("EDA",tabName = "EDA",icon = icon("th")),
            menuItem("Number of Charging Station",tabName = "number",icon = icon("th"),
                menuItem("All states",tabName = "all",icon = icon("map")),
                menuItem("Boston",tabName = "boston",icon = icon("map")),
                menuItem("California",tabName = "california",icon = icon("map"))),
            menuItem("Restaurant",tabName = "yelp",icon = icon("th")),
            menuItem("growthrate", tabName = "g",icon = icon("th")),
            menuItem("opinion",tabName = "opinion",icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview",
                    fluidRow(
                        column(width = 12,
                               box(width = 10,solidHeader = T,status = "info",
                                   title = "Project Overview",
                                   style = "font-family: 'Roboto'",
                                   h5("This project is to explore the electric charging station conditions in United States.")),
                               box(width = 5,solidHeader = T,status = "info",
                                   title = "DATA",
                                   style = "font-family: 'Roboto'",
                                   h5("U.S. Department of Energy"),
                                   h5("Yelp data"),
                                   h5("research paper")),
                               box(width = 5,solidHeader = T, status = "info",
                                   title = "Research Question",
                                   h5("1.The current number of charging stations in California and United States"),
                                   h5("2.If there are sufficient restaurants around the charging station and the distance between these two spots"),
                                   h5("3.The demand and the supply of the charging station"),
                                   h5("4.Opinions about the charging station (text mining of an article)"))
                               )
                    )),
            tabItem(tabName = "EDA",
                    fluidRow(
                        column(width = 12,
                               box(width = 6,solidHeader = T,status = "success",
                                   title = "fuel type",
                                   plotOutput("plot1",height = 250)
                               ),
                               box(width = 6,solidHeader = T,status = "success",
                                   title = "access time",
                                   plotOutput("plot2",height = 250)
                               ),
                               box(width = 6,solidHeader = T,status = "success",
                                   title = "facility type",
                                   plotOutput("plot3",height = 250)
                               ),
                               box(width = 6,solidHeader = T,status = "info",
                                   title = "conclusion",
                                   h5("Most file types are electric supply."),
                                   h5("Most of the charging stations are 24 hours."),
                                   h5("More than half of the charging stations are run by others, meaning that the from this plot we can not recognize if the infrastructures around charging stations are efficient."))
                               )
                            )),
            tabItem(tabName = "all",
                    fluidRow(
                        box(width = 12,solidHeader = T, status = "success",
                            title = "Number of charging stations all over the U.S.",
                            leafletOutput("map1",height = 500)
                        )
                    )),
            tabItem(tabName = "Boston",
                    fluidRow(
                        box(width = 12,solidHeader = T, status = "success",
                            title = "Number of charging stations in Boston",
                            leafletOutput("map2",height = 500)
                            )
                    )),
            tabItem(tabName = "California",
                    fluidRow(
                        box(width = 12,solidHeader = T,status = "success",
                            title = "Number of charging stations in California",
                            leafletOutput("map3",height = 500)
                            )
                    )),
            tabItem(tabName = "yelp",
                    fluidRow(
                        box(width = 12,solidHeader = T,status = "success",
                            title = "Restaruant around charging station `CAT`",
                            leafletOutput("map6",height = 500)
                            )
                    )),
            tabItem(tabName = "g",
                    fluidRow(
                        box(width = 6,solidHeader = T, status = "success",
                            title = "Growth rate of the electric vehicles",
                            tableOutput("table1")
                            ),
                        box(width = 6,solidHeader = T,status = "success",
                            title = "Grwoth rate of the charging station",
                            h5("36%–46%")
                            )
                    )),
            tabItem(tabName = "opinion",
                    fluidRow(
                        box(width = 6,solidHeader = T,status = "success",
                            title = "Wordcloud",
                            plotOutput("tm1",height = 500)
                            ),
                        box(width = 6,solidHeader = T,status = "success",
                            title = "Sentiment Analysis",
                            plotOutput("tm2",height = 500)
                            )
                    ))
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot1 <- renderPlot(plot1)
    output$plot2 <- renderPlot(plot2)
    output$plot3 <- renderImage(plot3) # output$plot3
    output$map1 <- renderPlot(map1)
    output$map2 <- renderLeaflet(map2)
    output$map3 <- renderLeaflet(map3)
    output$map6 <- renderLeaflet(map6)
    output$tm1 <- renderPlot({wordcloud(word_cloud,min.freq = 5,random.order = FALSE,rot.per = 0.2,
                                        colors = brewer.pal(5,"Dark2"),scale = c(4,0.2))})
    output$tm2 <- renderPlot(tm2)
    output$table <- DT::renderDataTable({DT::datatable(g)})
    
} # server

# Run the application 
shinyApp(ui = ui, server = server)
