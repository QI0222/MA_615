#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
pacman::p_load( readxl,plotly,tidyverse,tidytext,wordcloud,grid,gridExtra,mosaic,faraway,reshape2)
data<-readxl::read_xls("climate_change_download_0.xls",sheet = 1,col_types = "guess",na = "..")
summary(data)
mydata<-dplyr::filter(data,`Series code` =="IE.PPI.ENGY.CD" |`Series code` == "IE.PPI.TELE.CD"|`Series code` =="IE.PPI.TRAN.CD" |`Series code` =="IE.PPI.WATR.CD"|`Series code` =="EN.ATM.CO2E.PC"|`Series code` =="EN.ATM.CO2E.PP.GD.KD"|`Series code` =="EN.ATM.CO2E.KT")
names(mydata)[1]<-"countrycode"
names(mydata)[2]<-"countryname"
names(mydata)[3]<-"seriescode"
names(mydata)[4]<-"seriesname"
mydata<-mydata %>% as.data.frame() %>% melt(measure.var=7:28)
names(mydata)[7]<-"year"
summary(mydata)
names(mydata)
#make tables that shows countrywide CO2 emissions, classified by CO2 emission types (total emission, per capita, perGDP)
spreadtest<-tidyr::unite(mydata,"series",c("seriesname","seriescode"),sep="",remove=TRUE,na.rm=FALSE)
spreadtest$SCALE=NULL
spreadtest$Decimals=NULL
spreadtest<-tidyr::spread(spreadtest,series,value)
#is.na(spreadtest)
names(spreadtest)[4]<-"CO2percapita"
names(spreadtest)[5]<-"CO2perGDP"
names(spreadtest)[6]<-"CO2total"
names(spreadtest)[7]<-"investenergy"
names(spreadtest)[8]<-"investtelecom"
names(spreadtest)[9]<-"investtransport"
names(spreadtest)[10]<-"investwater"
##data cleaning
selected<-dplyr::filter(spreadtest,countrycode=="ARG"|countrycode=="BRA"|countrycode=="ARG"|countrycode=="CHL"|countrycode=="CHN"|countrycode=="COL"|countrycode=="IDN"|countrycode=="IND"|
                     countrycode=="MEX"|countrycode=="MYS"|countrycode=="PER"|countrycode=="PHL"|countrycode=="RUS"|countrycode=="THA")
sample<-na.omit(selected[3:10])
trace_0<-aggregate(sample$CO2percapita,by=list(year=sample$year),FUN=sum)
trace_1<-aggregate(sample$investenergy,by=list(year=sample$year),FUN=sum)
trace_2<-aggregate(sample$investtelecom,by=list(year=sample$year),FUN=sum)
trace_3<-aggregate(sample$investtransport,by=list(year=sample$year),FUN=sum)
plot_ly(sample,x=~trace_0$year,y=~trace_0$x,name="CO2percapita",type='scatter',mode='lines') %>% 
    add_trace(y=~scale(trace_1$x),name='energy',mode='lines+markers') %>% 
    add_trace(y=~scale(trace_2$x),name='telecom',mode='lines+markers') %>% 
    add_trace(y=~scale(trace_3$x),name='transport',mode='lines+markers')
# Define UI for application that draws a histogram
ui <- fluidPage(tabsetPanel(
    tabPanel("CO2 Emission vs Year",

    # Application title
    titlePanel("Investment in Segment "),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #selectInput("countryname",label="Country Name",choices=selected$countryname),
            selectInput("Choice",label="Energy or Telecom or Transport",c("Energy"=1,"Telecom"=2,"Transport"=3))
                    
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("linePlot")
        )
    )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$linePlot <- renderPlotly({
        if(input$Choice == 1){
            p <- plot_ly(sample,x=~trace_0$year,y=~trace_0$x,name="CO2percapita",type='scatter',mode='lines') %>%
                add_trace(y=~scale(trace_1$x),name='energy',mode='lines+markers')
        } else if(input$Choice == 2){
            p <- plot_ly(sample,x=~trace_0$year,y=~trace_0$x,name="CO2percapita",type='scatter',mode='lines') %>%
                add_trace(y=~scale(trace_2$x),name='telecom',mode='lines+markers')
        } else {
            p <- plot_ly(sample,x=~trace_0$year,y=~trace_0$x,name="CO2percapita",type='scatter',mode='lines') %>%
                add_trace(y=~scale(trace_3$x),name='transport',mode='lines+markers')
        }
        p
        # plot_ly(sample,x=~trace_0$year,y=~trace_0$x,name="CO2percapita",type='scatter',mode='lines') %>% 
        #              add_trace(y=~scale(trace_1$x),name='energy',mode='lines+markers')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
