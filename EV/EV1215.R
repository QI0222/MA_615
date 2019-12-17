#research question: is there any supply for the electric vehicle charging stations?
#1. EDA on existing electric vehicles
#2. Are the charging stations in areas with restaurants and stores? (MAP)
#3. Distance between charging stations?
#4. Growth rate of the electric vehicles
#5. Growth rate of the charging stations

#reference:https://developer.nrel.gov/docs/transportation/alt-fuel-stations-v1/nearby-route/#csv-output-format
#plots, maps, text analysis, PCA or EFA, appropriate plots, sturectured text with tables, images, links
#pacman::p_load("RSocrata","httr",ggplot2,leaflet,maps,stringr,jsonlite,dplyr,tidyverse,magrittr,rjson,geosphere,
               #NLP,tm,RColorBrewer,wordcloud,wordcloud2,pdftools,tidytext,
               #SentimentAnalysis,syuzhet)
library(RSocrata)
library(httr)
library(ggplot2)
library(leaflet)
library(maps)
library(stringr)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(magrittr)
library(rjson)
library(geosphere)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(pdftools)
library(tidytext)
library(SentimentAnalysis)
library(syuzhet)
library(usmap)
library(yelpr)
#Data acquisation
#NY charging station
df <- read.socrata(
  "https://data.ny.gov/resource/bpkx-gmh7.json",
  app_token = "LK05q1lMNPWAqZ7My1YlIGCkr",
  email     = "hayleyformer@gmail.com",
  password  = "Hailey07#"
)

#California charging station
url <-"https://developer.nrel.gov/api/alt-fuel-stations/v1.json?fuel_type=ELEC&state=CA&api_key=UI8ftsbXoXhViwIMWdzStMcwdpyZPqyhHBOpv5sP"
df1 <- jsonlite::fromJSON(url,simplifyDataFrame = TRUE,flatten = TRUE)
df1 <- df1$fuel_stations

#All states charging station
url2 <-"https://developer.nrel.gov/api/alt-fuel-stations/v1.json?fuel_type=ELEC&state=all&api_key=UI8ftsbXoXhViwIMWdzStMcwdpyZPqyhHBOpv5sP"
df2 <- jsonlite::fromJSON(url2,simplifyDataFrame = TRUE,flatten = TRUE)
df2 <- df2$fuel_stations
#percent share of US EV sales by State
g <- readxl::read_xlsx("Percent share of US EV Sales by State.xlsx")

#Yelp data
yelp_long <- head(df$longitude,10)
yelp_lat <- head(df$latitude,10)
location <- as.data.frame(cbind(yelp_long,yelp_lat))
key <- "QOk02yFuAi8h5EBi-4eq1VO88CYP_pCw-kyej_XDucODNmTPOsqubieFDm0hmLqM16sjksD8aVEHuw4F00GWoIl9tavU7T4LmRRm2wob9rJVQZ1RYcNQCAKhVRX3XXYx"
yelpdata1 <- business_search(api_key = key, longitude = yelp_long[1], latitude = yelp_lat[1])
yelpdata1 <- as.data.frame(yelpdata1)
#yelpdata2 <- business_search(api_key = key, longitude = yelp_long[2], latitude = yelp_lat[2])
#as.data.frame(yelpdata2)
#yelpdata3 <- business_search(api_key = key, longitude = yelp_long[3], latitude = yelp_lat[3])
#as.data.frame(yelpdata3)
#yelpdata4 <- business_search(api_key = key, longitude = yelp_long[4], latitude = yelp_lat[4])
#as.data.frame(yelpdata4)
#yelpdata5 <- business_search(api_key = key, longitude = yelp_long[5], latitude = yelp_lat[5])
#as.data.frame(yelpdata5)

#Data cleaning
#choose variables to use
df <- df %>% select(1,2,3,5,6,7,8,11,12,16,17,21,28,42,44,45,46)
#clean the access_days_time
df%<>%mutate(access_time_1 = if_else(str_detect(access_days_time,"24 hours daily"),
                                     "24 hours daily",
                                     if_else(str_detect(access_days_time,"Dealership business hours"),
                                             "Dealership business hours",
                                             if_else(str_detect(access_days_time,"24 hours"),
                                                     "Office business hours",
                                                     if_else(str_detect(access_days_time,"	MON"),
                                                             "24 hours daily",
                                                             "others")))))
#clean the facility type
df%<>%dplyr::mutate(facility_type_1 = if_else(str_detect(facility_type,"PAY_GARAGE"),
                                     "PAY_GARAGE",
                                     if_else(str_detect(facility_type,"HOTEL"),
                                             "HOTEL",
                                             if_else(str_detect(facility_type,"CAR DEALER"),
                                                     "CAR DEALER",
                                                     if_else(str_detect(facility_type,"MUNI_GOV"),
                                                             "GOVERNMENT",
                                                             if_else(str_detect(facility_type,"COVENIENCE_STORE"),
                                                                     "CONVENIENCE_STORE",
                                                             "others"))))))

#1. EDA on existing electric vehicles
#1.1 fuel type
plot1 <- df %>% ggplot(aes(x=fuel_type_code))+
                         geom_bar(fill = "steelblue")+
  geom_text(stat = 'count',aes(label = ..count..),vjust=-.5)+
  theme_bw()
plot1

#1.2 access time
plot2 <- df %>% ggplot(aes(x=access_time_1))+
  geom_bar(fill = "steelblue")+
  geom_text(stat = 'count',aes(label = ..count..),vjust=-.5)+
  theme_bw()
plot2

#1.3 facility type
fac <- as.data.frame(df$facility_type_1)
fac <- na.omit(fac)
names(fac) <- "facility"
slices <- fac %>% group_by(facility) %>% summarise(n())
names(slices) <- c("facility","count")
lbls <- c("GOVERNMENT","HOTEL","OTHERS","PAY_GARAGE")
pct <- round(slices$count/sum(slices$count)*100)
lbls <- paste(lbls,pct)
lbls <- paste(lbls,"%",sep="")
plot3 <- pie(pct,labels = lbls,main = "Pie Chart of Facility Type")

#1.5 mapping
#mapStates = map("state",fill = TRUE,plot = FALSE)
#U.S. state map with numbers of charging stations
#map1 <- leaflet(data = mapStates) %>% addTiles() %>% 
  #addPolygons(fillColor = topo.colors(10,alpha = NULL),stroke = FALSE)
number <- df2 %>% group_by(state) %>% summarise(n())
colnames(number) <- c("state","number")
map1 <- plot_usmap(data = number,values = "number",color = "red")+
  scale_fill_continuous(
    low = "white", high = "red", name = "charging station", label = scales::comma)+
  theme(legend.position = "right")
map1
#New York charging station
map2 <- leaflet(df) %>% addTiles() %>% 
  addMarkers(~as.numeric(longitude),~as.numeric(latitude))
map2

#CA charging station
map3 <- leaflet(df1) %>% addTiles() %>% 
  addMarkers(~as.numeric(longitude),~as.numeric(latitude))
map3

#All states charging station with clustering
map4 <- leaflet(df2) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)

# charging station only for Tesla
tesla <- df %>% filter(str_detect(access_days_time,"Tesla"))
non_tesla <- df %>% filter(!str_detect(access_days_time,"Tesla"))
icons_tesla <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "green"
)
icons_all <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "red"
)
map5 <- leaflet() %>% addTiles() %>% 
  addAwesomeMarkers(data = tesla,lng = ~as.numeric(longitude),lat = ~as.numeric(latitude),group = "Tesla - Only",icon = icons_tesla)%>%
  addAwesomeMarkers(data = non_tesla,lng = ~as.numeric(longitude),lat = ~as.numeric(latitude),group = "For - All",icon = icons_all)
  
map5%>%addLayersControl(overlayGroups = c("Tesla - Only","For - All"),options = layersControlOptions(collapsed = FALSE))%>%hideGroup("For - All")

#test<- yelpdata$businesses
#test_coor <- test$coordinates
#test%<>%dplyr::select(name,rating,price,display_phone)
#test <- dplyr::bind_cols(test_coor,test)

#Growth rate of the electric vehicles
g[,6] <- g[,4]/g[,2] - 1
colnames(g)[6] <- "growth"
table1 <- as.data.frame(g)
#Grwoth rate of the charging station
#As noted previously, charging infrastructure increased by 36%–46% across these charging types for the same 100 markets from 2016 to 2017. 
#https://theicct.org/sites/default/files/publications/US_charging_Gap_20190124.pdf

#Connected with YELP data
yelpdata1$businesses.coordinates
map6 <- leaflet(yelpdata1$businesses.coordinates) %>% addTiles() %>% 
  addMarkers(~as.numeric(longitude),~as.numeric(latitude)) %>% 
  addMarkers(lng = 	-73.932309, lat = 	40.718037, icon = list(
    iconUrl = "https://img.icons8.com/pastel-glyph/64/000000/cat--v3.png",
    iconSize = c(75, 75)
  ))
map6 #the place with the cat icon is the charging station.

#distances between the charging station and the resutaurants around
p1 <- yelpdata1$businesses.coordinates
p2 <- as.data.frame(cbind(rep(40.718037,20),rep(-73.932309,20)))
distance <- as.data.frame(geosphere::distHaversine(p1,p2))
colnames(distance) <- "distance for charging station CAT"
#Opinion about charging station 
#text mining
pdf_path <- "./energies-11-02174.pdf"
text <- pdftools::pdf_text(pdf_path)
cat(text[1])
text <- read_lines(text)
#cleaning the text
clean_text <- tolower(text) #make text to lower case using tolower() function
clean_text <- gsub(pattern = "\\W",replace=" ", clean_text) #remove puncations
clean_text <- gsub(pattern = "\\d",replace=" ", clean_text) #remove digits
clean_text <- removeWords(clean_text,words = c(stopwords(),"ai")) #remove stop words
clean_text <- gsub(pattern = "\\b[A-z]\\b{1}",replace=" ", clean_text) #remove single letters
clean_text <- stripWhitespace(clean_text) #remove white spaces
clean_text <- strsplit(clean_text," ") #split individual words and add space between them as split

#word_cloud
word_cloud <- unlist(clean_text)
tm1 <- wordcloud(word_cloud,min.freq = 5,random.order = FALSE,rot.per = 0.2,
                 colors = brewer.pal(5,"Dark2"),scale = c(4,0.2))
tm1

#sentiment analysis
sent <- analyzeSentiment(text,language = "english")
sent <- as.data.frame(sent[,1:4])
head(sent)
summary(sent$SentimentGI)
sent2 <- get_nrc_sentiment(text)
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3)
colnames(sent3) <- c("emotion","count")
tm2 <- ggplot(sent3, aes(x = emotion, y = count, fill = emotion)) + 
  geom_bar(stat = "identity") + theme_minimal() + 
  theme(legend.position="none", 
        panel.grid.major = element_blank()) + labs( x = "Emotion", y = "Total Count") + 
  ggtitle("Sentiment of Electric Vehicle") + theme(plot.title = element_text(hjust=0.5))
tm2

