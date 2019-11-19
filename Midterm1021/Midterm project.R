pacman::p_load(ggplot2,readr,leaflet,dplyr,data.table,xlsx,readxl,janitor,lubridate,plotly,countrycode,tm,tidyverse,SnowballC,tidytext,wordcloud,grid,gridExtra,mosaic,faraway)
data<-read_xls("climate_change_download_0.xls",sheet = 1,col_types = "guess",na = "..")
summary(data)
mydata<-filter(data,`Series code` =="IE.PPI.ENGY.CD" |`Series code` == "IE.PPI.TELE.CD"|`Series code` =="IE.PPI.TRAN.CD" |`Series code` =="IE.PPI.WATR.CD"|`Series code` =="EN.ATM.CO2E.PC"|`Series code` =="EN.ATM.CO2E.PP.GD.KD"|`Series code` =="EN.ATM.CO2E.KT")
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
table1<-xtabs(~CO2total+countryname,data=spreadtest)
table1<-aggregate(CO2total~countryname,rm.na=TRUE,data=spreadtest,FUN = sum)
##data cleaning
spreadtest<-filter(spreadtest,investenergy!=0|investtelecom!=0|investtransport!=0|investwater!=0)
datatest<-filter(spreadtest,investenergy!=0&investtelecom!=0&investtransport!=0&investwater!=0)
selected<-filter(spreadtest,countrycode=="ARG"|countrycode=="BRA"|countrycode=="ARG"|countrycode=="CHL"|countrycode=="CHN"|countrycode=="COL"|countrycode=="IDN"|countrycode=="IND"|
                   countrycode=="MEX"|countrycode=="MYS"|countrycode=="PER"|countrycode=="PHL"|countrycode=="RUS"|countrycode=="THA")


table2<-xtabs(~CO2percapita+countryname,data=selected)
table2<-aggregate(CO2percapita~countryname,rm.na=TRUE,data=selected,FUN=sum)

table3<-xtabs(~CO2perGDP+countryname,data=spreadtest)
table3<-aggregate(CO2perGDP~countryname,rm.na=TRUE,data=spreadtest,FUN = sum)

##data scaling
ARG<-filter(selected,countrycode=="ARG")



##data visualization (CO2 emission)
ggplot(spreadtest,aes(x=countryname,y=CO2total,color=year))+geom_point()+geom_smooth()
ggplot(spreadtest,aes(x=countryname,y=CO2percapita,color=year))+geom_point()+geom_smooth()
ggplot(selected,aes(x=countryname,y=CO2perGDP,color=year))+geom_point()+geom_smooth()
ggplot(spreadtest,aes(x=year,y=CO2total))+geom_point()+geom_smooth()
ggplot(spreadtest,aes(x=year,y=CO2percapita))+geom_point()+geom_smooth()
ggplot(selected,aes(x=as.numeric(as.character(year)),y=CO2perGDP,color=countryname))+geom_point()+geom_smooth()
plot_ly(selected,x=~as.numeric(as.character(year)),y=~CO2perGDP,type='scatter',mode='lines')
plot_ly(selected,x=~as.numeric(as.character(year)),y=~CO2total,type='scatter',mode='lines')
plot_ly(selected,x=~as.numeric(as.character(year)),y=~CO2percapita,type='scatter',mode='lines')
#ignore countryname
trend<-aggregate(.~year,rm.na=TRUE,data=spreadtest,FUN=sum)
trend<-spreadtest[5:12] %>% group_by(year) 
#ggplot(trend,aes(x=year,y=CO2percapita))+geom_point()+geom_line()
#ggplot(trend)+geom_line(aes(x=year,y=CO2percapita),stat = "identity")
#trend %>% plot_ly(x=~year) %>% add_trace(y=~CO2percapita,mode='line',type='scatter',name='CO2 per capita') %>% 
  #add_trace(y=~CO2perGDP,mode='line',type='scatter',name='CO2percapita') %>% layout(title='Emission of CO2',yaxis=list(title='CO2 Emission'))

##Top emission from country (CO2 emission per GDP)
clean<-na.omit(spreadtest)
topemission<-arrange(table3,CO2perGDP)
ggplot(head(topemission,n=10),aes(countryname,CO2perGDP,fill=countryname,label=paste(CO2perGDP)))+geom_bar(sta='identity',color='black')+
  coord_flip()+theme_bw()+theme(legend.position = 'none')+geom_text(aes(y=1),hjust=0,vjust=0.5,size=4)+
  labs(title='Top countries with highest CO2 emission per GDP')

##comparing investment in different segments with CO2 emission for each selected country
plot_ly(ARG,x=~ARG$year,y=~ARG$CO2percapita,name="CO2percapita",type='scatter',mode='lines') %>% 
  add_trace(y=~scale(ARG$investenergy),name='energy',mode="lines+markers") %>% 
  add_trace(y=~scale(ARG$investtelecom),name='telecom',mode='lines+markers') %>% 
  add_trace(y=~scale(ARG$investtransport),name='transport',mode='lines+markers')

#same thing for other 13 countries

##use the 13 countries as a sample to test if investment in different segments would impact the amount of CO2 emission
sample<-na.omit(selected[3:10])
trace_0<-aggregate(sample$CO2percapita,by=list(year=sample$year),FUN=sum)
trace_1<-aggregate(sample$investenergy,by=list(year=sample$year),FUN=sum)
trace_2<-aggregate(sample$investtelecom,by=list(year=sample$year),FUN=sum)
trace_3<-aggregate(sample$investtransport,by=list(year=sample$year),FUN=sum)
plot_ly(sample,x=~trace_0$year,y=~trace_0$x,name="CO2percapita",type='scatter',mode='lines') %>% 
  add_trace(y=~scale(trace_1$x),name='energy',mode='lines+markers') %>% 
  add_trace(y=~scale(trace_2$x),name='telecom',mode='lines+markers') %>% 
  add_trace(y=~scale(trace_3$x),name='transport',mode='lines+markers')




