library(DBI)
library(odbc)
con<-DBI::dbConnect(odbc(),
                    Driver = "ODBC Driver 17 for SQL Server",
                    Server = "bu-rstudio-connect.bu.edu",
                    UID = "qihuang1",
                    PWD = 'Qihuang1330416',
                    port = 1433)
courses<-read.csv(file = "courses.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
as.data.frame(courses)
dbWriteTable(con,"school2_qihuang1",as.data.frame(courses))
dbGetQuery(con,"select*from school2_qihuang1 where CDUR = 5")
data<-dbSendQuery(con,"SELECT * FROM school2_qihuang1 where CDUR=5;")
d<-dbFetch(data)
as.data.frame(d)
