pacman::p_load(tidyverse,RSQLite,DBI,readxl)

#read data from excel file
donor_des<-read_excel("Top MA Donors 2016-2020.xlsx",sheet=1)
contrib_all<-read_excel("Top MA Donors 2016-2020.xlsx",sheet=2)
JFC<-read_excel("Top MA Donors 2016-2020.xlsx",sheet=3)

#build table
contributors<-select(contrib_all,contribid,City,State,Zip,fam,Fecoccemp,orgname,ultorg,lastname) %>% distinct()
recipents<-select(contrib_all,recipid,recipient,party,recipcode,cmteid) %>% distinct()
transactions<-select(contrib_all,fectransid,Fecoccemp,cycle,contribid,fam,recipid,cmteid,date,amount,type) %>% distinct()
organizations<-select(contrib_all,orgname,ultorg) %>% distinct() %>% na.omit()
mydb<-dbConnect(SQLite(),"political_contribution_QH.sqlite")
dbWriteTable(conn=mydb,value=contributors,name="contributors")
dbWriteTable(conn=mydb,value=recipents,name="recipents")
dbWriteTable(conn=mydb,value=transactions,name="transactions")
dbWriteTable(conn=mydb,value=organizations,name="organizations")
