pacman::p_load(RODBCDBI,DBI,odbc)
pacman::p_load(RODBCDBI,odbc)


con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "ODBC Driver 17 for SQL Server",
                      Server   = "bu-rstudio-connect.bu.edu",
                      
                      UID      = "Lee",
                      PWD      = "0123@Lee",
                      Port     = 1433)
dbDisconnect(con)
