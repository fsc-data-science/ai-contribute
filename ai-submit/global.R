library(httr)
library(jsonlite)
library(odbc)

# Always gitignore credentials and keys
snowflake_credentials <- jsonlite::read_json('snowflake-details.json')

submitSnowflake <- function(query, driver, user, pass, role, server, warehouse, database){
  
  connection <- dbConnect(
    odbc::odbc(),
    .connection_string = paste0("Driver={",driver,"}",
                                ";Server={",server,
                                "};uid=",user,
                                ";role=",role,
                                ";pwd=",pass,
                                ";warehouse=", warehouse,
                                ";database=", database)
  )
  
  output <- dbGetQuery(connection, query)
  dbDisconnect(connection)
  return(output)
  
}