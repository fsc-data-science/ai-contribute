library(httr)
library(jsonlite)
library(odbc)

# Always gitignore credentials and keys
snowflake_credentials <- jsonlite::read_json('snowflake-details.json')


submitSnowflake <- function(raw_, submitter, ai_input, ai_output, 
                            driver, user, pass, role, server, warehouse, database){
  
  connection <- dbConnect(
    odbc::odbc(),
    .connection_string = paste0("Driver={", driver, "}",
                                ";Server={", server,
                                "};uid=", user,
                                ";role=", role,
                                ";pwd=", pass,
                                ";warehouse=", warehouse,
                                ";database=", database)
  )
  
  # Prepare the query with placeholders
  query <- "INSERT INTO playground.ai.flippy_training_data (raw, submit_timestamp, submitter, input, output) VALUES (?, CURRENT_TIMESTAMP, ?, ?, ?)"
  
  # Bind parameters safely
  dbExecute(connection, query, params = list(raw_, submitter, ai_input, ai_output))
  
  dbDisconnect(connection)
  
  return("Data successfully inserted.")
}
