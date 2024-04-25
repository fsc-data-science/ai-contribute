source("global.R")
library(shiny)
library(DBI)
library(jsonlite)

ui <- fluidPage(
  
  # Headers ----
  tags$head(
    title = 'Contribute to Flipside AI',
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "sql_style.css"),
    tags$link(rel = "icon", href = "fliptrans.png"),
    tags$script(HTML("
  function copyToClipboard() {
    var content = document.getElementById('sql-code').innerText;
    var el = document.createElement('textarea');
    el.value = content;
    document.body.appendChild(el);
    el.select();
    document.execCommand('copy');
    document.body.removeChild(el);
  }
"))
  ),
  
  # top row ----
  fluidRow(class = "titlerow",
           column(width = 3, div(id = "applogo", 
                                 a(href = "https://flipsidecrypto.xyz",
                                   img(src = "Flipside_black_logo_wordmark.svg", height = "24px"),
                                   onclick = paste0("rudderanalytics.track('", 'ai_submit', "_flipside')"),
                                   target = "_blank"))
            ),
           column(width = 6,
                  fluidRow(div(id = "appname", "Contribute to Flipside AI"))
                  ),
           
           column(width = 3,
                  div(id = "sidelinks",
                      a(href = "https://flipsidecrypto.xyz/pricing", 
                        class = "data-shares-link", 
                        img(src = "Flipside_icon_white.svg", height = "14px"), 
                        "Flipside Pro",
                        onclick = paste0("rudderanalytics.track('",  'ai_submit', "_enterprise')"),
                        target = "_blank"),
                      a(href = "https://twitter.com/flipsidecrypto", 
                        img(src = "twitter.svg", height = "14px"),
                        style = "margin-left: 15px",
                        onclick = paste0("rudderanalytics.track('",  'ai_submit', "_twitter')"),
                        target = "_blank"),
                      a(href = "https://discord.com/invite/ZmU3jQuu6W", 
                        img(src = "discord.svg", height = "14px"),
                        style = "margin-left: 15px",
                        onclick = paste0("rudderanalytics.track('",  'ai_submit', "_discord')"),
                        target = "_blank")
                  )
           )
  ), # End title row

  # App ----
  hr(),
  div(class = "appbody",
  
  div(class = 'description-bar',
      fluidRow(
        column(3, class = 'info-box',
              HTML("<h4> AI learns from examples </h4>
                   <p> You provide: Context + {input} => {output} </p>
                   <p> Trained AI can then create {output} from new {input}</p>
                   ")               
               ),
        column(3, class = 'info-box',
               HTML("<h4>SQL Queries </h4>
                   <p>write a sql query for count of solana tx today</p>
                   <p>select count(tx_id) as n_tx from solana.core ...</p>
                   ")     
               ),
        column(3, class = 'info-box',
               HTML("<h4>Crypto Knowledge</h4>
                   <p>How can I track balance changes over time</p>
                   <p>tokens can be minted, burned, transferred, or even rebase...</p>
                   ")  
               ),
        column(3, class = 'info-box',
        HTML("<h4>Flipside Knowledge</h4>
                   <p>What docs are available to learn flipside's schema?</p>
                   <p>Flipside docs are at docs.flipsidecrypto.com</p>
                   ") 
               
               )
        )
      ),
  br(),
  div(class = "context-submit",
      
      fluidRow(
        column(1, class = 'btns clr',
               actionButton(inputId = 'clear', label = "Clear Boxes")
               ),
        column(1, class = 'btns clr', 
               checkboxInput('keep_context', "Keep Context", value = TRUE)
               ),
        column(6, textAreaInput('ai_context', "Context:", width = '90%', rows = 3,
                    placeholder = "This is general information relevant to entire categories of input. Can be blank. Uncheck to clear.
                    
                    using flipside crypto's snowflake SQL schema ...")
               ),
        column(2, class = 'btns',
          actionButton(inputId = "submit", label = "Send!")
        )
          )
      ),
  div(class = "submit-area", 
    fluidRow(
      column(6, 
     textAreaInput('ai_input', "Input:", width = '80%', rows = 20,
                   placeholder = "This is what you want future users to be able to ask the AI. Example:
                   
                   write a sql query for total eth spent in tx fees in last 7 days")
             ),
      column(6, 
     textAreaInput('ai_output', "Output:", width = '80%', rows = 20, 
                           placeholder = "This is the best possible output you want to teach the AI to generate by itself. Example:
                           
                           select sum(tx_fee) as total_fee 
                           from ethereum.core.fact_transactions 
                           where block_timestamp >= current_date - 7") )
      )
    ),
  div(class = 'submitter',
      textInput('submitter', label = "Your username:", placeholder = "Thank you! (kept when cleared)")
      )
  )
)

server <- function(input, output, session) {

  clear_IO <- function(keep_context){
    updateTextAreaInput(session, inputId = 'ai_output',label = "Output:", value = "",
                        placeholder = "This is the 'ideal output you want to teach the AI to generate by itself. Example:
                           
                           select sum(tx_fee) as total_fee 
                           from ethereum.core.fact_transactions 
                           where block_timestamp >= current_date - 7")
    
    updateTextAreaInput(session, inputId = 'ai_input',label = "Input:", value = "",
                        placeholder = "This is the 'ideal output you want to teach the AI to generate by itself. Example:
                           
                           select sum(tx_fee) as total_fee 
                           from ethereum.core.fact_transactions 
                           where block_timestamp >= current_date - 7")
    if(!keep_context){
      
      updateTextAreaInput(session, inputId = 'ai_context',label = "Context:", value = "",
                          placeholder = "This is general information relevant to entire categories of input. Can be blank.
                    
                    using flipside crypto's snowflake SQL schema ...")
    }
  }
  
  observeEvent(input$clear, {
    clear_IO(input$keep_context)
  })  
  
  observeEvent(input$submit, {
    ai_context = input$ai_context 
    ai_input = input$ai_input
    ai_output = input$ai_output
    submitter = input$submitter
    
    
    raw_ <- list(
      ai_context = ai_context, 
      ai_input = ai_input,
      ai_output = ai_output
    )
    
    raw_ <- toJSON(raw_)
    
    withProgress({
      
      incProgress(.1, message = "Collecting & Cleaning")
      incProgress(.1, message = "Submitting Data")
  
      submitSnowflake(raw_ = raw_,
                      submitter = submitter,
                      ai_input = ai_input,
                      ai_output = ai_output,
                      driver = "Snowflake",
                      user = snowflake_credentials$username,
                      pass = snowflake_credentials$password,
                      role = snowflake_credentials$role,
                      server = snowflake_credentials$server_url,
                      warehouse = snowflake_credentials$warehouse,
                      database = snowflake_credentials$database)
      
      incProgress(0.8, message = "Added!")
      
    })
    
    clear_IO(input$keep_context)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
