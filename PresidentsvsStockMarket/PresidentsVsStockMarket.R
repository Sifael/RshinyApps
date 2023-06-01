"********************************************************

            PRESIDENTS VS STOCK MARKET APP

********************************************************"

library(shiny)
library(fullPage)
library(highcharter)
library(tidyverse)
library(shinyWidgets)



"*********************
   DATA FUNCTIONS
*********************"

setwd("/Users/sifael/Desktop/R/Apps/PresidentsvsStockMarket/")

# Data Implementation
presidents_markets_df = read.csv("presidents_and_markets.csv")

# RETURN PRESIDENTS
get_presidents = function( presidents_markets_df )
{
  presidents_markets_df %>% distinct(President) %>% pull(President)
}

# RETURN MARKETS
get_markets = function( presidents_markets_df )
{
  presidents_markets_df %>% distinct(Market) %>% pull(Market)
}

markets = get_markets(presidents_markets_df)
presidents = get_presidents(presidents_markets_df)

"*********************
      UI SECTION
*********************"

ui = pagePiling(
    sections.color = c("#F6EADF", "#ECF8F9"),
    menu = c("Markets" = "markets", "Data" = "data" ),
    pageSection( center = TRUE, menu = "markets", uiOutput("markets")),
    pageSection( center = TRUE, menu = "data", uiOutput('dataset'))
    
    
)



"*********************
     SERVER SECTION
*********************"

server = function(input, output) 
{
  
  
    # SELECT MARKET
    output$select_market = renderUI({
         
      radioGroupButtons( inputId = "market",
                         label = "Select Market Index:",
                         choices = markets,
                         selected = sample(markets, 1),
                         checkIcon = list( yes = icon("ok", lib = "glyphicon")))
    })
    
    output$select_president = renderUI({
      selectizeInput( inputId = "president",
                      label = "Type any Presidents: ",
                      multiple = TRUE,
                      choices = presidents,
                      selected = sample(presidents, 2))
    })
    
    # Market Chart
    output$market_trend = renderHighchart({
      
        presidents_markets_df %>% filter( President %in% input$president,
                                          Market %in% input$market,
                                          MonthEnd == 'True') %>%
                                  group_by(President, Market, MonthEnd) %>%
                                  mutate( MonthlyClosePrice_PctChange = (Close/lag(Close) - 1 ) * 100, 
                                          Month = row_number()) %>%
                                  hchart(., 'line', hcaes(x = Month, y = MonthlyClosePrice_PctChange, group = President))
    })
    
    
    # MARKET OUTPUT
    output$markets = renderUI({
      
        pageContainer( 
                        h3("US Presidents vs The Stock Market"), br(), br(),
                        fluidRow( 
                                  column( width = 6, uiOutput("select_market"),  align="center", style=""),
                                  column( width = 6, uiOutput("select_president"), align="center", style="")
                        ), br(),
                       highchartOutput("market_trend", height = "50vh"))
      
    })
    
    
    # MARKET DATA
    output$market_data = renderDataTable({ presidents_markets_df })
    output$dataset = renderUI({
      
        div( style = "padding-top: 80px;",
             pageContainer( dataTableOutput("market_data") ))
    })
    
 
  
  
  
  
   
}


"*********************
    SHINY APP
*********************"

shinyApp( ui = ui, server = server )
