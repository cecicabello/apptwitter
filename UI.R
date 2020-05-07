## UI- users interface
library(shiny)
library(leaflet)
# Create a timeframe wish our model will cover
timeframe <- data.frame(
  Date = seq(as.Date("2015/1/1"), as.Date("2015/12/31"), "days"))

## To change format of our date
# timeframe <- timeframe %>% mutate(
#  Week = format(Date, "%Y-%m-%U"))

fluidPage(
  titlePanel("Sentiment analysis of hashtags"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "hashtag",
                label = "Hashtag you would like to analyze:",
                value = ""),
      submitButton("update"),
      textInput(inputId = "timer",
                label = "Time in seconds to stream",
                value = "5")),
      #dateRangeInput("daterange", "Choose the date",
                     #start = min(timeframe$Date),
                     #end = max(timeframe$Date),
                     #min = min(timeframe$Date),
                     #max = max(timeframe$Date),
                     #separator = " - ", format = "dd/mm/yy",
                     #startview = 'Week', language = 'fr', weekstart = 1)),
    mainPanel(
      plotOutput("barchart"),
      plotOutput("linechart"),
      leafletOutput("mymap"),
      dataTableOutput('tbl')
    )
  )
)
