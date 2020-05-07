library(shiny)
library(ggplot2)
library(rtweet)
library(tm)
library(tidytext)
library(textdata)
library(quanteda)
library(stringi)
library(jsonlite)
library(dplyr)
library(RColorBrewer)
library(sys)

file.create(paste("~/stream",Sys.Date(),".csv",sep = '')) #initializes an empty file in wd
nrc <- get_sentiments("nrc")
class_df <- data.frame("class" = character(),
                       "time" = character(),
                       "lat" = character(),
                       "lon" = character(),
                       "tweet" = character()) #empty df 4 output

countClass <- function(x){
  x <- removePunctuation(x)
  x <- gsub("[^A-Za-z0-9]+", " ", x)
  x <- tolower(x)
  x <- removeWords(x, stopwords("en"))
  x <- trimws(x)
  x <- stripWhitespace(x)
  #try(
  temp_sent <- names(table(nrc[nrc$word %in% as.character(quanteda::tokens(x)), "sentiment"]))[1]
  #)
  return(temp_sent)
}

streamTweet <- function(query, timer){
  #filename <- "teststream.json"
  streamtime <- floor(as.integer(timer)/5) #number of seconds for each sampling from twitter
  timer <- 1 #initialize timer
  rt <- stream_tweets(q = query, timeout = streamtime) #initialize dataframe
  rt <- rt[rt$lang == "en",] #remove non-english text
  class_list = c()
  time_list = c()
  lat_list = c()
  lon_list = c()
  raw_text = c()
  
  repeat { #start repeat statement
    temp_rt <- stream_tweets(q = query, timeout = streamtime) #stream new data
    temp_rt <- temp_rt[temp_rt$lang == "en",] 
    rt <- rbind(rt, temp_rt) #add new data to dataframe
    timer <- timer + 1 #add loop to timer
    #temp_rt <- lat_lng(temp_rt)
    
    for (t in 1:nrow(temp_rt)){
      class_list <- c(class_list, countClass(temp_rt$text[t]))
      if (!is.null(countClass(temp_rt$text[t]))){
        time_list = c(time_list, as.POSIXct(temp_rt$created_at[t]))
        lat_list = c(lat_list, temp_rt$geo_coords[[t]][[2]])
        lon_list = c(lon_list, temp_rt$geo_coords[[t]][[1]])
        raw_text = c(raw_text, temp_rt$text[t])
      }
    }
    
    #time_list = c(as.POSIXct(temp_rt$created_at[temp_rt$text %in% raw_text, ]))
    
    # the final dataframe as is just includes classification, needs time of tweet and lat/lon
    temp_class_df <- data.frame("class" = matrix(unlist(class_list), nrow = length(class_list), byrow = T),
                                "time"  = matrix(unlist(time_list), nrow = length(class_list), byrow = T),
                                "lat" = matrix(unlist(lat_list), nrow = length(class_list), byrow = T),
                                "lon" = matrix(unlist(lon_list), nrow = length(class_list), byrow = T),
                                "tweet" = matrix(unlist(raw_text), nrow = length(class_list), byrow = T))
    
    class_df <<- rbind(class_df, temp_class_df) #data.frame("class" = matrix(unlist(class_list), nrow=length(class_list), byrow=T))
    #class_df <- reactiveValues(class = class_list)
    #class_df
    write.csv(x = class_df,file = paste("~/stream",Sys.Date(),".csv",sep = ''), row.names = FALSE)
    
    if (timer == 5){ #break statement, stream ends after 5 loops
      break
    }
  }
}

#streamTweet("covid")

shinyServer(function(input,output){
  
  #myStreamer <- reactive({streamTweet(input$hashtag)})
  
  myEvent <- eventReactive(input$hashtag, {streamTweet(input$hashtag, input$timer)})
  
  data <- reactiveFileReader(intervalMillis = 1000, session = NULL, filePath = paste("~/stream",Sys.Date(),".csv",sep = ''), readFunc = read.csv, header = T)
  
  output$barchart <-renderPlot({
    if (!stringi::stri_isempty(input$hashtag)){ #checks if there's anything in the text field
      myEvent()
      data() %>%
        #group_by(V1) %>%
        #tally() %>%
        ggplot(data = ., aes(x = class)) + #, y = n)) +
        geom_bar(colour="#02818a",fill="#02818a") +
        #scale_y_discrete(breaks=seq(0, 100, by=10)) +
        #stat_summary(fun = mean, geom = "bar",colour="#56B4E9",fill="#56B4E9") +
        #geom_bar(stat="identity") +
        labs(title="Sentiment composition of tweets", y ="Number of tweets", x= "Sentiment") +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  output$linechart <- renderPlot({
    data() %>%
      group_by(time, class) %>%
      tally() %>%
      ggplot(data = ., aes(x = as.Date(time, origin = Sys.time()), y = n, color = class)) + 
      geom_line(size = 3, alpha = 0.5) +
      labs(title="Change of sentiments through time", y ="Number of tweets", x= "Time", color="Sentiment") +
      scale_color_brewer(palette = "Spectral", guide = "legend") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$mymap <- renderLeaflet({
    leaflet(data()) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      try(addMarkers(lat = ~lat, lng = ~lon))
  })
  
  output$tbl <- renderDataTable(data()$tweet)
})

