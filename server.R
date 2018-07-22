library(plotly)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
#install.packages("ggrepel")
library(ggrepel)
library(scales)
#source("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/helper.R", local = TRUE)
source("~/helper.R", local = TRUE)
shinyServer(function(input,output){
  #source("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/helper.R", local = TRUE)
  output$PieChart=renderPlotly({
    library(ggplot2)
    return(cate_trend(input$year,input$country))
  })
  output$bar=renderPlot({

    return(Number_likes(input$year,input$country,input$Bar_data))
  })
  
  output$Tag_keywords=renderPlot({
    #args$year=input$year
    #args$country=input$country
    #do.call(cate_trend,list(input$year,input$country))
    return(Tag(input$Tag_year,input$Tag_country,input$Tag_category,input$min,input$wd_max))
  })
  
  output$Trend1=renderPlot({
    return(Time_trend1(input$view_data,input$Trend_country,input$Trend_category))
  })
  
  output$Trend2=renderPlot({
    return(Time_trend2(input$view_data,input$Trend_country,input$Trend_category))
  })
})