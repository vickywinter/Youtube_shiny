library(shiny)
library(ggplot2)
library(plotly,warn.conflicts = FALSE)
library(dplyr,warn.conflicts = FALSE)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(reshape2)
library(DT,warn.conflicts = FALSE)

shinyUI(navbarPage("Youtube Video Analysis",
                   tabPanel("User Interact Data",
                            fluidRow(
                              column(12,
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: NA}")),
                                         sliderInput("year",
                                                     "Chose a year:",
                                                     min=2007,
                                                     max=2018,
                                                     value=2018,
                                                     sep = ""
                                         ),
                                         checkboxGroupInput('country',
                                                            'Chose Countries',
                                                            choiceNames =unique(Youtube_all$Country),
                                                            choiceValues=unique(Youtube_all$Country),
                                                            selected=unique(Youtube_all$Country)),
                                         textOutput("txt")
                                       ),
                                       mainPanel(
                                         plotlyOutput("PieChart")
                                       )
                                     )),
                              column(12,
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: NA}")),
                                         checkboxGroupInput('Bar_data',
                                                            'Chose data to view:',
                                                            choiceNames =c("Total Video Amount","Total Views","No. of Likes","No. of Dislike","No. of Commonts"),
                                                            choiceValues=c("Video_cnt","Views","Likes","Dislikes","Comments_count"),
                                                            selected=c("Video_cnt","Views","Likes","Dislikes","Comments_count"))
                                         
                                       ),
                                       mainPanel(
                                         plotOutput("bar")
                                       )
                                     ))
                            )
                            
                            ),
                   tabPanel("Video Trend by Category",
                            fluidPage(
                              titlePanel("Trend"),
                              sidebarLayout(
                                sidebarPanel(
                                  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: NA}")),
                                  selectInput('Trend_country',
                                                     'Chose Countries:',
                                                     choice =unique(Youtube_all$Country)),
                                  selectInput("Trend_category", "Category:",
                                              choices=unique(Youtube_all$category)),
                                  selectInput('view_data',
                                                     'Chose data to view:',
                                                     c("Total Video Amount"='number',"Total Views"='tol_views',"No. of Likes"='tol_like',"No. of Dislike"='tol_dislike',"No. of Commonts"='tol_comm'),
                                                    selected=c('tol_views')
                                                     )
                                ),
                                mainPanel(
                                  plotOutput("Trend1")
                                  ,plotOutput("Trend2")
                                )
                              ))

                   ),
                   tabPanel("KeyWords",
                            fluidPage(
                              titlePanel("Tag Keyword Word Cloud"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: NA}")),
                                         sliderInput("Tag_year",
                                                     "Chose a year:",
                                                     min=2007,
                                                     max=2018,
                                                     value=2018,
                                                     sep = ""
                                         ),
                                         checkboxGroupInput('Tag_country',
                                                            'Chose Countries',
                                                            choiceNames =unique(Youtube_all$Country),
                                                            choiceValues=unique(Youtube_all$Country),
                                                            selected=unique(Youtube_all$Country)),
                                         selectInput("Tag_category", "Category:",
                                                     choices=unique(Youtube_all$category)),
                                         textInput("min", "Min Frequency", 1),
                                         textInput("wd_max", "Words Capacity", value=10)
                                       ),
                                       mainPanel(
                                         plotOutput("Tag_keywords"),
                                         dataTableOutput("Channel_Rank")
                                       )
                                     ))
                                     
                            )
                   )
        )