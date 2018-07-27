library(ggplot2)
library(magrittr)
library(plotly,warn.conflicts = FALSE)
library(dplyr,warn.conflicts = FALSE)
# install.packages("ggplot2")
#install.packages("ggplot2",lib="/usr/local/lib/R/site-library")
library(ggplot2)
library(RColorBrewer)
library(scales)
library(rsconnect,warn.conflicts = FALSE)
library(shiny)
library(ggrepel)
library(scales)
library(reshape2)
library(DT,warn.conflicts = FALSE)
library(lubridate,warn.conflicts = FALSE)
#install.packages("ggplot2",lib="/usr/local/lib/R/site-library")

# USA<-read.csv("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/Data/USvideos.csv",stringsAsFactors = FALSE)
# Germ<-read.csv("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/Data/DEvideos.csv",stringsAsFactors = FALSE)
# Fran<-read.csv("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/Data/FRvideos.csv",stringsAsFactors = FALSE)
# GB<-read.csv("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/Data/GBvideos.csv",stringsAsFactors = FALSE)
# Cana<-read.csv("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/Data/CAvideos.csv",stringsAsFactors = FALSE)
# # #
# USA_Cate<-read.csv("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/Data/US_category_id.csv",stringsAsFactors = FALSE)
# Germ_Cate<-read.csv("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/Data/DE_category_id.csv",stringsAsFactors = FALSE)
# Fran_Cate<-read.csv("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/Data/FR_category_id.csv",stringsAsFactors = FALSE)
# GB_Cate<-read.csv("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/Data/GB_category_id.csv",stringsAsFactors = FALSE)
# Cana_Cate<-read.csv("~/Documents/NYC/Shiny Proj/Youtube/Shiny_youtube/Data/CA_category_id.csv",stringsAsFactors = FALSE)
#

# 
# USA<-read.csv("./Data/USvideos.csv",stringsAsFactors = FALSE)
# Germ<-read.csv("./Data/DEvideos.csv",stringsAsFactors = FALSE)
# Fran<-read.csv("./Data/FRvideos.csv",stringsAsFactors = FALSE)
# GB<-read.csv("./Data/GBvideos.csv",stringsAsFactors = FALSE)
# Cana<-read.csv("./Data/CAvideos.csv",stringsAsFactors = FALSE)
# # #
# USA_Cate<-read.csv("./Data/US_category_id.csv",stringsAsFactors = FALSE)
# Germ_Cate<-read.csv("./Data/DE_category_id.csv",stringsAsFactors = FALSE)
# Fran_Cate<-read.csv("./Data/FR_category_id.csv",stringsAsFactors = FALSE)
# GB_Cate<-read.csv("./Data/GB_category_id.csv",stringsAsFactors = FALSE)
# Cana_Cate<-read.csv("./Data/CA_category_id.csv",stringsAsFactors = FALSE)
# 
# # #
# # #
# USA_all<- merge(USA,USA_Cate,by.x='category_id',by.y='id', all.x=TRUE)
# Germ_all<- merge(Germ,Germ_Cate,by.x='category_id',by.y='id', all.x=TRUE)
# Fran_all<- merge(Fran,Fran_Cate,by.x='category_id',by.y='id', all.x=TRUE)
# GB_all<- merge(GB,GB_Cate,by.x='category_id',by.y='id', all.x=TRUE)
# Cana_all<- merge(Cana,Cana_Cate,by.x='category_id',by.y='id', all.x=TRUE)
# # #
# Youtube_all<-rbind(USA_all,Germ_all,Fran_all,GB_all,Cana_all)

Youtube_all<-read.csv("./Data//Youtube_all.csv",stringsAsFactors = FALSE)

Youtube_all$publish_time <-substring(Youtube_all$publish_time,1,10)
Youtube_all$publish_time <-as.Date(Youtube_all$publish_time,"%Y-%m-%d")
Youtube_all$month<-format(Youtube_all$publish_time, "%Y-%m")
Youtube_all$Year<-format(Youtube_all$publish_time, "%Y")
# #
Youtube_all$category<-ifelse(Youtube_all$category_id==29,"Nonprofits & Activism",Youtube_all$category)
Youtube_all<-Youtube_all[,c('category','Country','video_id','publish_time','month','Year','channel_title','tags','views','likes','dislikes','comment_count')]
# #
Youtube_all<- as.data.frame(group_by(Youtube_all,category,Country,video_id,publish_time,month,Year,channel_title,tags) %>%
                              summarize(views=max(views),likes=max(likes),dislikes=max(dislikes),comment_count=max(comment_count)))


Youtube_all$half_month <- floor_date(Youtube_all$publish_time, unit = "week")
Youtube_all$weekday<-weekdays(Youtube_all$publish_time)


