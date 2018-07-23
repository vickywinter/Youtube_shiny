library(magrittr)
library(plotly,warn.conflicts = FALSE)
library(dplyr,warn.conflicts = FALSE)
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
cate_trend<-function(year,country){
  library(plotly)
  Youtube_country<-filter(Youtube_all,Country %in% country,Year==year)
  cate_trend<- group_by(Youtube_country,Year,category) %>% summarize(number=n(),avg_views=round(mean(views)),tol_views=sum(as.numeric(views)))
  
  cate_trend_year <- cate_trend%>% 
    mutate(per=number/sum(number),per_view=tol_views/sum(tol_views)) %>% 
    arrange(desc(category))
  
  cate_trend_year<-cate_trend_year[order(-cate_trend_year$number),]
  
  cate_trend_year$label <- scales::percent(cate_trend_year$per)
  cate_trend_year$label2<-ifelse(cate_trend_year$per<=0.05, "",cate_trend_year$label)
  cate_trend_year$category2<-ifelse(cate_trend_year$per<=0.05, "",cate_trend_year$category)
  
  library(plotly)
  #install.packages("plotly")
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  
  p <- plot_ly(cate_trend_year, labels = ~category, values = ~number, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste(category, "\n","Market Share:",label, "\n","Total Videos:",number, "\n","Avg Views:", avg_views),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) %>%
    layout(title = 'Youtube Market Share of Category',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p$elementId <- NULL
  
  #cate_trend_year2<-cate_trend_year[,c('category','per','per_view')]
  #library(reshape2)
  #cate_trend_year3<-melt(as.data.frame(cate_trend_year2),id.vars='category')
  
  #par(mfrow=c(2,1)) 
  p
  #ggplot(cate_trend_year3,aes(x=category,y=value,fill=variable))+
    #geom_bar(stat='identity',position='dodge')
}

Number_likes<-function(year, country,list_data){
  Youtube_country<-filter(Youtube_all,Country %in% country,Year==year)
  cate_trend<- group_by(Youtube_country,Year,category) %>% summarize(number=n(),tol_like=sum(as.numeric(likes)),tol_views=sum(as.numeric(views)),tol_dislike=sum(as.numeric(dislikes)),tol_comm=sum(as.numeric(comment_count)))
  
  cate_trend_year <- cate_trend%>% 
    mutate(Video_cnt=number/sum(number),Views=tol_views/sum(tol_views),Likes=tol_like/sum(tol_like),Dislikes=tol_dislike/sum(tol_dislike),Comments_count=tol_comm/sum(tol_comm)) %>% 
    arrange(desc(category))
  data_list=append(list_data,'category')
  cate_trend_year2<-cate_trend_year[,data_list]
  library(reshape2)
  cate_trend_year3<-melt(as.data.frame(cate_trend_year2),id.vars='category')
  ggplot(cate_trend_year3,aes(x=category,y=value,fill=variable))+
    geom_bar(stat='identity',position='dodge')+
    ylab("Percentage of total")+
    xlab("")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

Tag<-function(year, tag_country,tag_cate,min, max){
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer")
  library("tidyr")
  
  Tag<-filter(Youtube_all,tags!="[none]",Year==year,Country %in% tag_country,category %in% tag_cate)[,c("Year","tags")]
  Sys.setlocale('LC_ALL','C') 
  Tag1<-data.frame(tags=unlist(strsplit(Tag$tags,"|",fixed=TRUE)))
  
  Tag1$tags <- gsub('"', '', Tag1$tags)
  
  Tag1$tags<-tolower(Tag1$tags)
  Tag1$tags<-gsub('trailers', '', Tag1$tags)
  Tag1$tags<-gsub('trailer', '', Tag1$tags)
  Tag1$tags<-gsub('movies', '', Tag1$tags)
  Tag1$tags<-gsub('movie', '', Tag1$tags)
  Tag1$tags<-gsub(year, '', Tag1$tags)
  Tag1$tags<-gsub('previews', '', Tag1$tags)
  Tag1$tags<-gsub('preview', '', Tag1$tags)
  Tag1$tags<-gsub('reviews', '', Tag1$tags)
  Tag1$tags<-gsub('review', '', Tag1$tags)
  Tag1$tags<-gsub('official', '', Tag1$tags)
  
  Tag1$tags<-gsub('channel', '', Tag1$tags)
  Tag1$tags<-gsub('blogs', '', Tag1$tags)
  Tag1$tags<-gsub('blog', '', Tag1$tags)
  Tag1$tags<-gsub('videos', '', Tag1$tags)
  Tag1$tags<-gsub('video', '', Tag1$tags)
  Tag1$tags<-trimws(Tag1$tags)
  
  Tag2<-filter(Tag1,!((!is.na(Tag1$tags) & Tag1$tags=="")| Tag1$tags==" "))
  Tag2<-as.data.frame(table(Tag2))
  
  wordcloud(words = Tag2$Tag2, freq = Tag2$Freq, min.freq = as.numeric(min),
            max.words=as.numeric(max), random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

Time_trend1<-function(view_data,Trend_country,Trend_category){
  Youtube_trend<-filter(Youtube_all,Country %in% Trend_country,month>'2017-10',category==Trend_category)
  month_trend<- group_by(Youtube_trend,half_month,category) %>% summarize(number=n(),tol_like=sum(as.numeric(likes)),tol_views=sum(as.numeric(views)),tol_dislike=sum(as.numeric(dislikes)),tol_comm=sum(as.numeric(comment_count)))
  # 
  # ggplot(month_trend,aes(x=half_month,y=number,fill=number))+
  #   geom_bar(stat='identity',position='dodge')

  y_data=view_data[2]
  ggplot(month_trend,aes_string(x='half_month',y=view_data,fill=view_data))+
   geom_bar(stat='identity',position='dodge')+
   ylab("Video Trend")+
   xlab("")+
   scale_x_date(date_breaks = "week", labels=date_format("%Y-%m-%d"))+
   geom_line()+geom_point()+
   theme_classic()+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

Time_trend2<-function(view_data,Trend_country,Trend_category){
  Youtube_trend<-filter(Youtube_all,Country %in% Trend_country,month>'2017-10',category==Trend_category)
  month_trend<- group_by(Youtube_trend,half_month,category) %>% summarize(number=n(),tol_like=sum(as.numeric(likes)),tol_views=sum(as.numeric(views)),tol_dislike=sum(as.numeric(dislikes)),tol_comm=sum(as.numeric(comment_count)))


  weekday_trend<- group_by(Youtube_trend,weekday,category) %>% summarize(number=n(),tol_like=sum(as.numeric(likes)),tol_views=sum(as.numeric(views)),tol_dislike=sum(as.numeric(dislikes)),tol_comm=sum(as.numeric(comment_count)))
  weekday_trend$weekday<-factor(weekday_trend$weekday, levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))


  ggplot(weekday_trend,aes_string(x='weekday',y=view_data,fill=view_data))+
    geom_bar(stat='identity',position='dodge')+
    scale_fill_gradient(low="red", high="orange")+
    ylab("Video Trend")+
    xlab("")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


}

Channel<-function(cha_year,cha_country,cha_cate){
  Youtube_channel<-as.data.frame(filter(Youtube_all,Year==cha_year,Country %in% cha_country,category %in% cha_cate))[,c('channel_title','views','likes','dislikes','comment_count')]
  
  channel_sum<-group_by(Youtube_channel,channel_title) %>% summarize(avg_view=round(mean(views)),upload_video_cnt=n(),avg_like=round(mean(likes)),avg_dislike=round(mean(dislikes)),avg_com=round(mean(comment_count)))
  channel_sum<-channel_sum[order(channel_sum$avg_view,decreasing=TRUE),]
  channel_sum
  
}