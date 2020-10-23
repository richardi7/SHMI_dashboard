#DarkLightDark ribbon
bar.figDLD <- function(df){
  
  bar<- data.frame(
    y = c (1))
  
  #band values
  bar$bar0value=min(df$value, na.rm = TRUE)
  bar$bar10value=quantile(df$value, prob = c(0.1), na.rm = TRUE)
  bar$bar20value=quantile(df$value, prob = c(0.20), na.rm = TRUE)
  bar$bar25value=quantile(df$value, prob = c(0.25), na.rm = TRUE)
  bar$bar30value=quantile(df$value, prob = c(0.30), na.rm = TRUE)
  bar$bar40value=quantile(df$value, prob = c(0.40), na.rm = TRUE)
  bar$bar50value=quantile(df$value, prob = c(0.5), na.rm = TRUE)
  bar$bar60value=quantile(df$value, prob = c(0.6), na.rm = TRUE)
  bar$bar75value=quantile(df$value, prob = c(0.75), na.rm = TRUE)
  bar$bar80value=quantile(df$value, prob = c(0.80), na.rm = TRUE)
  bar$bar90value=quantile(df$value, prob = c(0.90), na.rm = TRUE)
  bar$bar100value=max(df$value, na.rm = TRUE)
  
  
  #gap values
  bar$bar0=bar$bar0value #min
  bar$bar10=bar$bar10value-bar$bar0value      #min to 10
  bar$bar20=bar$bar20value-bar$bar10value     #10 to 20
  bar$bar25=bar$bar25value-bar$bar10value    #10 to 25
  bar$bar40=bar$bar40value-bar$bar20value   #20 to 40
  bar$bar50=bar$bar50value-bar$bar25value   #25 to 50
  bar$bar60=bar$bar60value-bar$bar40value   #40 to 60
  bar$bar75=bar$bar75value-bar$bar50value   #50 to 75
  bar$bar80=bar$bar80value-bar$bar60value   #60 to 80
  bar$bar90=bar$bar90value-bar$bar80value   #80 to 90
  bar$bar100=bar$bar100value-bar$bar90value #90 to 100
  
  dot_trust<-df%>%filter(ProviderCode ==params$trust)
  dot_trust$dot_trust_y=1
  
  dot_alltrusts<-df%>%filter(ProviderCode !=params$trust)
  dot_alltrusts$y<-c(1)
  
  test_fig<- plot_ly(bar, x = ~bar0, y = ~y,name ="Minimum", type = 'bar', orientation = 'h',
                     marker = list(color = 'white', width = 1),hoverinfo = 'text',
                     text=~paste('Lowest:', round(bar0value,1)))
  test_fig<- test_fig %>% add_trace(bar, x = ~bar10, y = ~y,name ="Lowest tenth", type = 'bar', 
                                    marker = list(color = 'rgba(33,113,181, 0.8)', width = 1),hoverinfo = 'text',
                                    text=~paste('10th centile:', round(bar10value,1)))
  test_fig <- test_fig %>% add_trace(x = ~bar20, name ="Lowest 20%", marker = list(color = 'rgba(107,174,214, 0.8)'),
                                     hoverinfo = 'text',text=~paste('20th centile: ', round(bar20value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar40, name ="20-40%", marker = list(color = 'rgba(189,215,231, 0.8)'),
                                     hoverinfo = 'text',text=~paste('40th centile: ', round(bar40value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar60,  name ="Median",marker = list(color = 'rgba(239,243,255, 0.8)'),
                                     hoverinfo = 'text',text=~paste('Midrange: ', round(bar60value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar80, name ="60-80%", marker = list(color = 'rgba(189,215,231, 0.8)'),
                                     hoverinfo = 'text',text=~paste('80th centile: ', round(bar80value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar90,  name ="Highest 10%",marker = list(color = 'rgba(107,174,214, 0.8)'),
                                     hoverinfo = 'text',text=~paste('90th centile: ', round(bar90value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar100,name ="maxlimit", marker = list(color = 'rgba(33,113,181, 0.8)'),
                                     hoverinfo = 'text',text=~paste('Highest tenth: ', round(bar100value,1)) )
  test_fig<- test_fig %>% add_trace(x = ~dot_alltrusts$value, y = ~dot_alltrusts$y, 
                                    type='scatter',name="trusts", mode = 'markers',  marker = list(size=7,color='grey', line = list(color = 'grey')) ,
                                    hoverinfo = 'text',text=~paste(dot_alltrusts$Shortname,'<br>Value: ', round(dot_alltrusts$value,1)) )
  test_fig<- test_fig %>% add_trace(x = ~dot_trust$value, y = ~dot_trust$dot_trust_y, 
                                    type='scatter',name="Trust", mode = 'markers',  marker = list(size=10,color='orange', line = list(color = 'orange')),
                                    hoverinfo = 'text',text=~paste(dot_trust$Shortname,'<br>Value: ', round(dot_trust$value,1)))
  test_fig <- test_fig %>% layout(xaxis = list(title = "",range = c(bar$bar0value, bar$bar100value),
                                               showgrid = FALSE,
                                               showline = FALSE,
                                               showticklabels = FALSE,
                                               zeroline = FALSE),
                                  yaxis = list(title = "",
                                               showgrid = FALSE,
                                               showline = FALSE,
                                               showticklabels = FALSE,
                                               zeroline = FALSE),
                                  barmode = 'stack',showlegend=FALSE)
}

#orange blue low bad ribbon
bar.figOB <- function(df){
  
  bar<- data.frame(
    y = c (1))
  
  bar$bar0value=min(df$value, na.rm = TRUE)
  bar$bar10value=quantile(df$value, prob = c(0.1), na.rm = TRUE)
  bar$bar20value=quantile(df$value, prob = c(0.20), na.rm = TRUE)
  bar$bar25value=quantile(df$value, prob = c(0.25), na.rm = TRUE)
  bar$bar30value=quantile(df$value, prob = c(0.30), na.rm = TRUE)
  bar$bar40value=quantile(df$value, prob = c(0.40), na.rm = TRUE)
  bar$bar50value=quantile(df$value, prob = c(0.5), na.rm = TRUE)
  bar$bar60value=quantile(df$value, prob = c(0.6), na.rm = TRUE)
  bar$bar75value=quantile(df$value, prob = c(0.75), na.rm = TRUE)
  bar$bar80value=quantile(df$value, prob = c(0.80), na.rm = TRUE)
  bar$bar90value=quantile(df$value, prob = c(0.90), na.rm = TRUE)
  bar$bar100value=max(df$value, na.rm = TRUE)
  
  
  #gap values
  bar$bar0=bar$bar0value #min
  bar$bar10=bar$bar10value-bar$bar0value      #min to 10
  bar$bar20=bar$bar20value-bar$bar10value  #10 to 20
  bar$bar25=bar$bar25value-bar$bar10value #10 to 25
  bar$bar40=bar$bar40value-bar$bar20value   #20 to 40
  bar$bar50=bar$bar50value-bar$bar25value   #25 to 50
  bar$bar60=bar$bar60value-bar$bar40value   #40 to 60
  bar$bar75=bar$bar75value-bar$bar50value   #50 to 75
  bar$bar80=bar$bar80value-bar$bar60value   #60 to 80
  bar$bar90=bar$bar90value-bar$bar80value   #80 to 90
  bar$bar100=bar$bar100value-bar$bar90value #90 to 100
  
  dot_trust<-df%>%filter(ProviderCode ==params$trust)
  dot_trust$dot_trust_y=1
  
  dot_alltrusts<-df%>%filter(ProviderCode !=params$trust)
  dot_alltrusts$y<-c(1)
  
  test_fig<- plot_ly(bar, x = ~bar0, y = ~y,name ="Minimum", type = 'bar', orientation = 'h',
                     marker = list(color = 'white', width = 1),hoverinfo = 'text',
                     text=~paste('Lowest:', round(bar10value,1)))
  test_fig<- test_fig %>% add_trace(bar, x = ~bar10, y = ~y,name ="Lowest tenth", type = 'bar', 
                                    marker = list(color = 'rgba(215,48,39, 0.8)', width = 1),hoverinfo = 'text',
                                    text=~paste('10th centile:', round(bar10value,1)))
  test_fig <- test_fig %>% add_trace(x = ~bar20, name ="Lowest 20%", marker = list(color = 'rgba(252,141,89, 0.8)'),
                                     hoverinfo = 'text',text=~paste('20th centile: ', round(bar20value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar40, name ="20-40%", marker = list(color = 'rgba(254,224,144, 0.8)'),
                                     hoverinfo = 'text',text=~paste('40th centile: ', round(bar40value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar60,  name ="Median",marker = list(color = 'rgba(255,255,191, 0.8)'),
                                     hoverinfo = 'text',text=~paste('Midrange: ', round(bar60value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar80, name ="60-80%", marker = list(color = 'rgba(224,243,248, 0.8)'),
                                     hoverinfo = 'text',text=~paste('80th centile: ', round(bar80value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar90,  name ="Highest 10%",marker = list(color = 'rgba(145,191,219, 0.8)'),
                                     hoverinfo = 'text',text=~paste('90th centile: ', round(bar90value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar100,name ="maxlimit", marker = list(color = 'rgba(69,117,180, 0.8)'),
                                     hoverinfo = 'text',text=~paste('Highest tenth: ', round(bar100value,1)) )
  test_fig<- test_fig %>% add_trace(x = ~dot_alltrusts$value, y = ~dot_alltrusts$y, 
                                    type='scatter',name="trusts", mode = 'markers',  marker = list(size=7,color='grey', line = list(color = 'grey')) ,
                                    hoverinfo = 'text',text=~paste(dot_alltrusts$Shortname,'<br>Value: ', round(dot_alltrusts$value,1)) )
  test_fig<- test_fig %>% add_trace(x = ~dot_trust$value, y = ~dot_trust$dot_trust_y, 
                                    type='scatter',name="Trust", mode = 'markers',  marker = list(size=10,color='orange', line = list(color = 'orange')),
                                    hoverinfo = 'text',text=~paste(dot_trust$Shortname,'<br>Value: ', round(dot_trust$value,1)))
  test_fig <- test_fig %>% layout(xaxis = list(title = "",range = c(bar$bar0value, bar$bar100value),
                                               showgrid = FALSE,
                                               showline = FALSE,
                                               showticklabels = FALSE,
                                               zeroline = FALSE),
                                  yaxis = list(title = "",
                                               showgrid = FALSE,
                                               showline = FALSE,
                                               showticklabels = FALSE,
                                               zeroline = FALSE),
                                  barmode = 'stack',showlegend=FALSE)
}

#blue orange hi bad ribbon
bar.figBO <- function(df){
  
  bar<- data.frame(
    y = c (1))  
  
  bar$bar0value=min(df$value, na.rm = TRUE)
  bar$bar10value=quantile(df$value, prob = c(0.1), na.rm = TRUE)
  bar$bar20value=quantile(df$value, prob = c(0.20), na.rm = TRUE)
  bar$bar25value=quantile(df$value, prob = c(0.25), na.rm = TRUE)
  bar$bar30value=quantile(df$value, prob = c(0.30), na.rm = TRUE)
  bar$bar40value=quantile(df$value, prob = c(0.40), na.rm = TRUE)
  bar$bar50value=quantile(df$value, prob = c(0.5), na.rm = TRUE)
  bar$bar60value=quantile(df$value, prob = c(0.6), na.rm = TRUE)
  bar$bar75value=quantile(df$value, prob = c(0.75), na.rm = TRUE)
  bar$bar80value=quantile(df$value, prob = c(0.80), na.rm = TRUE)
  bar$bar90value=quantile(df$value, prob = c(0.90), na.rm = TRUE)
  bar$bar100value=max(df$value, na.rm = TRUE)
  
  
  #gap values
  bar$bar0=bar$bar0value #min
  bar$bar10=bar$bar10value-bar$bar0value      #min to 10
  bar$bar20=bar$bar20value-bar$bar10value  #10 to 20
  bar$bar25=bar$bar25value-bar$bar10value #10 to 25
  bar$bar40=bar$bar40value-bar$bar20value   #20 to 40
  bar$bar50=bar$bar50value-bar$bar25value   #25 to 50
  bar$bar60=bar$bar60value-bar$bar40value   #40 to 60
  bar$bar75=bar$bar75value-bar$bar50value   #50 to 75
  bar$bar80=bar$bar80value-bar$bar60value   #60 to 80
  bar$bar90=bar$bar90value-bar$bar80value   #80 to 90
  bar$bar100=bar$bar100value-bar$bar90value #90 to 100
  
  dot_trust<-df%>%filter(ProviderCode ==params$trust)
  dot_trust$dot_trust_y=1
  
  dot_alltrusts<-df%>%filter(ProviderCode !=params$trust)
  dot_alltrusts$y<-c(1)
  
  test_fig<- plot_ly(bar, x = ~bar0, y = ~y,name ="Minimum", type = 'bar', orientation = 'h',
                     marker = list(color = 'white', width = 1),hoverinfo = 'text',
                     text=~paste('Lowest:', round(bar10value,1)))
  test_fig<- test_fig %>% add_trace(bar, x = ~bar10, y = ~y,name ="Lowest tenth", type = 'bar', 
                                    marker = list(color = 'rgba(69,117,180, 0.8)', width = 1),hoverinfo = 'text',
                                    text=~paste('10th centile:', round(bar10value,1)))
  test_fig <- test_fig %>% add_trace(x = ~bar20, name ="Lowest 20%", marker = list(color = 'rgba(145,191,219, 0.8)'),
                                     hoverinfo = 'text',text=~paste('20th centile: ', round(bar20value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar40, name ="20-40%", marker = list(color = 'rgba(224,243,248, 0.8)'),
                                     hoverinfo = 'text',text=~paste('40th centile: ', round(bar40value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar60,  name ="Median",marker = list(color = 'rgba(255,255,191, 0.8)'),
                                     hoverinfo = 'text',text=~paste('Midrange: ', round(bar60value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar80, name ="60-80%", marker = list(color = 'rgba(254,224,144, 0.8)'),
                                     hoverinfo = 'text',text=~paste('80th centile: ', round(bar80value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar90,  name ="Highest 10%",marker = list(color = 'rgba(252,141,89, 0.8)'),
                                     hoverinfo = 'text',text=~paste('90th centile: ', round(bar90value,1)) )
  test_fig <- test_fig %>% add_trace(x = ~bar100,name ="maxlimit", marker = list(color = 'rgba(215,48,39, 0.8)'),
                                     hoverinfo = 'text',text=~paste('Highest tenth: ', round(bar100value,1)) )
  test_fig<- test_fig %>% add_trace(x = ~dot_alltrusts$value, y = ~dot_alltrusts$y, 
                                    type='scatter',name="trusts", mode = 'markers',  marker = list(size=7,color='grey', line = list(color = 'grey')) ,
                                    hoverinfo = 'text',text=~paste(dot_alltrusts$Shortname,'<br>Value: ', round(dot_alltrusts$value,1)) )
  test_fig<- test_fig %>% add_trace(x = ~dot_trust$value, y = ~dot_trust$dot_trust_y, 
                                    type='scatter',name="Trust", mode = 'markers',  marker = list(size=10,color='orange', line = list(color = 'orange')),
                                    hoverinfo = 'text',text=~paste(dot_trust$Shortname,'<br>Value: ', round(dot_trust$value,1)))
  test_fig <- test_fig %>% layout(xaxis = list(title = "",range = c(bar$bar0value, bar$bar100value),
                                               showgrid = FALSE,
                                               showline = FALSE,
                                               showticklabels = FALSE,
                                               zeroline = FALSE),
                                  yaxis = list(title = "",
                                               showgrid = FALSE,
                                               showline = FALSE,
                                               showticklabels = FALSE,
                                               zeroline = FALSE),
                                  barmode = 'stack',showlegend=FALSE)
}

bar.text <- function(df,x){
  
  dot_trust<-df%>%filter(ProviderCode ==params$trust)
  
  return_text=case_when(
    dot_trust$value==min(df$value, na.rm = TRUE) ~ paste0(dot_trust$Shortname," has the lowest ",x),
    dot_trust$value==max(df$value, na.rm = TRUE) ~ paste0(dot_trust$Shortname," is the highest for ",x,"."),
    dot_trust$value>quantile(df$value, prob = c(0.90), na.rm = TRUE)~ paste0(dot_trust$Shortname," is in the highest 10% of trusts for ",x,"."),
    dot_trust$value<quantile(df$value, prob = c(0.1), na.rm = TRUE)~ paste0(dot_trust$Shortname," is in the lowest 10% of trusts for ",x,"."),
    dot_trust$value<quantile(df$value, prob = c(0.2), na.rm = TRUE) ~ paste0(dot_trust$Shortname," is in the lowest 20% of trusts for ",x,"."),
    dot_trust$value<quantile(df$value, prob = c(0.4), na.rm = TRUE) ~ paste0(dot_trust$Shortname," is in the lowest 40% of trusts for ",x,"."),
    dot_trust$value<quantile(df$value, prob = c(0.6), na.rm = TRUE)~ paste0(dot_trust$Shortname," is in the midrange of trusts for ",x,"."),
    dot_trust$value<quantile(df$value, prob = c(0.8), na.rm = TRUE)~ paste0(dot_trust$Shortname," is above the average for ",x,"."),
    dot_trust$value>quantile(df$value, prob = c(0.80), na.rm = TRUE)~ paste0(dot_trust$Shortname," is in the highest 20% of trusts for ",x,"."),
    TRUE ~ " "
  )
  return(return_text)
}