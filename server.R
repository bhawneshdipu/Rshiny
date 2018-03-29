source("global.R")
library(tidyquant)
server <- function(input, output) {

  output$plot1 <- renderPlot({
    x <- ggplot(data1,aes(x = Date, y = SUM,col = CHANNEL))+
      geom_bar(stat = "identity",size = 3,alpha = 0.5,fill = "blue")
    #ggplotly(x)
    (x)
  })
  output$plot2 <- renderPlot({
    #pie plot by day of week
    plot_ly(pac, labels = ~wday.lbl, values = ~SP10, type = 'pie',textposition = 'inside') %>%
      layout(title = 'Kol??ov? graf')
    
  })
  output$plot3 <- renderPlot({
    x <- ggplot(data1,aes(x = Date, y = data1$SUM,col = CHANNEL))+
      geom_bar(stat = "identity",size = 3,alpha = 0.5,fill = "blue")
    #ggplotly(x)
    (x)
  })
  output$plot4 <- renderPlot({
    plot_ly(data1,x = ~CHANNEL,y = ~SP11, color = ~wday.lbl,type = "box")%>%
      layout(boxmode = "group")
    
  })
  
}
