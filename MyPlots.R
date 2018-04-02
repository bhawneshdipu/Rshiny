## MyPlots.R ##


function.MyPlotSpeedBins<-function(pac){
  renderPlotly({
    #Plot of aggregation by SpeedBins
    plot_ly(pac, x = ~Index, y = ~SP1 , name = 'Number of cars in time aggregated')%>%
      add_trace(y = ~SP1, name = '0-50km/h', mode = 'lines') %>%
      add_trace(y = ~SP2, name = '50-60km/h', mode = 'lines') %>%
      add_trace(y = ~SP3, name = '60-70km/h', mode = 'lines') %>%
      add_trace(y = ~SP4, name = '70-80km/h', mode = 'lines') %>%
      add_trace(y = ~SP5, name = '80-90km/h', mode = 'lines') %>%
      add_trace(y = ~SP6, name = '90-100km/h', mode = 'lines') %>%
      add_trace(y = ~SP7, name = '100-110km/h', mode = 'lines') %>%
      add_trace(y = ~SP8, name = '110-120km/h', mode = 'lines') %>%
      add_trace(y = ~SP9, name = '120-130km/h', mode = 'lines') %>%
      add_trace(y = ~SP10, name = '130-140km/h', mode = 'lines') %>%
      add_trace(y = ~SP11, name = '140-150km/h', mode = 'lines') %>%
      add_trace(y = ~SP12, name = '150-160km/h', mode = 'lines') %>%
      add_trace(y = ~SP13, name = '160-180km/h', mode = 'lines')%>%
      add_trace(y = ~SP14, name = '180-999km/h', mode = 'lines')%>%
      add_trace(y = ~SUM, name = 'TOTAL', mode = 'lines')
    
  })
}

function.MyPlotLengthBins<-function(pac){
  renderPlotly({
    #Plot of aggregation by LengthBins
    plot_ly(pac, x = ~Index, y = ~LN1 , name = 'Number of cars in time aggregated')%>%
      add_trace(y = ~LN1, name = '0-300 cm', mode = 'lines') %>%
      #add_trace(y = ~LN2, name = '300-470 cm', mode = 'lines') %>%
      add_trace(y = ~LN3, name = '470-550 cm', mode = 'lines') %>%
      add_trace(y = ~LN4, name = '550-600 cm', mode = 'lines') %>%
      add_trace(y = ~LN5, name = '600-1300 cm', mode = 'lines') %>%
      add_trace(y = ~LN6, name = '1300-1800 cm', mode = 'lines') %>%
      add_trace(y = ~LN7, name = '1800-2550 cm', mode = 'lines') %>%
      add_trace(y = ~LN8, name = '2550-3600 cm', mode = 'lines') %>%
      add_trace(y = ~LN9, name = '3600-9999 cm', mode = 'lines') 
  })
  
}

function.MyPlotWeightBins<-function(pac){
  renderPlotly({
    #Plot of aggregation by WeigthBins
    plot_ly(data1, x = ~Date, y = ~CS1 , name = 'Number of cars in time aggregated')%>%
      add_trace(y = ~CS1, name = '0-2000 kg', mode = 'lines') %>%
      add_trace(y = ~CS2, name = '2000-3501 kg', mode = 'lines') %>%
      add_trace(y = ~CS3, name = '3501-7497 kg', mode = 'lines') %>%
      add_trace(y = ~CS4, name = '7497-12002 kg', mode = 'lines') %>%
      add_trace(y = ~CS5, name = '12002-17998  kg', mode = 'lines') %>%
      add_trace(y = ~CS6, name = '17998-23999  kg', mode = 'lines') 
  })
}