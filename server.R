library(shiny)
library(ggplot2)
library(dplyr)
library(dygraphs)
library(xts)
library(forecast)
library(lubridate)
library(readxl)


shinyServer(function(input, output) {
  
  # Live Chart
  output$myPlot <- renderPlot({
    
    # reading in the data from the API
    data_kurs <- read_xlsx("C:/Users/Sofia/Downloads/kurs.xlsx")
    
    # need to convert the dates to Date class in order to use scale_x_data
    Date_kurs_class <- as.Date(data_kurs$Tanggal)
    
    if(input$type == "Data Trend"){
      
      ggplot(data_kurs, aes(x = Date_kurs_class, y = Terakhir, group = 1)) +
        geom_line(color = "DarkBlue") + 
        xlab("Periode") +
        ylab("Kurs Dollar") +
        # spreads out the x values and lanels them by their respective months and dates
        scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
        # makes the labels slanted by 45 degrees
        theme_classic() +
        theme(axis.text.x = element_text(angle = -45, vjust = 0)) +
        ggtitle("Kurs Rupiah Terhadap Dollar")
    } else if(input$type == "Input Mandiri"){
      
      ggplot(data_kurs, aes(x = Date_kurs_class, y = Terakhir, group = 1)) +
        geom_line(color = "DarkGreen") + 
        xlab("Periode") +
        ylab("Kurs Dollar") +
        # spreads out the x values and lanels them by their respective months and dates
        scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
        # makes the labels slanted by 45 degrees
        theme_classic() +
        theme(axis.text.x = element_text(angle = -45, vjust = 0)) +
        ggtitle("Kurs Rupiah Terhadap Dollar")
      
    } else if(input$type == 3){
      
      ggplot(data_kurs, aes(x = Date_kurs_class, y = Terakhir, group = 1)) +
        geom_line(color = "DarkBlue") + 
        xlab("Periode") +
        ylab("Kurs Dollar") +
        # spreads out the x values and lanels them by their respective months and dates
        scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
        # makes the labels slanted by 45 degrees
        theme_classic() +
        theme(axis.text.x = element_text(angle = -45, vjust = 0)) +
        ggtitle("Kurs Rupiah Terhadap Dollar")
    } else if(input$type == 4){
      
      ggplot(data_kurs, aes(x = Date_kurs_class, y = Terakhir, group = 1)) +
        geom_line(color = "DarkBlue") + 
        xlab("Periode") +
        ylab("Kurs Dollar") +
        # spreads out the x values and lanels them by their respective months and dates
        scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
        # makes the labels slanted by 45 degrees
        theme_classic() +
        theme(axis.text.x = element_text(angle = -45, vjust = 0)) +
        ggtitle("Kurs Rupiah Terhadap Dollar")
    } else if(input$type == 5){
      
      ggplot(data_kurs, aes(x = Date_kurs_class, y = Terakhir, group = 1)) +
        geom_line(color = "DarkBlue") + 
        xlab("Periode") +
        ylab("Kurs Dollar") +
        # spreads out the x values and lanels them by their respective months and dates
        scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
        # makes the labels slanted by 45 degrees
        theme_classic() +
        theme(axis.text.x = element_text(angle = -45, vjust = 0)) +
        ggtitle("Kurs Rupiah Terhadap Dollar")
    }
  })
  
  # second plot
  # must use renderDygraph instead of renderPlot
  output$dygraph <- renderDygraph({
    
    data <- get_data(from = input$start2, to = input$end2, stock_symbol = input$company2, view_type = input$type2, api_key = "Q86JK3PHWQIELR2K")
    
    new_dat <- data %>%
      arrange(Dates)
    
    start_date <- as.Date(new_dat[1,1])
    end_date <- as.Date(new_dat[length(new_dat$Dates), 1])
    
    inds <- seq(start_date, end_date, by = "day")
    
    uni_data <- new_dat %>%
      select(Stock_data)
    
    zoo.obj <- zoo(uni_data, inds)
    
    model = auto.arima(zoo.obj)
    
    predict = forecast(model, h = input$predict) # number of days predicted
    
    predict %>%
      {cbind(actuals=.$x, forecast_mean=.$mean)} %>%
      dygraph()
    
    predict %>%
      {cbind(actuals=.$x, forecast_mean=.$mean,
             lower_95=.$lower[,"95%"], upper_95=.$upper[,"95%"],
             lower_80=.$lower[,"80%"], upper_80=.$upper[,"80%"])} %>%
      dygraph() %>%
      dySeries("actuals", color = "black") %>%
      dySeries(c("lower_80", "forecast_mean", "upper_80"),
               label = "80%", color = "blue") %>%
      dySeries(c("lower_95", "forecast_mean", "upper_95"),
               label = "95%", color = "blue") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
})