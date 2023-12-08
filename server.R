library(shiny)
library(ggplot2)
library(dplyr)
library(dygraphs)
library(xts)
library(forecast)
library(lubridate)
library(readxl)
library(plotly)
library(fpp2)

# ===================== SERVER ===================== #

shinyServer(function(input, output) {
  #First tab 
  # ------------ Definisi
  
  #Second tab
  # ------------- Time Series Plot
  
  # ------------- Menu Upload Mandiri
  data <- reactiveVal(NULL)
  observeEvent(input$chosen_file, {
    req(input$chosen_file)
    
    # Membaca file CSV
    data_read <- read.csv(input$chosen_file$datapath)
    data(data_read)
    })
  
  
  # -------------- Menampilkan Plot Upload Mandiri
  observeEvent(input$loadBtn, {
    req(data())
    output$uploadData <- renderPlotly({
      req(data())
      if (!is.null(data()$XColumn) && !is.null(data()$YColumn)) {
        plot_ly(data(), x = ~Date, y = ~General, type = 'scatter', mode = 'lines')
      } else {
        plot_ly(data()) 
      }
    })
  })
  
  # --------------- Pola Data Time Series
  
  output$myPlot <- renderPlotly({
    # ------------ Memanggil Data
    data_kurs <- read_xlsx("C:/Users/Sofia/Downloads/kurs.xlsx")
    data_penduduk <- read_xlsx("C:/Users/Sofia/OneDrive/Documents/Semester 3/Eksplorasi dan Visualisasi Data/Dashboard EVD/penduduk.xlsx")
    
      if(input$tipe == "Data Trend"){
      p1 <- plot_ly(data_penduduk, x= data_penduduk$Tanggal, y = data_penduduk$Jumlah, type = 'scatter', mode = 'lines', marker = list(color = 'red'))
      p1 <- p1 %>% layout(title = "Jumlah Penduduk Dunia",
                          xaxis = list(title = "Tahun"),
                          yaxis = list (title = "Jumlah Penduduk"))
      bins <- seq(min(data_penduduk$Tanggal), max(data_penduduk$Tanggal), length.out <- input$slider.n+1)
      ggplotly(p1, x = ~Tanggal, y = ~Jumlah, breaks = bins)
    
    }  
      else if(input$tipe == "Data Musiman"){
      plot_ly(data_kurs, x= data_kurs$Tanggal, y = data_kurs$Terakhir, type = 'scatter', mode = 'lines')
      
    } else if(input$tipe == "Data Siklus"){
      plot_ly(data_temperatur, x= data_temperatur$AirPassengers, y = data_kurs$Terakhir, type = 'scatter', mode = 'lines')
      
    } else if(input$tipe == "Data Fluktuatif"){
      p4 <- plot_ly(data_kurs, x= data_kurs$Tanggal, y = data_kurs$Terakhir, type = 'scatter', mode = 'lines', marker = list(color = 'blue'))
      p4 <- p4 %>% layout(title = "Kurs Rupiah Terhadap Dolar",
                          xaxis = list(title = "Tahun"),
                          yaxis = list (title = "Kurs Dollar"))
      ggplotly(p4, smoothnes = input$slider.n)  #(slider smooth trendnya masih tidak berfungsi)
    }
    
  })
  
  #----------------- Smooth Trend  (smooth trendnya masih tidak berfungsi)
  
  output$PolaSmooth <- renderPlotly({
    data_kurs <- read_xlsx("C:/Users/Sofia/Downloads/kurs.xlsx")
    scatter.smooth(x = data_kurs$Tanggal, y = data_kurs$Terakhir, type = 'o', pch=20, 
                   lpars = list(col = 'red', lwd = 2), 
                   main = 'Kurs Rupiah Terhadap Dollar',
                   xlab = 'Tahun',
                   ylab = 'Kurs')
    
  })
  
  
  # Third tab
  # -------------- Forecast Plot
  
  output$trendPlot <- renderPlot({
    penduduk <- read_xlsx("C:/Users/Sofia/OneDrive/Documents/Semester 3/Eksplorasi dan Visualisasi Data/Dashboard EVD/penduduk.xlsx")
    penduduk <- penduduk[c("Tanggal", "Jumlah")]
    penduduk$Tanggal <- as.Date(penduduk$Tanggal)
    
    require(gridExtra)
    p1 <- autoplot(ts(penduduk[input$n[1]:input$n[2], "Jumlah"])) +
      ggtitle("Data Penduduk Dunia")
    
    end = dim(penduduk)[1]
    start = end - 50
    
    mod <- auto.arima(penduduk[start : end, "Jumlah"])
    data <- forecast(mod, arm = input$arm)
    p2 <- autoplot(forecast(mod, arm = input$arm)) + 
      ggtitle("Meramalkan data 5 tahun kedepan berdasarkan data 50 tahun sebelumnya")
    
    grid.arrange(p1, p2, ncol=1)
  })
  
})
