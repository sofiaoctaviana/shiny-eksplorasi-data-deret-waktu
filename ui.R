library(shiny)
library(dygraphs)

======================UI=========================
shinyUI(navbarPage(
  
  # Application title
  "Dashboard Eksplorasi Data Deret Waktu",
  
  # applies a theme to the shiny app using shinythemes package
 # theme = shinytheme("flatly"),
  
  # first tab panel
  tabPanel("Time Series Plot",
           # Sidebar inputs
           sidebarLayout(
             sidebarPanel(
               # first argument is the name of the input(doesn't really matter i guess)
               # the second argument is the title of the input that you will see
               
               #textInput("company", 
                      #   label = "Enter a company stock symbol:",
                       #  "IBM"),
               dateInput("start", 
                         label = "Tanggal Awal Analisis:",
                         value = "2019-01-01"),
               dateInput("end", 
                         label = "Tanggal Akhir Analisis:",
                         value = "2023-10-30"),
              # textInput("type",
                         #   label = "Enter Type of Stock Data (1-5):"),
               selectInput("type", label = "Pilih Pola data", 
                           choices = c("Input Mandiri", "Data Trend", "Data Musiman ", "Data Siklus","Data Fluktuatif" ))
             ),
             
             # Shows first plot from server file
             mainPanel(
               plotOutput("myPlot"))
           )
  ),
  
  # second tab panel
  tabPanel("Forcast Plot",
           sidebarLayout(
             sidebarPanel(
               # first argument is the name of the input(doesn't really matter i guess)
               # the second argument is the title of the slider or select input that you will see
               textInput("company2", 
                         label = "Enter a company stock symbol:",
                         "IBM"),
               dateInput("start2", 
                         label = "Enter start date for your analysis:",
                         value = "2017-05-26"),
               dateInput("end2", 
                         label = "Enter end date for your analysis:",
                         value = "2020-06-02"),
               numericInput("type2",
                            label = "Enter Type of Stock Data (1-5):",
                            value = 1),
               checkboxInput("showgrid", label = "Show Grid", value = TRUE),
               numericInput("predict",
                            label = "Enter number of days to predict:",
                            value = 30),
             ),
             
             # Shows second plot from server file
             mainPanel(
               # need to use dygraphOutput because it is interactive and from a special package
               dygraphOutput("dygraph"))
           )
  )
)
)