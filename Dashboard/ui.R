library(dplyr)
library(ggvis)
library(shiny)
library(DT)


shinyUI(fluidPage(

  titlePanel(h2("Dashboard Sample -- Random data", align = "center")),
  br(),

  mainPanel(
    tabsetPanel(
      tabPanel("Graph",
        br(),
        fluidRow(
          column(4,
            wellPanel(
            sliderInput("x_domain", "Product A (USD $)", 0,  25000, c(0, 25000), step = 100),
            br(),
            sliderInput("y_domain", "Product B (Units)", 0, 250000, c(0, 250000), step = 1000),
            
            selectInput("service", "Service used", choices = c("Service1", "Service2"), 
              selected =  c("Service1", "Service2"), multiple = TRUE),
                      
            selectInput("region", "Region", choices = c("Region1", "Region2", "Region3"), 
              selected =  c("Region1", "Region2", "Region3"), multiple = TRUE),
                            
            selectInput("classification", "Classification", choices = c("Class1","Class2", "Class3"),
              selected =  c("Class1","Class2", "Class3"), multiple = TRUE))),
          column(8,
            br(),
            ggvisOutput("graph-plot"), align = "center"))),

      tabPanel("Table",
        fluidRow(
          column(12,
          dataTableOutput(outputId="x1")) #, width = "50%")
        ),
        br(),
        fluidRow(
          column(12,
         p(class = 'text-center', downloadButton('downloadData', 'Download Filtered Data')))
        )       
      )
    )
  )
))