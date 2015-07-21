library(dplyr)
library(ggvis)
library(shiny)
library(DT)

datos <- read.csv("data.csv")

rownames(datosQ) <- datos[,1]
datosQ$meanU <- format(datosQ$meanU, big.mark=",", scientific=FALSE, digits = 0)



shinyServer(function(input, output, session) {

  filterdata <- reactive({

    input$x_domain
    input$y_domain
 
    datos <- datos %>% filter(Service %in% input$service)
    datos <- datos %>% filter(Classification %in% input$classification)
    datos <- datos %>% filter(Region %in% input$region)
  })
  

  client_tag <- function(x) {
    
    if(is.null(x)) return (NULL)
    if(is.null(x$ID)) return(NULL)
    
    datos <- isolate(filterdata())
    filterdata <- datos[datos$ID == x$ID,]
    
    filterdata$meanR <- round(filterdata$meanR,digits = 2)
    filterdata$meanU <- round(filterdata$meanU,digits = 0)
    
    paste0("<b>", filterdata$Legal_Name, "</b><br>",
           paste("Product A: $", format(filterdata$meanR, big.mark=",", scientific = FALSE)), "<br>",
           paste("Product B: ", format(filterdata$meanU, big.mark=",", scientific = FALSE), " Units"),"<br>",
           paste("Projects: ", filterdata$N_projects),"<br>",
           paste("Classification: ", filterdata$Classification),"<br>",
           paste("Region: ", filterdata$Region)
  )}
  

  datos$groups <-factor(datos$groups, levels=c("Group1", "Group2", "Group3", "Group4"))
  .range <- c("green", "red", "orange", "purple")   
  
  
  gra <- reactive({
    
      filterdata %>% 
      ggvis(~meanR, ~meanU) %>%
      layer_points(fill = ~groups, opacity := 1, key:=~ID) %>%
      scale_ordinal("fill", range = .range) %>% 
      scale_numeric("x", domain = input$x_domain, nice = FALSE, clamp = TRUE, expand = 0) %>%
      scale_numeric("y", domain = input$y_domain, nice = FALSE, clamp = TRUE) %>%
      add_tooltip(client_tag, "hover") %>%
      add_legend("fill", title = "Group", values = c("Group1", "Group2", "Group3", "Group4")) %>%
      add_axis("x", title = "Average Product A (USD $)", title_offset = 50, 
               properties = axis_props(
                                      labels = list(
                                      fontSize = 12,
                                      align = "right"),
                                      title = list(fontSize = 12))) %>%
      add_axis("y", title = "Average Product B (Units)", title_offset = 80,
               properties = axis_props(
                                      labels = list(
                                      fontSize = 12,
                                      align = "right"),
                                      title = list(fontSize = 12))) %>%
      set_options(width = 750, height = 500)
  })
             
  gra %>% bind_shiny("graph-plot")


  
  output$x1 <- DT::renderDataTable({
    datatable(datosQ, rownames = TRUE,
                       class = 'cell-border stripe',
                       options = list(pageLength = 25, autoWidth = FALSE,
                                      columnDefs = list(list(className = 'dt-center', targets = c(1:4, 11:13)),
                                                        list(className = 'dt-right', targets = 5:10))),
                       extensions = c('TableTools','FixedColumns'), 
                       filter = "bottom",
                       selection = "multiple",
                       escape = FALSE,
                       colnames = c('Groups',
                                    '<span style="color:red">Red_Flag (Ranking)</span>',
                                    '<span style="color:blue">Probability</span>',
                                    'Mean Product A', 'Increment Product A', 'Net lost', 
                                    'Mean Product B', 'Increment Product B', 'Ratio', 
                                    'Service Used', 'Projects', 'Classification','Region')) %>% 
                       formatCurrency(c('meanR', 'Ratio', 'RDiffQ')) %>% 
                       formatPercentage('Probability', 0) %>%
                       formatPercentage(c('mean.incre.rev', 'mean.incre.url'), 2) %>%
                       formatStyle('Probability', background = styleColorBar(datosQ$Probability, 'steelblue'))
                       }, server = FALSE)
  
  
  # download the filtered data
  output$downloadData <- downloadHandler('name_for_table_data.csv', 
    content = function(file) {
    s = input$x1_rows_all
    write.csv(datosQ[s, , drop = FALSE], file)
  })
  
})