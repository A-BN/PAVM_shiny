library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(shiny)

read_sizes <- 
  read_tsv(file = "./read_sizes.dat", col_names = c("size", "read_name"))
read_sizes <-
  read_sizes %>%
  mutate(primer = ifelse(test = str_detect(string = read_name, pattern = "./[BNR]/"), 
                         yes = str_replace(string = read_name, pattern = "./([BNR])/.*", replacement = "\\1"), 
                         no = "other"))

ui <- fluidPage(
  titlePanel("PAVM read sizes"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput(inputId = "primer_choice", 
                             label = "Primer Selection", 
                             multiple = TRUE, 
                             selected = unique(read_sizes$primer), 
                             choices = unique(read_sizes$primer)
                 ),
                 checkboxInput(inputId = "log_tog", 
                               label = "Log10 scale", 
                               value = TRUE
                 ),
                 downloadButton(outputId = "downloadData", 
                                label = "Download"
                 )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", id = "los_tablos",
                  tabPanel("graph", verbatimTextOutput(outputId = "summa"),
                           plotOutput(outputId = "logbox"),
                           plotOutput(outputId = "histo")),
                  tabPanel("data", dataTableOutput(outputId = "read_df")
                  )
      )
    )
  )
)
server <- function(input, output){
  
  curr_read_sizes <- reactive({
    read_sizes %>%
      filter(primer %in% input$primer_choice) %>%
      mutate(size_plot = ifelse(rep(input$log_tog, n()), log10(size), size)) %>%
      identity()
  })
  
  output$read_df <-
    renderDataTable(expr = curr_read_sizes())
  output$summa <-
    renderPrint(expr = summary(curr_read_sizes()$size))
  
  output$logbox <- renderPlot(
    ggplot(data = curr_read_sizes())+
      geom_boxplot(mapping = aes(y = size_plot,
                                 x = primer))+
      ylab(ifelse(input$log_tog, "log10(size)", "size"))
  )
  output$histo <- renderPlot(
    ggplot(data = curr_read_sizes())+
      geom_freqpoly(mapping = aes(x = size_plot, color = primer), 
                    binwidth = function(x) max(x)/10)+
      xlab(ifelse(input$log_tog, "log10(size)", "size"))
    
  )
  
  output$downloadData <-
    downloadHandler(filename = "2018_shiny_PAVM.tsv", 
                    content = function(down_path) write_tsv(x = curr_read_sizes(), 
                                                            path = down_path))
}


shinyApp(ui = ui, server = server)

