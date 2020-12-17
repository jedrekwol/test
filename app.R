library(shiny)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

#setwd("C:/Users/Jedrek/Documents/GitHub/imiona/names/apka")


db <- iris
db_unique <- as.vector(unique(db$Species) )





ui <- fluidPage(shinyUI(fluidPage(
  # setBackgroundColor(
  #   color = "yellow",
  #   gradient = c("linear", "radial"),
  #   direction = c("bottom", "top", "right", "left"),
  #   shinydashboard = FALSE
  # ),
  headerPanel("Popularnosc imion w Polsce"),
  tabsetPanel(
    tabPanel("Table", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year1", "Wybierz rok",
                             min = 2000, max = 2019, value = 2005, sep = "" )
               ),
               mainPanel(
                 uiOutput("output1"),
                 tableOutput("table")
               ) 
             )
    ),
    tabPanel("Plot", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 multiInput(
                   inputId = "names", label = "Wybierz imie:",
                   choices = db_unique,
                   selected = "Jakub", width = "350px"
                 )
               ),
               mainPanel(
                 uiOutput("test"),
                 plotOutput("plot")
               )
             )
    )
  ),
  
)))

server <- shinyServer(function(input, output) {
  
  output$table <- renderTable({
    
    head(db, 10)
    
    top10_s <- db %>%
      select(Species, Sepal.Length) %>%
      filter(Species == "setosa") %>%
      arrange(desc(Sepal.Length))
 
    top10_s <- top10_s[1:10,]
    top10_s$Sepal.Length <- as.character(top10_s$Sepal.Length)
    
    top10_v <- db %>%
      select(Species, Sepal.Length) %>%
      filter(Species == "virginica") %>%
      arrange(desc(Sepal.Length))
    
    top10_v <- top10_v[1:10,]
    top10_v$Sepal.Length <- as.character(top10_v$Sepal.Length)
    

     
    top10 <- cbind(1:10, top10_v, top10_s)
    names(top10)[1] <- ""
     top10
    
  })
  
  output$test <- renderUI({
    if (length(input$names) == 0) {h2(print("Wybierz przynajmniej jedno imie"))}
  })
  
  output$plot <- renderPlot ({
    
    
    
    ggplot(db[db$Species == input$names,], aes  (x = Sepal.Length, y = Sepal.Width, group = Species)) +
      geom_point(aes(color =Species), size = 0.5) +
      theme_classic() +
      # ylim(0, max(imiona_lata$Count)) +
      theme(legend.position="bottom")
    
  })
  
  output$output1 <- renderUI({
    h2(paste('Najpopularniejsze imiona w Polsce w ', input$year1))
  })
  
  
})


shinyApp(ui, server)