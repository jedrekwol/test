library(shiny)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

#setwd("C:/Users/Jedrek/Documents/GitHub/imiona/names/apka")

imiona <- readxl::read_excel("imiona2.xlsx")
imiona_unique <- imiona %>%
  select(Name, Count) %>%
  group_by(Name) %>%
  summarise(Count = sum(Count)) %>%
  arrange(desc(Count))






ui <- fluidPage(shinyUI(fluidPage(
  # setBackgroundColor(
  #   color = "yellow",
  #   gradient = c("linear", "radial"),
  #   direction = c("bottom", "top", "right", "left"),
  #   shinydashboard = FALSE
  # ),
  headerPanel("Popularnosc imion w Polsce"),
  tabsetPanel(
    tabPanel("Tabelka", fluid = TRUE,
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
    tabPanel("Fajny wykres", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 multiInput(
                   inputId = "names", label = "Wybierz imie:",
                   choices = imiona_unique$Name,
                   selected = "Jakub", width = "350px"
                 )
               ),
               mainPanel(
                 uiOutput("test"),
                 plotlyOutput("plot")
               )
             )
    )
  ),

)))

server <- shinyServer(function(input, output) {
  
  output$table <- renderTable({
    
    #to mozna zrobic jako funkcje
    
    top10_m <- imiona %>%
      filter(Year == input$year1, Sex == "M") %>%
      select(Name, Count)# 
    
    top10_m <- top10_m[1:10,]
    top10_m$Count <- as.character(top10_m$Count)
    top10_m
    
    top10_k <- imiona %>%

      filter(Year == input$year1, Sex == "K") %>%
      select(Name, Count)

    top10_k <- top10_k[1:10,]
    top10_k$Count <- as.character(top10_k$Count)

    top10 <- cbind(1:10, top10_m, top10_k)
    names(top10)[1] <- ""
    top10
  })
  
  output$test <- renderUI({
    if (length(input$names) == 0) {h2(print("Wybierz przynajmniej jedno imie"))}
  })
  
  output$plot <- renderPlotly ({
    
    imionka <- input$names
    
    imiona_lata <- imiona %>%
      filter(Name %in% imionka) %>%
      select(Year, Name, Count, Sex) %>%
      group_by(Name, Sex) %>%
      complete(Year = full_seq(2000:2019, 1)) 
    
    imiona_lata[is.na(imiona_lata)] <- 0
    
    p <- ggplot(imiona_lata, aes  (x = Year, y = Count, group = Name)) +
      geom_line(aes(color =Name), size = 0.5) +
      theme_classic() +
      ylim(0, max(imiona_lata$Count)) +
      theme(legend.position="bottom") 
    if (length(input$names) > 0) {ggplotly(p)}
    
    
  })
  
  output$output1 <- renderUI({
    h2(paste('Najpopularniejsze imiona w Polsce w ', input$year1))
  })
  
  
})


shinyApp(ui, server)