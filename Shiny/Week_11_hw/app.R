#11-18-2024
# Hanalei Hoopai-Sylva
#  Week 11 HW

library(shiny)
library(tidyverse)
library(ggplot2)

head(diamonds) #look at diamonds dataset

ui <- fluidPage(
  titlePanel("Diamond Price vs Carat"), #title of the page
  
  sidebarLayout( #where to create and edit the side bar panel
    sidebarPanel(
      selectInput("cut", "Diamond Cut",  #use "cut" from diamonds data, title it to ʻDiamond Cutʻ on the output
                  choices = unique(diamonds$cut), #pick out the types cuts to be able to choose from
                  selected = "Premium") #start the selection at premium
    ),
    
    mainPanel( #create and edit the center panel
      plotOutput("scatterPlot"), #make a scatter plot
      
      # Table output for showing average price by cut
      tableOutput("avgPriceTable") #second type of output, find ave of prices
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({ #make ʻdataʻ be reactive
    diamonds %>% filter(cut == input$cut) # in the diamonds dataset, filter everything that falls under each cut (ex: if cut is ʻpremiumʻ filter all rows that deal with premium diamonds)
  })
  
  output$scatterPlot <- renderPlot({ #make a scatter plot 
    data <- data()  # filtered data based on the selected cut
    
    ggplot(data, aes(x = carat, y = price)) + 
      geom_point(aes(color = clarity), alpha = 0.5) +  # color by clarity
      labs(title = paste("Price vs Carat for", input$cut, "Cut"), #needed chatgpt, somehow this makes the reactive labels 
           x = "Carat", 
           y = "Price") +
      theme_minimal()
  })
  
  output$avgPriceTable <- renderTable({ #make a table of ave costs
    data <- data()
    avg_price <- data %>% #new ʻavg_priceʻ dataset
      summarise(Average_Price = mean(price))  # Calculate the average price for selected cut
    
    avg_price
  })
}

shinyApp(ui = ui, server = server)
