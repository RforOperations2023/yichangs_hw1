#
# This is HW 1. You can run the application by clicking
# the 'Run App' button above.
#
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(ggalt)
library(DT)
library(hrbrthemes)
theme_set(theme_classic())

df <- read.csv("world_population.csv")


# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  navbarPage(
    "HW 1",
    tabPanel("Overall population", 
             # Sidebar layout with a input and output definitions --------------
             sidebarLayout(
               
               # Inputs: Select variables to plot ------------------------------
               sidebarPanel(
                 
                 # Select variable for y-axis ----------------------------------
                 selectInput(inputId = "y", 
                             label = "Year:",
                             choices = c("X1995", "X1996", "X1997", "X1998", "X1999", "X2000", "X2001", "X2002",
                                         "X2003", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010",
                                         "X2011", "X2012", "X2013"), 
                             selected = "X2000"),
                 
                 # Select variable for color -----------------------------------
                 selectInput(inputId = "z", 
                             label = "Color:",
                             choices = c("darkred", "darkgreen", "darkblue"),
                             selected = "darkblue")
                 
                 
               ),
               
               # Output: Show barplot --------------------------------------
               mainPanel(
                 plotOutput(outputId = "barchart")
               )
             )
    ),
    
    tabPanel("Population for each country", 
             # Sidebar layout with a input and output definitions --------------
             sidebarLayout(
               
               # Inputs: Select variables to plot ------------------------------
               sidebarPanel(
                 
                 # Select variable for c ----------------------------------
                 selectInput(inputId = "c", 
                             label = "Country:",
                             choices = c("Afghanistan","Argentina", "China", "India", "France", "Germany","Japan", "Mexico", "Philippines", "Portugal", "Republic of Korea",
                                         "Russian Federation", "Spain", "South Africa", "United Kingdom of Great Britain and Northern Ireland", "United States of America", "Zimbabwe", "Yemen",
                                         "Ukraine", "Nepal", "Malaysia", "Brazil"), 
                             selected = "Philippines")
               ),
               
               # Output: Show barplot --------------------------------------
               mainPanel(
                 plotOutput(outputId = "country_barchart")
               )
             )
    )
  )
)

# Define server function required ---------
server <- function(input, output) {
  
  # Create barplot object the plotOutput function is expecting --
  output$barchart <- renderPlot({
    ggplot(data = df, aes_string(x = "country", y = input$y)) + geom_bar(stat = "identity", fill=input$z) +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      labs(title=paste0("Population of ", input$y))
  })
  
  population_country <- reactive({
    new_table <- as.data.frame(t(dplyr::filter(df, country %in% c(input$c))[,-1]))
    colnames(new_table)[1] ="count"
    new_table$count_type <- ifelse(new_table$count < 10000000, "below", "above") 
    new_table$year <- row.names(new_table)
    new_table
  })
  
  output$country_barchart <- renderPlot({
    ggplot(data = population_country(), aes(x = year, y = count, label = count)) + 
      geom_bar(stat='identity', aes(fill=count_type), width=.5)  +
      scale_fill_manual(name="Count", 
                        labels = c("Above 80 Million", "Below 80 Millon"), 
                        values = c("above"="#00ba38", "below"="#f8766d")) + 
      labs(subtitle=paste0("Population of ", input$c), 
           title= "Diverging Bars") + 
      coord_flip()
  })
  
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)