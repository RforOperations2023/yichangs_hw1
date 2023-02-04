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
                             selected = "X2000")
                 
               ),
               
               # Output: Show barplot --------------------------------------
               mainPanel(
                 plotOutput(outputId = "barchart")
               )
             )
    )
  )
)

# Define server function required ---------
server <- function(input, output) {
  
  # Create barplot object the plotOutput function is expecting --
  output$barchart <- renderPlot({
    ggplot(data = df, aes_string(x = "country", y = input$y)) + geom_bar(stat = "identity") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      labs(title=paste0("Population of ", input$y))
  })
  
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)