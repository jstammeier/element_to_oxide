library(shiny)
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson4/

# Define UI ----
ui <- fluidPage(
  titlePanel("Element to Oxide"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select the file that you would like to convert"),
      
      fileInput("file", h3("File input")),
      
      numericInput("num", 
                   h3("Row containing units"), 
                   value = 1)
    ),
    
    mainPanel()  
  # fluidRow(
  #     
  #     column(4,
  #            fileInput("file", h3("File input"))),
  #     
  #     column(4, 
  #            h3("Help text"),
  #            helpText("Note: help text isn't a true widget,", 
  #                     "but it provides an easy way to add text to",
  #                     "accompany other widgets.")),
  #     
  #     column(2, 
  #            numericInput("num", 
  #                         h3("Numeric input"), 
  #                         value = 1))   
    ),
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
