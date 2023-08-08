library(shiny)
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson4/

#Source scripts, load libraries, and read data sets 
#at the beginning of app.R outside of the server function.


# Define UI ----
ui <- fluidPage(
  titlePanel("Element to Oxide"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select the file that you would like to convert"),
      
      #fileInput("file", h3("File input")),
      
      # numericInput("num", 
      #              h3("Row containing units"), 
      #              value = 1)
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent White", 
                              "Percent Blue",
                              "Percent Green", 
                              "Percent Red"),
                  selected = "Percent White"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(
      # helpText("Mainpanel"),
      textOutput("selected_var"),
      textOutput("second")
    )  
    
  ),
)

# Define server logic ----
server <- function(input, output) {
  
  # Define user specific objects inside 
  # server function, but outside of any render* calls. 
  # These would be objects that you think each user 
  # will need their own personal copy of. 
  # For example, an object that records the user’s session information. 
  # This code will be run once per user
  
  output$selected_var <- renderText({ 
    
    # Only place code that Shiny must rerun to build an object 
    # inside of a render* function. 
    # Shiny will rerun all of the code in a render* chunk 
    # each time a user changes a widget mentioned in the chunk. 
    # This can be quite often.
    
    paste("You have selected", input$var)
  })
  
  output$second <- renderText({ 
    paste("You have chosen a range that goes from",
          input$range[1], "to", input$range[2])
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
