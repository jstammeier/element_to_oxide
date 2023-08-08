library(shiny)
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson4/

source("Element_to_oxide_Äpp.R")

ui <- fluidPage(
  titlePanel("Element to Oxide"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select the file that you would like to convert"),
      
      fileInput("file", h3("File input")),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("asdf", "jkl"),
                  selected = "asdf"),
    ),
    
    mainPanel(
      textOutput("selected_var"),
      downloadButton("Download"),
      tableOutput("contents")
    )  
  ),
)

# Define server logic ----
server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })

#   output$download <- downloadHandler(
#     filename = function() {
#       paste0(input$dataset, ".csv")
#     },
#     content = function(file) {
#       write.csv(data(), file)
#     }
#   )
    
  output$contents <- renderTable({
    file <- input$file
    ext <- tools::file_ext(file$datapath)
#     validate(need(ext == "txt", "Please upload a txt file"))
    req(file)
    ret = convert_f(file$datapath)
    return(ret)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
