library(shiny)
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson4/

source("Element_to_oxide_Äpp.R")

ui <- fluidPage(
  titlePanel("Element to Oxide"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select the file that you would like to convert"),
      
      fileInput("upload", h3("File input")),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("asdf", "jkl"),
                  selected = "asdf"),
    ),
    
    mainPanel(
      textOutput("selected_var"),

      tags$script(HTML(
        "console.log(input.upload);"
        )),

#       conditionalPanel(condition = "input.upload.lenght > 0",
#       conditionalPanel(condition = "input.upload.name === null",
      conditionalPanel(condition = "input.upload",
                       downloadButton("download"),
      ),

      tableOutput("contents")
    )  
  ),
)

# Define server logic ----
server <- function(input, output) {

  generate_output = reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
#     validate(need(ext == "txt", "Please upload a txt file"))
#     switch(ext,
#       csv = vroom::vroom(input$upload$datapath, delim = ","),
#       tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
#     )
    ret = convert_f(input$upload$datapath)
  })
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })

  output$download <- downloadHandler(
    filename = function() {
      "quak.csv"
    },
    content = function(file) {
      write.csv(generate_output(), file)
    }
  )

  output$contents <- renderTable({
    head(generate_output(), 5)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
