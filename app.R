library(shiny)
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson4/

source("Element_to_oxide_Äpp.R")

ui <- fluidPage(
  titlePanel("Element to Oxide Conversion"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", h4("File input")
                , accept = ".txt"),
      helpText("Select a tab-separated .txt file that you would like to convert"),
      
      selectInput("conversion_type",
                  label = "Choose your fighter",
                  choices = c("classic", "manual"),
                  selected = "classic"
      ),
      
      helpText("classic: converts percent range to oxides and ppm range to elements"),
               
      helpText("manual: choose elements individually"),
      
      
      selectInput("selected_minor",
                  label = "as element",
                  choices = c(),
                  multiple = T
      ),

      selectInput("selected_major", 
                  label = "as oxide",
                  choices = c(),
                  multiple = T
      ),

    ),
    
    mainPanel(
#       textOutput("selected_var"),

#       tags$script(HTML(
#         "console.log(input.upload);"
#         "console.log(input);"
#         "console.log(output.asdf);"
#         )),

#       conditionalPanel(condition = "input.var != null && input.var == 'manual'",
#       conditionalPanel(condition = "input.upload != null && input.upload.name != null",
                       
#       ),
        h4(helpText("Uploaded table")),
      tableOutput("original"),
      
        h4(helpText("Converted table")),
      tableOutput("converted"),
      
      downloadButton("download"),
      
      textOutput("testing")
    )  
  ),
)

# Define server logic ----
server <- function(input, output, session) {

    generate_output = reactive({
    req(input$upload)
#     req(input$selected_minor)

#     tmp = get_major_minor_f(input$upload$datapath)
#     major_df = tmp[[1]]
#     minor_df = tmp[[2]]

#     selected_major = major_df[ names(major_df) %in% input$var]
#     selected_minor = minor_df[ names(minor_df) %in% input$var]

    tbl = read.table(file = input$upload$name
                                ,sep = "\t"
                                ,header = T
                                ,nrows = 1 
    )

    selected_major_df = tbl[ names(tbl) %in% input$selected_major]
    selected_minor_df = tbl[ names(tbl) %in% input$selected_minor]

    print('------------------------------')
    print(selected_major_df)
    print(selected_minor_df)
    print('------------------------------')

    ext <- tools::file_ext(input$upload$name)
#     validate(need(ext == "txt", "Please upload a txt file"))
#     switch(ext,
#       csv = vroom::vroom(input$upload$datapath, delim = ","),
#       tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
#     )
    ret = convert_f(input$upload$datapath, selected_major_df, selected_minor_df)
  })
  
#   output$selected_var <- renderText({ 
#     paste("You have selected", input$var)
#   })

  output$download <- downloadHandler(
    filename = function() {
      "quak.csv"
    },
    content = function(file) {
      write.csv(generate_output(), file)
    }
  )

  output$original <- renderTable({
    req(input$upload)
    tbl = read.table(file = input$upload$name,
                     sep = "\t",
                     header = T)
    head(tbl, 5)
  })

  output$converted <- renderTable({
    head(generate_output(), 5)
    # generate_output()
  })


  output$testing <- renderText({"Patzhalter"})
  
  
  
  
  observe({
    req(input$upload)
    tmp = get_major_minor_f(input$upload$datapath)
    classic_major_df = tmp[[1]]
    classic_minor_df = tmp[[2]]
    convertible_columns = c(names(classic_minor_df) , names(classic_major_df))
    updateSelectInput(session, 'selected_minor', choices= convertible_columns)
  })

  observe({
    req(input$upload)
    tmp = get_major_minor_f(input$upload$datapath)
    classic_major_df = tmp[[1]]
    classic_minor_df = tmp[[2]]
    convertible_columns = c(names(classic_minor_df) , names(classic_major_df))
    updateSelectInput(session, 'selected_major', choices= convertible_columns)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
