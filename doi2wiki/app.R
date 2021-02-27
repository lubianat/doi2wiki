library(shiny)
source("utils.r")

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            h3("CSV must have a column named DOI"),
            fileInput("file1", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            
            radioButtons(
                inputId = "dropna",
                label = "Drop NAs?",
                choices = c("Sure", "Nah"),
                selected = "Sure"),
            downloadButton("download", "Download .tsv")
        ),
        
        mainPanel(
            tableOutput("preview")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        
        df <- read.csv(inFile$datapath, header = input$header)
        data <- reconcile_df_to_wikidata(df)
        
        remove_na <- input$dropna
        if (remove_na == "Sure"){
            data <- data[!is.na(data$item),] 
            return(data)
        } else {
            return(data)
        }
    
    })
    
    output$preview <- renderTable({
        head(data())
    })
    
    output$download <- downloadHandler(
        filename = function() {
            paste0("result.tsv")
        },
        content = function(file) {
            vroom::vroom_write(data(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
