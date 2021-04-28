library(shiny)
source("utils.r")

ui <- fluidPage(
    
    
    windowTitle = "doi2wiki",
    title = tags$head(tags$link(
        rel = "icon",
        href = "https://raw.githubusercontent.com/lubianat/doi2wiki/master/doi2wiki/favicon-32x32.png",
        type = "image/x-icon"
    ), ),
    
    
    
    sidebarLayout(
        sidebarPanel(

            p("Paste DOIs, one per row (without prefix; 10.xxx...)"),
            textAreaInput(inputId = "dois", label="DOIs", height = 180),
            
            
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            
            radioButtons(
                inputId = "doipmid",
                label = "DOI or PMID?",
                choices = c("DOI", "PMID"),
                selected = "DOI"),
            
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
    
    
    
    qids <- reactive({
        dois <- input$dois
        dois = strsplit(dois, "\n",fixed = TRUE)
        
        type <- input$doipmid
        
        if (type == "DOI"){
            df <- data.frame(DOI=dois[[1]])
            } else{
            df <- data.frame(PMID=dois[[1]])
        }

        
        data <- reconcile_df_to_wikidata(df, id_type)
        
        remove_na <- input$dropna
        if (remove_na == "Sure"){
            data <- data[!is.na(data[,"QID"]),] 
            return(data)
        } else {
            return(data)
        }
        print(dois)
        
    })
    
    output$preview <- renderTable({
        head(qids())
    })
    
    output$download <- downloadHandler(
        filename = function() {
            paste0("result.tsv")
        },
        content = function(file) {
            vroom::vroom_write(qids(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
