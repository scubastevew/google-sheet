library(shiny)
library(bslib)
library(googlesheets4)
library(DT)
library(dplyr)

# Disable authentication prompts for public sheets
# For private sheets, you'll need to set up authentication
# gs4_deauth()

ui <- page_sidebar(
  title = "Google Sheets Data Viewer",
  
  sidebar = sidebar(
    width = 300,
    
    card(
      card_header("Google Sheets Connection"),
      textInput(
        "sheet_url",
        "Google Sheets URL:",
        value = "https://docs.google.com/spreadsheets/d/1U6Cf_qEOhiR9AZqTqS3mbMF3zt2db48ZP5v3rkrAEJY/edit#gid=0",
        placeholder = "Enter Google Sheets URL..."
      ),
      
      selectInput(
        "worksheet",
        "Select Worksheet:",
        choices = NULL
      ),
      
      actionButton(
        "load_data",
        "Load Data",
        class = "btn-primary",
        width = "100%"
      ),
      
      br(), br(),
      
      conditionally_render(
        condition = "output.data_loaded",
        checkboxInput(
          "show_summary",
          "Show Data Summary",
          value = TRUE
        )
      )
    )
  ),
  
  # Main content area
  navset_card_tab(
    nav_panel(
      "Data Table",
      DTOutput("data_table")
    ),
    
    nav_panel(
      "Data Summary",
      verbatimTextOutput("data_summary")
    ),
    
    nav_panel(
      "Column Info",
      DTOutput("column_info")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the loaded data
  sheet_data <- reactiveVal(NULL)
  
  # Load worksheet names when URL is provided
  observeEvent(input$sheet_url, {
    req(input$sheet_url)
    
    tryCatch({
      # Extract sheet ID from URL or use direct ID
      if (grepl("docs.google.com", input$sheet_url)) {
        sheet_id <- input$sheet_url
      } else {
        sheet_id <- input$sheet_url
      }
      
      # Get worksheet names
      sheet_names <- sheet_names(sheet_id)
      
      updateSelectInput(
        session,
        "worksheet",
        choices = setNames(sheet_names, sheet_names),
        selected = sheet_names[1]
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error accessing sheet:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Load data when button is clicked
  observeEvent(input$load_data, {
    req(input$sheet_url, input$worksheet)
    
    # Show loading notification
    id <- showNotification(
      "Loading data from Google Sheets...",
      type = "message",
      duration = NULL
    )
    
    tryCatch({
      # Read data from Google Sheets
      data <- read_sheet(
        input$sheet_url,
        sheet = input$worksheet
      )
      
      # Store the data
      sheet_data(data)
      
      # Remove loading notification and show success
      removeNotification(id)
      showNotification(
        paste("Successfully loaded", nrow(data), "rows and", ncol(data), "columns"),
        type = "success",
        duration = 3
      )
      
    }, error = function(e) {
      removeNotification(id)
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Render data table
  output$data_table <- renderDT({
    req(sheet_data())
    
    datatable(
      sheet_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100)
      ),
      filter = "top",
      rownames = FALSE
    )
  })
  
  # Render data summary
  output$data_summary <- renderPrint({
    req(sheet_data())
    summary(sheet_data())
  })
  
  # Render column information
  output$column_info <- renderDT({
    req(sheet_data())
    
    data <- sheet_data()
    
    col_info <- data.frame(
      Column = names(data),
      Type = sapply(data, class),
      `Missing Values` = sapply(data, function(x) sum(is.na(x))),
      `Unique Values` = sapply(data, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    
    datatable(
      col_info,
      options = list(
        pageLength = 15,
        dom = 't'
      ),
      rownames = FALSE
    )
  })
  
  # Output for conditional rendering
  output$data_loaded <- reactive({
    !is.null(sheet_data())
  })
  
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
