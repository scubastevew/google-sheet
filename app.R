library(shiny)
library(bslib)
library(googlesheets4)
library(DT)
library(dplyr)

ui <- page_sidebar(
  title = "Google Sheets Data Editor",
  
  sidebar = sidebar(
    width = 300,
    
    card(
      card_header("Authentication & Connection"),
      
      actionButton(
        "authenticate",
        "Authenticate with Google",
        class = "btn-warning",
        width = "100%"
      ),
      
      br(), br(),
      
      textOutput("auth_status"),
      
      br(),
      
      textInput(
        "sheet_url",
        "Google Sheets URL:",
        value = "",
        placeholder = "Enter your Google Sheets URL..."
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
      
      conditionalPanel(
        condition = "output.data_loaded",
        h5("Edit Options:"),
        actionButton(
          "save_changes",
          "Save Changes to Sheet",
          class = "btn-success",
          width = "100%"
        ),
        
        br(), br(),
        
        numericInput(
          "add_rows",
          "Add empty rows:",
          value = 1,
          min = 1,
          max = 100
        ),
        
        actionButton(
          "add_rows_btn",
          "Add Rows",
          class = "btn-info",
          width = "100%"
        )
      )
    )
  ),
  
  # Main content area
  navset_card_tab(
    nav_panel(
      "Data Editor",
      div(
        style = "margin-bottom: 10px;",
        conditionalPanel(
          condition = "output.data_loaded",
          p("Double-click cells to edit. Changes are highlighted but not saved until you click 'Save Changes'.",
            style = "color: #666; font-style: italic;")
        )
      ),
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
  # Reactive values
  sheet_data <- reactiveVal(NULL)
  auth_status <- reactiveVal("Not authenticated")
  
  # Authentication
  observeEvent(input$authenticate, {
    tryCatch({
      # Set up authentication with user interaction
      gs4_auth(
        cache = ".secrets",
        use_oob = TRUE
      )
      auth_status("Authenticated successfully!")
      
      showNotification(
        "Successfully authenticated with Google!",
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      auth_status("Authentication failed")
      showNotification(
        paste("Authentication error:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Display authentication status
  output$auth_status <- renderText({
    auth_status()
  })
  
  # Load worksheet names when URL is provided
  observeEvent(input$sheet_url, {
    req(input$sheet_url)
    req(auth_status() == "Authenticated successfully!")
    
    tryCatch({
      # Get worksheet names
      sheet_names <- sheet_names(input$sheet_url)
      
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
    req(auth_status() == "Authenticated successfully!")
    
    # Show loading notification
    id <- showNotification(
      "Loading data from Google Sheets...",
      type = "default",
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
        type = "message",
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
  
  # Render editable data table
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
      rownames = FALSE,
      editable = TRUE  # Enable editing
    )
  })
  
  # Handle cell edits
  observeEvent(input$data_table_cell_edit, {
    req(sheet_data())
    
    info <- input$data_table_cell_edit
    data <- sheet_data()
    
    # Update the data
    data[info$row, info$col] <- info$value
    sheet_data(data)
    
    showNotification(
      "Cell updated. Click 'Save Changes' to write to Google Sheets.",
      type = "default",
      duration = 2
    )
  })
  
  # Save changes to Google Sheets
  observeEvent(input$save_changes, {
    req(sheet_data(), input$sheet_url, input$worksheet)
    req(auth_status() == "Authenticated successfully!")
    
    id <- showNotification(
      "Saving changes to Google Sheets...",
      type = "default",
      duration = NULL
    )
    
    tryCatch({
      # Write data back to Google Sheets
      write_sheet(
        sheet_data(),
        ss = input$sheet_url,
        sheet = input$worksheet
      )
      
      removeNotification(id)
      showNotification(
        "Changes saved successfully to Google Sheets!",
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      removeNotification(id)
      showNotification(
        paste("Error saving changes:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Add empty rows
  observeEvent(input$add_rows_btn, {
    req(sheet_data(), input$add_rows)
    
    data <- sheet_data()
    new_rows <- matrix(NA, nrow = input$add_rows, ncol = ncol(data))
    colnames(new_rows) <- colnames(data)
    new_rows <- as.data.frame(new_rows)
    
    # Add new rows to data
    updated_data <- rbind(data, new_rows)
    sheet_data(updated_data)
    
    showNotification(
      paste("Added", input$add_rows, "empty rows. Don't forget to save changes."),
      type = "default",
      duration = 3
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
