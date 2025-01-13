library(shiny)
library(shiny.fluent)
library(pool)
library(DBI)
library(duckdb)

ui <- fluentPage(
  Pivot(
    PivotItem(
      headerText = "User Info",
      alwaysRender = TRUE,  # <-- IMPORTANT: forces this PivotItem to be mounted immediately
      Stack(
        tokens = list(childrenGap = 10),
        Text(variant = "large", "User data:"),
        TextField.shinyInput("user_name", value = "", label = "Name"),
        TextField.shinyInput("user_surname", value = "", label = "Surname"),
        TextField.shinyInput("user_email", value = "", label = "Email")
      )
    ),
    PivotItem(
      headerText = "Selection",
      alwaysRender = TRUE,  # <-- IMPORTANT: forces this PivotItem to be mounted immediately
      Stack(
        tokens = list(childrenGap = 10),
        Text(variant = "large", children = "Select a fruit:"),
        Dropdown.shinyInput(
          "fruit_select",
          options = list(),  # Will be filled in the server
          label = "Available fruits"
        )
      )
    ),
    PivotItem(
      headerText = "Automatic_Result",
      alwaysRender = TRUE,  # <-- IMPORTANT: forces this PivotItem to be mounted immediately
      Stack(
        tokens = list(childrenGap = 10),
        Text(variant = "medium", "User Info:"),
        verbatimTextOutput("selected_user_info_auto"),

        Text(variant = "medium", "Selected Fruit:"),
        verbatimTextOutput("selected_fruit_auto")
      )
    ),
    PivotItem(
      headerText = "Manual_Result",
      alwaysRender = TRUE,  # <-- IMPORTANT: forces this PivotItem to be mounted immediately
      Stack(
        tokens = list(childrenGap = 10),
        Text(variant = "medium", "User Info:"),
        verbatimTextOutput("selected_user_info_manual"),

        Text(variant = "medium", "Selected Fruit:"),
        verbatimTextOutput("selected_fruit_manual"),

        br(),
        PrimaryButton.shinyInput(inputId = "save", text = "Save")
      )
    )
  )
)

server <- function(input, output, session) {

  # In-memory DuckDB connection
  pool <- dbPool(
    drv = duckdb(),
    dbdir = ":memory:"
  )

  # Create example tables
  dbExecute(pool, "CREATE TABLE frutas (id VARCHAR, nombre VARCHAR)")
  dbExecute(pool, "INSERT INTO frutas VALUES ('apple', 'APPLE')")
  dbExecute(pool, "INSERT INTO frutas VALUES ('banana', 'BANANA')")
  dbExecute(pool, "INSERT INTO frutas VALUES ('orange', 'ORANGE')")
  dbExecute(pool, "INSERT INTO frutas VALUES ('grape', 'GRAPE')")

  dbExecute(pool, "CREATE TABLE user (name VARCHAR, surname VARCHAR, email VARCHAR)")
  dbExecute(pool, "INSERT INTO user VALUES ('John', 'Doe', 'john.doe@example.com')")

  # At startup, read from user table (first row) and fill the text fields
  observe({
    user_info <- dbGetQuery(pool, "SELECT name, surname, email FROM user LIMIT 1")
    if (nrow(user_info) == 1) {
      updateTextField.shinyInput(session, "user_name",    value = user_info$name[1])
      updateTextField.shinyInput(session, "user_surname", value = user_info$surname[1])
      updateTextField.shinyInput(session, "user_email",   value = user_info$email[1])
    }
  })

  # Fruit options
  get_fruit_options <- reactive({
    df <- dbGetQuery(pool, "SELECT id as key, nombre as text FROM frutas")
    # Generate a list in the format: list(list(key=..., text=...), ...)
    lapply(seq_len(nrow(df)), function(i) {
      list(key = df$key[i], text = df$text[i])
    })
  })

  # selected_fruit: current dropdown value
  selected_fruit <- reactiveVal(NULL)

  # Initialize and update the dropdown
  observe({
    opts <- get_fruit_options()
    if (length(opts) > 0 && is.null(selected_fruit())) {
      selected_fruit(opts[[1]]$key)  # Select the first fruit by default
    }
    updateDropdown.shinyInput(
      session,
      "fruit_select",
      options = opts,
      value = selected_fruit()
    )
  })

  # When the user changes the fruit in the dropdown, update selected_fruit()
  observeEvent(input$fruit_select, {
    selected_fruit(input$fruit_select)
  })

  #========================
  # 1) AUTOMATIC RESULT
  #========================
  output$selected_user_info_auto <- renderText({
    # Immediately show what is in the text fields
    paste0(
      "Name: ",    req(input$user_name),    "\n",
      "Surname: ", req(input$user_surname), "\n",
      "Email: ",   req(input$user_email)
    )
  })

  output$selected_fruit_auto <- renderText({
    req(selected_fruit())  # Wait for a selected fruit
    # Convert key to text
    opts <- get_fruit_options()
    idx <- which(vapply(opts, function(x) x$key == selected_fruit(), logical(1)))
    if (length(idx) == 1) {
      opts[[idx]]$text
    } else {
      "Fruit not found"
    }
  })

  #========================
  # 2) MANUAL RESULT (with "Save" button)
  #========================

  # Store the values when the "Save" button is clicked
  manual_data <- reactiveVal(NULL)

  observeEvent(input$save, {
    # When the button is clicked, store the current values in manual_data
    fruit_key <- selected_fruit()
    # Find the text for the selected fruit
    opts <- get_fruit_options()
    idx <- which(vapply(opts, function(x) x$key == fruit_key, logical(1)))
    fruit_text <- if (length(idx) == 1) opts[[idx]]$text else "Fruit not found"

    # Save everything in the reactiveVal
    manual_data(list(
      name    = input$user_name,
      surname = input$user_surname,
      email   = input$user_email,
      fruit   = fruit_text
    ))
  })

  # Outputs that depend on manual_data
  output$selected_user_info_manual <- renderText({
    # If manual_data() is NULL, no data is shown (the button was not clicked yet)
    req(manual_data())
    paste0(
      "Name: ",    manual_data()$name,    "\n",
      "Surname: ", manual_data()$surname, "\n",
      "Email: ",   manual_data()$email
    )
  })

  output$selected_fruit_manual <- renderText({
    req(manual_data())
    manual_data()$fruit
  })

  onStop(function() {
    poolClose(pool)
  })
}

shinyApp(ui, server)
