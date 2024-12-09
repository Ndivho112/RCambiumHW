# Load required libraries for the application
library(shiny)
library(shinythemes)
devtools::load_all()
library(CAMBIUM)
library(DT)
library(shinyjs)

# Custom CSS for Styling
custom_css <- "
  body {
    background-color: #f0f8ff;
    font-family: Arial, sans-serif;
  }
  .nav-tabs {
    background-color: #009688;
  }
  .nav-tabs > li > a {
    color: white;
    padding: 15px;
  }
  .nav-tabs > li > a:hover {
    background-color: #00796b;
  }
  .btn-primary {
    background-color: #ff5722;
    border-color: #ff5722;
  }
  .btn-primary:hover {
    background-color: #e64a19;
    border-color: #e64a19;
  }
  h2, h4 {
    color: #004d40;
  }
  .dataTables_wrapper {
    margin: 20px;
  }
  .shiny-input-container {
    margin-bottom: 15px;
  }
  .plot-container {
    margin-top: 20px;
  }
  .sidebar {
    background-color: #009688;
    color: #fff;
  }
  .sidebar h3, .sidebar h4, .sidebar label {
    color: #fff;
  }
  .main-panel {
    background-color: #ffffff;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0px 0px 15px rgba(0, 0, 0, 0.1);
  }
  .footer {
    text-align: center;
    padding: 20px;
    font-size: 12px;
    color: #666;
    background-color: #f0f0f0;
  }
  .loading-spinner {
    display: none;
    text-align: center;
  }
  .loading-spinner img {
    width: 50px;
  }
"

# Define the UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  useShinyjs(),  # For showing/hiding loading spinner
  tags$head(
    tags$style(HTML(custom_css))  # Include custom CSS
  ),

  # Define the navigation bar with different tabs
  navbarPage(
    "CAMBIUM Model Interface",

    # Home Tab
    tabPanel("Home",
             fluidRow(
               column(12,
                      div(style = "text-align: center; padding: 50px;",
                          h2("Welcome to the CAMBIUM Model Shiny App"),
                          h4("Process-Based Model of Daily Xylem Development in Eucalyptus"),
                          p("This application allows you to simulate daily xylem development in Eucalyptus trees using the CAMBIUM model. Customize inputs, run simulations, and visualize results with ease."),
                          br(),
                          h4("Key Features:"),
                          tags$ul(
                            tags$li("Upload custom forcing and parameter data"),
                            tags$li("Adjust simulation settings"),
                            tags$li("View and edit model parameters")
                          ),
                          br(),
                          h4("Use the navigation bar above to get started!")
                      )
               )
             )
    ),

    # Run Model Tab
    tabPanel("Run Model",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 h3("Input Data"),
                 fileInput("forcing_data", "Upload Forcing Data (CSV)", accept = c(".csv")),
                 fileInput("parameters_data", "Upload Parameters Data (CSV)", accept = c(".csv")),
                 h3("Settings"),
                 actionButton("run_model", "Run CAMBIUM Model", class = "btn btn-primary")
               ),

               mainPanel(
                 class = "main-panel",
                 h4("Model Status"),
                 textOutput("run_status"),  # Display the model status (run duration)
                 div(class = "loading-spinner", id = "loading_spinner",
                     img(src = "https://www.wpfaster.org/wp-content/uploads/2013/06/loading-gif.gif"),
                     p("Running... Please wait.")
                 ),
                 br(),
                 h4("Model Output"),
                 textOutput("model_output")  # Display success or error message
               )
             )
    ),

    # Parameters & Instructions Tab
    tabPanel("Parameters & Instructions",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 h4("Model Parameters")
               ),

               mainPanel(
                 class = "main-panel",
                 h4("Parameters Table"),
                 DTOutput("params_table"),  # Table for model parameters
                 br(),
                 h4("Instructions Table"),
                 DTOutput("instructions_table")  # Table for instructions
               )
             )
    ),

    # About Tab
    tabPanel("About",
             fluidRow(
               column(12,
                      div(style = "text-align: center; padding: 50px;",
                          h3("About CAMBIUM"),
                          p("The CAMBIUM model simulates daily xylem development in Eucalyptus trees. This app provides an interface for running the model with user-provided data."),
                          br(),
                          h4("References"),
                          p("Drew, D. M., Downes, G. M., & Battaglia, M. (2010). CAMBIUM, a process-based model of daily xylem development in Eucalyptus. Journal of Theoretical Biology, 264(2), 395–406. https://doi.org/10.1016/j.jtbi.2010.02.013"),
                          br(),
                          h4("Created by Nemutshili Ndivho Confidence")
                      )
               )
             )
    )
  ),

  # Footer
  tags$footer(class = "footer",
              "© 2024 Nemutshili Ndivho Confidence. All rights reserved."
  )
)

# Server Logic
server <- function(input, output, session) {
  # ---- Reactive Values ----
  model_results <- reactiveVal(NULL)

  # ---- Render Data Tables ----
  output$params_table <- renderDT({
    data(params)  # Ensure 'params' is a valid data frame
    datatable(params, options = list(pageLength = 10, autoWidth = TRUE))
  })

  output$instructions_table <- renderDT({
    data(runsettings)  # Ensure 'runsettings' is a valid data frame
    datatable(runsettings, options = list(pageLength = 10, autoWidth = TRUE))
  })

  # ---- Event Handlers ----
  observeEvent(input$run_model, {
    req(input$forcing_data, input$parameters_data)  # Ensure files are uploaded

    # Show loading spinner
    shinyjs::show("loading_spinner")

    start_time <- Sys.time()

    # Read the uploaded files
    forcing_data_path <- input$forcing_data$datapath
    parameters_data_path <- input$parameters_data$datapath

    # Run the CAMBIUM model
    output$run_status <- renderText("Running CAMBIUM Model...")

    tryCatch({
      # Assuming 'run_CAMBIUM' is a function that runs the model
      results <- run_CAMBIUM(
        filename_forcing = forcing_data_path,
        filename_param = parameters_data_path,
        run_settings = runsettings
      )

      model_results(results)

      # Get the duration in minutes
      run_duration <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      output$run_status <- renderText(paste("CAMBIUM run duration was:", round(run_duration, 2), "minutes."))

      # Display success message after the model run
      output$model_output <- renderText({
        "Model ran successfully!"
      })

      shinyjs::hide("loading_spinner")

    }, error = function(e) {
      output$model_output <- renderText({
        paste("Error in running CAMBIUM model:", e$message)
      })
      shinyjs::hide("loading_spinner")
    })
  })
}

# Run the Application
shinyApp(ui = ui, server = server)
