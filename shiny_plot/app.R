library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(ggsci)
library(ggdist)
library(patchwork)
library(vroom)
library(readr)

# Define the list of models
model_list <- c(
  "PGS000010", "PGS000011", "PGS000012", "PGS000013", "PGS000018", "PGS000019", "PGS000057",
  "PGS000058", "PGS000059", "PGS000200", "PGS000296", "PGS000329", "PGS000337", "PGS000349",
  "PGS000710", "PGS000746", "PGS000747", "PGS000748", "PGS000749", "PGS000798", "PGS000818",
  "PGS000899", "PGS000962", "PGS001048", "PGS001314", "PGS001315", "PGS001316", "PGS001317",
  "PGS001355", "PGS001780", "PGS001839", "PGS002048", "PGS002244", "PGS002262", "PGS002775",
  "PGS002776", "PGS002777", "PGS002778", "PGS002809", "PGS003355", "PGS003356", "PGS003438",
  "PGS003446", "PGS003725", "PGS003726", "PGS003866", "PGS_LDP2Auto", "PGS_prscsx"
)

df_ntile_norm <- read.csv("CAD_ref_ntile.csv")

custom_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(family = "Arial", size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5) # Updated x-axis text settings
  )

# Custom CSS for JAMA-inspired styling with improved navbar readability
jama_css <- "
  body {
    background-color: #f8f8f8;
    color: #333333;
    font-family: Arial, sans-serif;
  }
  .container-fluid {
    max-width: 1200px;
    margin: 0 auto;
  }
  .well {
    background-color: #ffffff;
    border: 1px solid #dddddd;
    border-radius: 4px;
    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.05);
  }
  .btn-default {
    color: #ffffff;
    background-color: #0072B5;
    border-color: #0072B5;
  }
  .btn-default:hover {
    background-color: #005a8e;
    border-color: #005a8e;
  }
  .form-control {
    border-color: #dddddd;
  }
  .selectize-input {
    border-color: #dddddd;
  }
  .shiny-notification {
    background-color: #0072B5;
    color: #ffffff;
  }
  /* Updated styles for the header */
  .navbar {
    background-color: #B24745FF;
    border-color: #B24745FF;
  }
  .navbar-brand {
    color: #ffffff !important;
  }
  .navbar-nav > li > a {
    color: #ffffff !important;
    font-weight: bold;
  }
  .navbar-nav > li > a:hover,
  .navbar-nav > li > a:focus {
    background-color: #CC79A7 !important;
    color: #ffffff !important;
  }
  .navbar-nav > .active > a,
  .navbar-nav > .active > a:hover,
  .navbar-nav > .active > a:focus {
    color: #ffffff !important;
    background-color: #B24745FF !important;
  }
  .citation {
    background-color: #f8f8f8;
    border-left: 5px solid #0072B5;
    padding: 15px;
    margin: 20px 0;
    font-size: 0.9em;
    font-style: italic;
  }
  .citation strong {
    font-style: normal;
  }
"

# UI for Shiny App
ui <- fluidPage(
  tags$head(
    tags$style(HTML(jama_css))
  ),
  navbarPage(
    "CAD PRS Variability",
    tabPanel(
      "Plot",
      p("Plot ancestry-normalized CAD polygenic risk scores for a random selection of individuals from the 1000 Genomes + HGDP reference panel"),
      sidebarLayout(
        sidebarPanel(
          width = 3, # Narrower sidebar
          actionButton("plot_button", "Plot PGS Variability", class = "btn-block"),
          hr(),
          checkboxInput("show_advanced", "Show Advanced Options", FALSE),
          conditionalPanel(
            condition = "input.show_advanced == true",
            selectInput("selected_models",
              label = "Select CAD PGS",
              choices = model_list,
              selected = model_list,
              multiple = TRUE
            ),
            numericInput("seed_input",
              label = "Set Random Seed (leave empty for random)",
              value = NA
            ),
            sliderInput("sample_size_input",
              label = "Select number of individuals",
              min = 1,
              max = 10,
              value = 5,
              step = 1
            )
          )
        ),
        mainPanel(
          width = 9, # Wider main panel
          plotOutput("score_plot", height = "600px")
        )
      )
    ),
    tabPanel(
      "About",
      fluidRow(
        column(
          12,
          h2("About CAD PRS Variability"),
          p("This application visualizes the variability of Coronary Artery Disease (CAD) Polygenic Risk Scores (PRS) across different models and individuals."),
          h3("Key Features:"),
          tags$ul(
            tags$li("Visualize CAD PRS variability for randomly selected individuals from the 1000 Genomes + HGDP reference panel."),
            tags$li("Compare multiple PRS models side by side."),
            tags$li("Customize the number of individuals and specific PRS models to display."),
            tags$li("Set a specific random seed for reproducibility or generate new random selections.")
          ),
          h3("Data Source:"),
          p(
            "The PRS models used in this application are sourced from the ",
            tags$a(href = "https://www.pgscatalog.org/", "PGS Catalog"),
            "focusing on those related to Coronary Artery Disease."
          ),
          h3("How to Use:"),
          tags$ol(
            tags$li("Navigate to the 'Plot' tab."),
            tags$li("Click 'Plot PGS Variability' to generate a visualization."),
            tags$li("Use 'Advanced Options' to customize the plot:"),
            tags$ul(
              tags$li("Select specific CAD PGS models to include."),
              tags$li("Set a random seed for reproducibility."),
              tags$li("Adjust the number of individuals to display.")
            )
          ),
          h3("Manuscript Citation:"),
          div(
            class = "citation",
            p(strong("Population Performance and Individual Agreement of Coronary Artery Disease Polygenic Risk Scores")),
            p("Sarah A. Abramowitz, Kristin Boulier, Karl Keat, Katie M. Cardone, Manu Shivakumar, John DePaolo, Renae Judy, Dokyoon Kim, Daniel J. Rader, Ritchie, Benjamin F. Voight, Bogdan Pasaniuc, Michael G. Levin, Scott M. Damrauer"),
            p("doi: ", a(href = "https://doi.org/10.1101/2024.07.25.24310931", "https://doi.org/10.1101/2024.07.25.24310931"))
          ),
          h3("About the Developers:"),
          p("This application was developed by Sarah Abramowitz and Michael Levin at the University of Pennsylvania. For more information or to report issues, please contact ", tags$a(href = "mailto:Michael.Levin@pennmedicine.upenn.edu", "Michael.Levin@pennmedicine.upenn.edu"))
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store the current seed
  current_seed <- reactiveVal(NULL)

  # Function to generate the plot
  generate_plot <- function() {
    model_selection <- if (is.null(input$selected_models)) model_list else input$selected_models

    # Use the seed from advanced options if set, otherwise generate a new one
    if (!is.na(input$seed_input) && !is.null(input$seed_input)) {
      seed_value <- input$seed_input
    } else {
      seed_value <- as.integer(runif(1) * 1e6) # Generate a random seed
      showNotification(paste("Using a randomly generated seed:", seed_value), type = "message")
    }
    current_seed(seed_value) # Store the current seed

    set.seed(seed_value)

    ntile_list <- paste("ntile_", model_selection, sep = "")
    sample_size <- ifelse(is.null(input$sample_size_input), 5, input$sample_size_input)
    random_ntile <- sample_n(df_ntile_norm, sample_size) %>% select(IID, all_of(ntile_list))

    melt_random_ntile <- melt(random_ntile, id = c("IID")) %>%
      mutate(variable = str_replace(variable, "ntile_", ""))

    melt_random_ntile$variable <- factor(melt_random_ntile$variable, levels = model_list)

    point_plot <- ggplot(data = melt_random_ntile, aes(x = variable, y = value, color = IID, group = IID)) +
      geom_point(size = 3) +
      geom_hline(yintercept = 50, linetype = "dashed") +
      facet_grid(rows = vars(IID)) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(x = "", y = "Percentile", title = paste("Seed:", current_seed())) +
      custom_theme +
      scale_color_jama(guide = "none")

    beeswarm_plot <- melt_random_ntile %>%
      ggplot(aes(y = value, fill = IID), x = "a") +
      geom_swarm(dotsize = 1.5, color = "black", alpha = 0.5) +
      geom_boxplot(width = 0.15, outlier.shape = NA, fill = "white", linewidth = 1) +
      geom_hline(yintercept = 50, linetype = "dashed") +
      facet_grid(rows = vars(IID), scales = "free_x") +
      scale_fill_jama(guide = "none") +
      labs(x = "", y = "Percentile") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      custom_theme +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )

    (point_plot | beeswarm_plot) +
      plot_layout(ncol = 2, widths = c(9, 1))
  }

  # Reactive value to trigger initial plot
  initial_load <- reactiveVal(FALSE)

  # Observe the plot button click
  observeEvent(input$plot_button, {
    output$score_plot <- renderPlot({
      generate_plot()
    })
  })

  # Automatically generate plot on app startup
  observe({
    req(initial_load())
    output$score_plot <- renderPlot({
      generate_plot()
    })
  })

  # Set initial_load to TRUE after a short delay
  # This ensures all reactive elements are properly initialized
  session$onFlushed(function() {
    initial_load(TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
