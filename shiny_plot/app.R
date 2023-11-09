# This version is used in the PRS_Var folder, attempting to host the shiny app publicly on github

library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(ggsci)

# Define the list of models
model_list <- c("PGS000013", "PGS000018", "PGS000058", "PGS000296", "PGS000329", "PGS000337", "PGS000746", "PGS000747", "PGS000748", "PGS000749", "PGS000899",
                "PGS001355", "PGS001780", "PGS001839", "PGS002048", "PGS002244", "PGS002775", "PGS002776", "PGS002809", "PGS003355", "PGS003356", "PGS003438",
                "PGS003446", "PGS003725", "PGS003726", "PGS_LDP2Auto", "PGS_prscsx")

df_ntile_norm <- bigreadr::fread2("/project/damrauer_shared/Users/sabramow/PRS_Var/data/CAD_PGS_ref_ntile.txt")

custom_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(20, 20, 20, 20)
  )


# UI for Shiny App
ui <- fluidPage(
  titlePanel("Plot CAD PRS"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_models", "Select Models", choices = model_list, selected = model_list),
      numericInput("seed_input", "Seed value:", value = 2023),
      sliderInput("sample_size_input", "Number of Individuals", min = 1, max = 6, value = 5, step = 1),
      actionButton("submit_button", "Generate Plot")
    ),
    mainPanel(
      plotOutput("score_plot"),
      p("Plot ancestry-normalized CAD polygenic risk scores for a random selection of individuals from a HGDP/1k Genomes reference population")
    )
  )
)


# Server logic
server <- function(input, output) {
  observeEvent(input$submit_button, {
    req(input$selected_models)
    model_selection <- input$selected_models
    
    seed_value <- isolate(input$seed_input)  # Retrieve the seed value
    
    set.seed(seed_value)  # Set the seed value
    
    ntile_list <- paste("ntile_", model_selection, sep = "")
    sample_size <- input$sample_size_input  # Get the user-selected sample size
    random_ntile <- sample_n(df_ntile_norm, sample_size) %>% select(IID, all_of(ntile_list))
    
    melt_random_ntile <- melt(random_ntile, id = c("IID")) %>%
      mutate(variable = str_replace(variable, "ntile_", ""))
    
    melt_random_ntile$variable <- factor(melt_random_ntile$variable, levels = model_list)
    
    output$score_plot <- renderPlot({
      ggplot(data = melt_random_ntile, aes(x = variable, y = value, color = IID, group = IID)) +
        geom_line(size = 1.25) +
        labs(x = "Score", y = "Percentile") +
        custom_theme +
        theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 1)) +
        scale_color_jama(guide = "none")
    })
  })
}


# Run the application
shinyApp(ui = ui, server = server)



