# This version is used in the PRS_Var folder, attempting to host the shiny app publicly on github

library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(ggsci)
library(ggdist)
library(patchwork)

# Define the list of models
model_list <- c("PGS000010", "PGS000011", "PGS000012", "PGS000013", "PGS000018", "PGS000019", "PGS000057", 
                "PGS000058", "PGS000059", "PGS000200", "PGS000296", "PGS000329", "PGS000337", "PGS000349", 
                "PGS000710", "PGS000746", "PGS000747", "PGS000748", "PGS000749", "PGS000798", "PGS000818", 
                "PGS000899", "PGS000962", "PGS001048", "PGS001314", "PGS001315", "PGS001316", "PGS001317", 
                "PGS001355", "PGS001780", "PGS001839", "PGS002048", "PGS002244", "PGS002262", "PGS002775", 
                "PGS002776", "PGS002777", "PGS002778", "PGS002809", "PGS003355", "PGS003356", "PGS003438", 
                "PGS003446", "PGS003725", "PGS003726", "PGS003866", "PGS_LDP2Auto", "PGS_prscsx")


df_ntile_norm <- bigreadr::fread2("CAD_PGS_ref_ntile.txt")


custom_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(family = "Arial", size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    # axis.title.x = element_text(size = 14),
    # axis.title.y = element_text(size = 14),
    # axis.text.x = element_text(size = 12),
    # axis.text.y = element_text(size = 12),
    plot.margin = margin(20, 20, 20, 20)
  )


# UI for Shiny App
ui <- fluidPage(
  titlePanel("CAD PRS Variability"),
  "Plot ancestry-normalized CAD polygenic risk scores for a random selection of individuals from a HGDP/1k Genomes reference population",
  sidebarLayout(
    sidebarPanel(
      markdown(
        mds = c(
          "#### 1) Select CAD PGS",
          "Select PGS for CAD obtained from the [PGS Catalog](https://www.pgscatalog.org/trait/EFO_0001645/)"
        )
      ),
      pickerInput("selected_models", 
                  label = NULL, 
                  choices = model_list, 
                  selected = model_list, 
                  options = pickerOptions(
                    actionsBox = TRUE, 
                    size = 10,
                    selectedTextFormat = "count > 3"
                  ),
                  multiple = TRUE),
      hr(),
      markdown(
        mds = c(
          "#### 2) Set Random Seed",
          "Set a random seed used to randomly select indiviuals for plotting"
        )
      ),
      numericInput("seed_input", 
                   label = NULL, 
                   value = 8675309),
      hr(),
      markdown(
        mds = c(
          "#### 3) Select number of individuals",
          "Select number of individuals to include in plot of PGS variability"
        )
      ),
      sliderInput("sample_size_input", 
                  label = NULL, 
                  min = 1, 
                  max = 6, 
                  value = 5, 
                  step = 1),
      hr(),
      actionButton("submit_button", "Generate Plot")
    ),
    mainPanel(
      plotOutput("score_plot"),
      # p("Plot ancestry-normalized CAD polygenic risk scores for a random selection of individuals from a HGDP/1k Genomes reference population")
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
    
    # Calculate mean percentiles
    mean_percentiles <- melt_random_ntile %>%
      group_by(IID) %>%
      summarize(mean_percentile = mean(value))
    
    # Merge the mean percentiles back with the melted data for plotting
    melt_random_ntile <- melt_random_ntile %>%
      left_join(mean_percentiles, by = "IID")
    
    melt_random_ntile$variable <- factor(melt_random_ntile$variable, levels = model_list)
    
    output$score_plot <- renderPlot({
      # line_plot <- ggplot(data = melt_random_ntile, aes(x = variable, y = value, color = IID, group = IID)) +
      #   geom_line(size = 1.25) +
      #   geom_text(data = mean_percentiles, aes(x = 1, y = 100, label = paste("Mean:", round(mean_percentile, 2))), hjust = 0, vjust = 0) +
      #   geom_hline(yintercept = 50, linetype = "dashed") +
      #   facet_grid(rows = vars(IID)) +
      #   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      #   labs(x = "", y = "Percentile") +
      #   custom_theme +
      #   theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) +
      #   scale_color_jama(guide = "none")
      
      score_plot_ntile_all <- ggplot(data = melt_random_ntile, aes(x = variable, y = value, color = IID, group = IID)) +
        geom_point(size = 3) +
        geom_text(data = mean_percentiles, 
                  aes(x = length(model_list), y = 100, label = paste("Mean:", round(mean_percentile, 2))), 
                  hjust = 1, vjust = 10, color = "black") +
        geom_hline(yintercept = 50, linetype = "dashed") +
        labs(x = "Score", y = "Percentile") + 
        custom_theme +
        facet_wrap(IID ~ . , nrow = sample_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1), strip.background = element_blank(),
              strip.text.x = element_blank()) +
        scale_color_jama(guide = "none")
      
      score_plot_ntile_all
      # 
      # beeswarm_plot <- melt_random_ntile %>%
      #   ggplot(aes(y = value, fill = IID), x = "a") +
      #   geom_swarm(dotsize = 1.5, color = "black") +
      #   geom_hline(yintercept = 50, linetype = "dashed") +
      #   facet_grid(rows = vars(IID), scales = "free_x") +
      #   scale_fill_jama(guide = "none") +
      #   labs(x = "", y = "Percentile") +
      #   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      #   custom_theme +
      #   theme(axis.text.x = element_blank(),
      #         axis.ticks.x = element_blank())
      # 
      # (score_plot_ntile_all | beeswarm_plot) +
      #   plot_layout(ncol = 2, widths = c(9,1))
      # 
    },
    height = 600)
  })
}


# Run the application
shinyApp(ui = ui, server = server)