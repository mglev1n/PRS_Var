# 
# 
# 
# #This version is used in the PRS_Var folder, attempting to host the shiny app publicly on github
# 
# library(shiny)
# library(shinyWidgets)
# library(dplyr)
# library(ggplot2)
# library(reshape2)
# library(stringr)
# library(ggsci)
# library(ggdist)
# library(patchwork)
# library(bigreadr)
# 
# # Define the list of models
# model_list <- c("PGS000010", "PGS000011", "PGS000012", "PGS000013", "PGS000018", "PGS000019", "PGS000057",
#                 "PGS000058", "PGS000059", "PGS000200", "PGS000296", "PGS000329", "PGS000337", "PGS000349",
#                 "PGS000710", "PGS000746", "PGS000747", "PGS000748", "PGS000749", "PGS000798", "PGS000818",
#                 "PGS000899", "PGS000962", "PGS001048", "PGS001314", "PGS001315", "PGS001316", "PGS001317",
#                 "PGS001355", "PGS001780", "PGS001839", "PGS002048", "PGS002244", "PGS002262", "PGS002775",
#                 "PGS002776", "PGS002777", "PGS002778", "PGS002809", "PGS003355", "PGS003356", "PGS003438",
#                 "PGS003446", "PGS003725", "PGS003726", "PGS003866", "PGS_LDP2Auto", "PGS_prscsx")
# 
# 
# df_ntile_norm <- bigreadr::fread2("CAD_PGS_ref_ntile.txt")
# 
# 
# custom_theme <- theme_minimal() +
#   theme(
#     panel.background = element_rect(fill = "white"),
#     text = element_text(family = "Arial", size = 14),
#     plot.title = element_text(size = 16, hjust = 0.5),
#     plot.subtitle = element_text(size = 14, hjust = 0.5),
#     # axis.title.x = element_text(size = 14),
#     # axis.title.y = element_text(size = 14),
#     # axis.text.x = element_text(size = 12),
#     # axis.text.y = element_text(size = 12),
#     plot.margin = margin(20, 20, 20, 20)
#   )
# 
# 
# # UI for Shiny App
# ui <- fluidPage(
#   titlePanel("CAD PRS Variability"),
#   "Plot ancestry-normalized CAD polygenic risk scores for a random selection of individuals from a HGDP/1k Genomes reference population",
#   sidebarLayout(
#     sidebarPanel(
#       markdown(
#         mds = c(
#           "#### 1) Select CAD PGS",
#           "Select PGS for CAD obtained from the [PGS Catalog](https://www.pgscatalog.org/trait/EFO_0001645/)"
#         )
#       ),
#       pickerInput("selected_models",
#                   label = NULL,
#                   choices = model_list,
#                   selected = model_list,
#                   options = pickerOptions(
#                     actionsBox = TRUE,
#                     size = 10,
#                     selectedTextFormat = "count > 3"
#                   ),
#                   multiple = TRUE),
#       hr(),
#       markdown(
#         mds = c(
#           "#### 2) Set Random Seed",
#           "Set a random seed used to randomly select indiviuals for plotting"
#         )
#       ),
#       numericInput("seed_input",
#                    label = NULL,
#                    value = 8675309),
#       hr(),
#       markdown(
#         mds = c(
#           "#### 3) Select number of individuals",
#           "Select number of individuals to include in plot of PGS variability"
#         )
#       ),
#       sliderInput("sample_size_input",
#                   label = NULL,
#                   min = 1,
#                   max = 6,
#                   value = 5,
#                   step = 1),
#       hr(),
#       actionButton("submit_button", "Generate Plot")
#     ),
#     mainPanel(
#       plotOutput("score_plot"),
#       # p("Plot ancestry-normalized CAD polygenic risk scores for a random selection of individuals from a HGDP/1k Genomes reference population")
#     )
#   )
# )
# 
# 
# # Server logic
# server <- function(input, output) {
#   observeEvent(input$submit_button, {
#     req(input$selected_models)
#     model_selection <- input$selected_models
# 
#     seed_value <- isolate(input$seed_input)  # Retrieve the seed value
# 
#     set.seed(seed_value)  # Set the seed value
# 
#     ntile_list <- paste("ntile_", model_selection, sep = "")
#     sample_size <- input$sample_size_input  # Get the user-selected sample size
#     random_ntile <- sample_n(df_ntile_norm, sample_size) %>% select(IID, all_of(ntile_list))
# 
#     melt_random_ntile <- melt(random_ntile, id = c("IID")) %>%
#       mutate(variable = str_replace(variable, "ntile_", ""))
# 
#     # Calculate mean percentiles
#     mean_percentiles <- melt_random_ntile %>%
#       group_by(IID) %>%
#       summarize(mean_percentile = mean(value))
# 
#     # Merge the mean percentiles back with the melted data for plotting
#     melt_random_ntile <- melt_random_ntile %>%
#       left_join(mean_percentiles, by = "IID")
# 
#     melt_random_ntile$variable <- factor(melt_random_ntile$variable, levels = model_list)
# 
#     output$score_plot <- renderPlot({
#       # line_plot <- ggplot(data = melt_random_ntile, aes(x = variable, y = value, color = IID, group = IID)) +
#       #   geom_line(size = 1.25) +
#       #   geom_text(data = mean_percentiles, aes(x = 1, y = 100, label = paste("Mean:", round(mean_percentile, 2))), hjust = 0, vjust = 0) +
#       #   geom_hline(yintercept = 50, linetype = "dashed") +
#       #   facet_grid(rows = vars(IID)) +
#       #   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#       #   labs(x = "", y = "Percentile") +
#       #   custom_theme +
#       #   theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) +
#       #   scale_color_jama(guide = "none")
# 
#       score_plot_ntile_all <- ggplot(data = melt_random_ntile, aes(x = variable, y = value, color = IID, group = IID)) +
#         geom_point(size = 3) +
#         geom_text(data = mean_percentiles,
#                   aes(x = length(model_list), y = 100, label = paste("Mean:", round(mean_percentile, 2))),
#                   hjust = 1, vjust = 10, color = "black") +
#         geom_hline(yintercept = 50, linetype = "dashed") +
#         labs(x = "Score", y = "Percentile") +
#         custom_theme +
#         facet_wrap(IID ~ . , nrow = sample_size) +
#         theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1), strip.background = element_blank(),
#               strip.text.x = element_blank()) +
#         scale_color_jama(guide = "none")
# 
#       score_plot_ntile_all
#       #
#       # beeswarm_plot <- melt_random_ntile %>%
#       #   ggplot(aes(y = value, fill = IID), x = "a") +
#       #   geom_swarm(dotsize = 1.5, color = "black") +
#       #   geom_hline(yintercept = 50, linetype = "dashed") +
#       #   facet_grid(rows = vars(IID), scales = "free_x") +
#       #   scale_fill_jama(guide = "none") +
#       #   labs(x = "", y = "Percentile") +
#       #   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#       #   custom_theme +
#       #   theme(axis.text.x = element_blank(),
#       #         axis.ticks.x = element_blank())
#       #
#       # (score_plot_ntile_all | beeswarm_plot) +
#       #   plot_layout(ncol = 2, widths = c(9,1))
#       #
#     },
#     height = 600)
#   })
# }
# 
# 
# # Run the application
# shinyApp(ui = ui, server = server)

# Forecasting Sandbox ----
# This is an example for a Shinylive R app
# The app provides a forecasting sandbox for the AirPassengers dataset
# It supports 3 stats forecasting models - Linear Regression, ARIMA, and Holt-Winters

library(shiny)
data(AirPassengers)

#df_ntile_norm <- bigreadr::fread2("CAD_PGS_ref_ntile.txt")
# UI ----
ui <- fluidPage(

  # App title ----
  titlePanel("Sarah's frustrated Forecasting Sandbox"),
  sidebarLayout(

    sidebarPanel(width = 3,
                 selectInput(inputId = "model",
                             label = "Select Model",
                             choices = c("Linear Regression", "ARIMA", "Holt-Winters"),
                             selected = "Linear Regression"),
                 # Linear Regression model arguments
                 conditionalPanel(condition = "input.model == 'Linear Regression'",
                                  checkboxGroupInput(inputId = "lm_args",
                                                     label = "Select Regression Features:",
                                                     choices = list("Trend" = 1,
                                                                    "Seasonality" = 2),
                                                     selected = 1)),
                 # ARIMA model arguments
                 conditionalPanel(condition = "input.model == 'ARIMA'",
                                  h5("Order Parameters"),
                                  sliderInput(inputId = "p",
                                              label = "p:",
                                              min = 0,
                                              max = 5,
                                              value = 0),
                                  sliderInput(inputId = "d",
                                              label = "d:",
                                              min = 0,
                                              max = 5,
                                              value = 0),
                                  sliderInput(inputId = "q",
                                              label = "q:",
                                              min = 0,
                                              max = 5,
                                              value = 0),
                                  h5("Seasonal Parameters:"),
                                  sliderInput(inputId = "P",
                                              label = "P:",
                                              min = 0,
                                              max = 5,
                                              value = 0),
                                  sliderInput(inputId = "D",
                                              label = "D:",
                                              min = 0,
                                              max = 5,
                                              value = 0),
                                  sliderInput(inputId = "Q",
                                              label = "Q:",
                                              min = 0,
                                              max = 5,
                                              value = 0)
                 ),
                 # Holt Winters model arguments
                 conditionalPanel(condition = "input.model == 'Holt-Winters'",
                                  checkboxGroupInput(inputId = "hw_args",
                                                     label = "Select Holt-Winters Parameters:",
                                                     choices = list("Beta" = 2,
                                                                    "Gamma" = 3),
                                                     selected = c(1, 2, 3)),
                                  selectInput(inputId = "hw_seasonal",
                                              label = "Select Seasonal Type:",
                                              choices = c("Additive", "Multiplicative"),
                                              selected = "Additive")),

                 checkboxInput(inputId = "log",
                               label = "Log Transformation",
                               value = FALSE),
                 sliderInput(inputId = "h",
                             label = "Forecasting Horizon:",
                             min = 1,
                             max = 60,
                             value = 24)
                 #   actionButton(inputId = "update",
                 #                 label = "Update!")

    ),

    # Main panel for displaying outputs ----
    mainPanel(width = 9,
              # Forecast Plot ----
              plotOutput(outputId = "fc_plot",
                         height = "400px")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # Load the dataset a reactive object
  d <- reactiveValues(df = data.frame(input = as.numeric(AirPassengers),
                                      index = seq.Date(from = as.Date("1949-01-01"),
                                                       by = "month",
                                                       length.out = length(AirPassengers))),
                      air = AirPassengers)

  # Log transformation
  observeEvent(input$log,{
    if(input$log){
      d$df <- data.frame(input = log(as.numeric(AirPassengers)),
                         index = seq.Date(from = as.Date("1949-01-01"),
                                          by = "month",
                                          length.out = length(AirPassengers)))

      d$air <- log(AirPassengers)
    } else {
      d$df <- data.frame(input = as.numeric(AirPassengers),
                         index = seq.Date(from = as.Date("1949-01-01"),
                                          by = "month",
                                          length.out = length(AirPassengers)))

      d$air <- AirPassengers
    }
  })

  # The forecasting models execute under the plot render
  output$fc_plot <- renderPlot({

    # if adding a prediction intervals level argument set over here
    pi <- 0.95

    # Holt-Winters model
    if(input$model == "Holt-Winters"){
      a <- b <- c <- NULL

      if(!"2" %in% input$hw_args){
        b <- FALSE
      }

      if(!"3" %in% input$hw_args){
        c <- FALSE
      }

      md <- HoltWinters(d$air,
                        seasonal = ifelse(input$hw_seasonal == "Additive", "additive", "multiplicative"),
                        beta = b,
                        gamma = c
      )
      fc <- predict(md, n.ahead = input$h, prediction.interval = TRUE) |>
        as.data.frame()
      fc$index <- seq.Date(from = as.Date("1961-01-01"),
                           by = "month",
                           length.out = input$h)
      # ARIMA model
    } else if(input$model == "ARIMA"){

      md <- arima(d$air,
                  order = c(input$p, input$d, input$q),
                  seasonal = list(order = c(input$P, input$D, input$Q))
      )
      fc <- predict(md, n.ahead = input$h, prediction.interval = TRUE) |>
        as.data.frame()
      names(fc) <- c("fit", "se")

      fc$index <- seq.Date(from = as.Date("1961-01-01"),
                           by = "month",
                           length.out = input$h)

      fc$upr <- fc$fit + 1.96 * fc$se
      fc$lwr <- fc$fit - 1.96 * fc$se
      # Linear Regression model
    } else if(input$model == "Linear Regression"){

      d_lm <- d$df

      d_fc <- data.frame(index = seq.Date(from = as.Date("1961-01-01"),
                                          by = "month",
                                          length.out = input$h))

      if("1" %in% input$lm_args){
        d_lm$trend <- 1:nrow(d_lm)
        d_fc$trend <- (max(d_lm$trend) + 1):(max(d_lm$trend) + input$h)
      }

      if("2" %in% input$lm_args){
        d_lm$season <- as.factor(months((d_lm$index)))
        d_fc$season <- factor(months((d_fc$index)), levels = levels(d_lm$season))
      }

      md <- lm(input ~ ., data = d_lm[, - which(names(d_lm) == "index")])

      fc <- predict(md, n.ahead = input$h, interval = "prediction",
                    level = pi, newdata = d_fc) |>
        as.data.frame()


      fc$index <- seq.Date(from = as.Date("1961-01-01"),
                           by = "month",
                           length.out = input$h)

    }

    # Setting the plot
    at_x <- pretty(seq.Date(from = min(d$df$index),
                            to = max(fc$index),
                            by = "month"))

    at_y <- c(pretty(c(d$df$input, fc$upr)), 1200)



    print(c(min(d$df$input), max(fc$upr)))


    plot(x = d$df$index, y = d$df$input,
         col = "#1f77b4",
         type = "l",
         frame.plot = FALSE,
         axes = FALSE,
         panel.first = abline(h = at_y, col = "grey80"),
         main = "AirPassengers Forecast",
         xlim = c(min(d$df$index), max(fc$index)),
         ylim = c(min(c(min(d$df$input), min(fc$lwr))), max(c(max(fc$upr), max(d$df$input)))),
         xlab = paste("Model:", input$model, sep = " "),
         ylab = "Num. of Passengers (in Thousands)")
    mtext(side =1, text = format(at_x, format = "%Y-%M"), at = at_x,
          col = "grey20", line = 1, cex = 0.8)

    mtext(side =2, text = format(at_y, scientific = FALSE), at = at_y,
          col = "grey20", line = 1, cex = 0.8)
    lines(x = fc$index, y = fc$fit, col = '#1f77b4', lty = 2, lwd = 2)
    lines(x = fc$index, y = fc$upr, col = 'blue', lty = 2, lwd = 2)
    lines(x = fc$index, y = fc$lwr, col = 'blue', lty = 2, lwd = 2)

  })

}
#
# Create Shiny app ----
shinyApp(ui = ui, server = server)
