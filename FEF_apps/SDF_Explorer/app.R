# Load libraries
library(shiny)
library(readxl)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(ggthemes)
library(plotly)
library(gt)
library(car)

# Load your new dataset
dat.fracs <- read_excel("DC_FEF_Main.xlsx", sheet = "fracs")

# UI for the sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Explore Data", tabName = "explore", icon = icon("dashboard")),
    sidebarMenu(
      checkboxGroupInput("watershed_selector", "Select Watersheds", choices = unique(dat.fracs$watershed)),
      checkboxGroupInput("variable_selector", "Select Variables", choices = names(dat.fracs)[8:ncol(dat.fracs)]),
      checkboxGroupInput("depth_selector", "Select Depth", choices = unique(dat.fracs$depth)),
      actionButton("run_anova", "Run ANOVA")
    )
  )
)

# UI for main content
ui <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "explore",
      conditionalPanel(
        condition = "input.watershed_selector.length === 0 && input.variable_selector.length === 0 && input.depth_selector.length === 0",
        HTML("<p style='color: black; text-align: center; font-size: 16px; font-weight: bold;'>Please select at least one watershed, one variable, and one depth to use the explorer tool.</p>")
      ),
      fluidRow(
        column(6, plotlyOutput("fraction_plot")),
        column(6, 
               verbatimTextOutput("anova_results"),
               plotlyOutput("hist_plot")
        )
      ),
      dataTableOutput("data_table")
    )
  )
)

# Server logic for the reactive function
server <- function(input, output, session) {
  shinyjs::enable()  # Enable shinyjs
  
  filtered_data <- reactive({
    dat.fracs %>%
      filter(watershed %in% input$watershed_selector &
               depth %in% input$depth_selector) %>%
      select(SampleID, watershed, depth, !!input$variable_selector)
  })
  
  # Distribution plot (Box plot) with plotly
  output$fraction_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data(), aes_string(x = "watershed", y = input$variable_selector, color = "watershed")) +
               geom_boxplot() +
               geom_jitter(aes(x = watershed, y = !!sym(input$variable_selector), text = paste("Sample ID:", as.character(SampleID))),
                           position = position_jitterdodge(dodge.width = 0.25, jitter.width = 0.2),
                           alpha = 0.7) +
               labs(title = "")
    )
  })
  
  # Data table
  output$data_table <- renderDataTable({
    filtered_data()
  })
  
  # Reactive expression for ANOVA
  anova_results <- eventReactive(input$run_anova, {
    if (length(input$watershed_selector) > 1) {
      formula_text <- paste(input$variable_selector, "~ watershed")
      anova_model <- aov(as.formula(formula_text), data = filtered_data())
      anova_summary <- summary(anova_model)
      residuals <- residuals(anova_model)
      
      # Histogram of residuals using plotly
      hist_plot <- plot_ly(x = residuals, type = "histogram", nbinsx = 30, marker = list(color = "#00C9A7")) %>%
        layout(title = "Histogram of Residuals", xaxis = list(title = "Residuals"), yaxis = list(title = "Frequency"))
      
      list(anova_summary = anova_summary, hist_plot = hist_plot)
    } else {
      NULL
    }
  })
  
  # Display ANOVA results and histogram of residuals
  output$anova_results <- renderPrint({
    anova_results()$anova_summary
  })
  
  output$hist_plot <- renderPlotly({
    anova_results()$hist_plot
  })
}

# Combine sidebar and main content
ui <- dashboardPage(
  dashboardHeader(title = "Soil Fraction Explorer"),
  sidebar,
  ui
)

# Run the Shiny app
shinyApp(ui = ui, server = server)
