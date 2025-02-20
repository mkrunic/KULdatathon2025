
library(shiny)
library(plotly)
library(stringr)
library(dplyr)
library(RColorBrewer)

# Load data
data <- read.csv("/Users/sofialuk/Downloads/KULdatathon2025-main/policy_dashboard/data/embedding.csv", stringsAsFactors = FALSE)

# Remove any rows with NA in 'continent'
data <- data %>% filter(!is.na(continent))

# Wrap policy summaries for better hover readability
data$policy_summary_wrapped <- sapply(
  data$policy_summary, 
  function(x) paste(strwrap(x, width = 50), collapse = "<br>")
)

# Prepare consistent pastel colors for each continent
continent_levels <- sort(unique(data$continent))
# Use Pastel1 for up to 9 categories; expand if you have >9 continents
continent_pal <- brewer.pal(n = max(9, length(continent_levels)), name = "Pastel1")[1:length(continent_levels)]
names(continent_pal) <- continent_levels

ui <- fluidPage(
  titlePanel("Interactive t-SNE Embedding of Climate Policies"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "continent", "Select Continent:",
        choices = continent_levels,
        selected = continent_levels[1],  # Default to first continent
        multiple = TRUE
      ),
      selectInput(
        "country", "Select Country:",
        choices = sort(unique(data$country_iso)),
        selected = NULL,
        multiple = TRUE
      )
    ),
    mainPanel(
      plotlyOutput("tsnePlot")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data filter: show only selected continents + countries
  filtered_data <- reactive({
    df <- data
    
    # Filter by chosen continents (if any)
    if (length(input$continent) > 0) {
      df <- df %>% filter(continent %in% input$continent)
    }
    # Filter by chosen countries (if any)
    if (length(input$country) > 0) {
      df <- df %>% filter(country_iso %in% input$country)
    }
    
    df
  })
  
  output$tsnePlot <- renderPlotly({
    df <- filtered_data()
    
    plot_ly(
      data = df,
      x = ~t.SNE.1,
      y = ~t.SNE.2,
      color = ~continent,
      colors = continent_pal,
      symbol = I("circle"),  # All markers are circles
      text = ~paste(
        "Country:", country_iso,
        "<br>Policy Name:", policy_name,
        "<br>Summary:", policy_summary_wrapped
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers"
    ) %>%
      layout(
        title = "Policy Description Embeddings",
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        xaxis = list(title = "t-SNE 1", zeroline = FALSE, showgrid = FALSE),
        yaxis = list(title = "t-SNE 2", zeroline = FALSE, showgrid = FALSE),
        legend = list(title = list(text = "Continent & Country"))
      )
  })
}

shinyApp(ui, server)
