######
######  R-shiny dashboard for visualizing the environmental policy focus of each country.
######
######  Datathon LSTAT/KUL, Febuary 2025
######

# Load libraries
library(shiny)
library(dplyr)
library(paletteer)
library(ggplot2)
library(maps)
library(plotly)
library(tidyr)


topic_scores <- read.csv("data/topic_scores_output.csv", header = TRUE, sep = ';')

topic_scores <- topic_scores[, c("country_iso", "Renewable.Energy", "Carbon.Emissions",
                                 "Carbon.Capture", "Waste.Management", "Sustainable.Agriculture",
                                 "Water.Conservation", "Public.Awareness.Initiatives", "Carbon.Tax",
                                 "Biodiversity.Conservation", "Energy.Efficiency")]

# add up scores for each country
topic_scores <- topic_scores %>%
  group_by(country_iso) %>%
  summarise(Renewable.Energy = sum(Renewable.Energy, na.rm = TRUE) / n(),
    Carbon.Emissions = sum(Carbon.Emissions, na.rm = TRUE) / n(),
    Carbon.Capture = sum(Carbon.Capture, na.rm = TRUE) / n(),
    Waste.Management = sum(Waste.Management, na.rm = TRUE) / n(),
    Sustainable.Agriculture = sum(Sustainable.Agriculture, na.rm = TRUE) / n(),
    Water.Conservation = sum(Water.Conservation, na.rm = TRUE) / n(),
    Public.Awareness.Initiatives = sum(Public.Awareness.Initiatives, na.rm = TRUE) / n(),
    Carbon.Tax = sum(Carbon.Tax, na.rm = TRUE) / n(),
    Biodiversity.Conservation = sum(Biodiversity.Conservation, na.rm = TRUE) / n(),
    Energy.Efficiency = sum(Energy.Efficiency, na.rm = TRUE) / n()
  ) %>%
  mutate(
    total_score = Renewable.Energy + Carbon.Emissions + Carbon.Capture + 
                  Waste.Management + Sustainable.Agriculture + 
                  Water.Conservation + Public.Awareness.Initiatives + 
                  Carbon.Tax + Biodiversity.Conservation + 
                  Energy.Efficiency,
    Renewable.Energy = Renewable.Energy / total_score,
    Carbon.Emissions = Carbon.Emissions / total_score,
    Carbon.Capture = Carbon.Capture / total_score,
    Waste.Management = Waste.Management / total_score,
    Sustainable.Agriculture = Sustainable.Agriculture / total_score,
    Water.Conservation = Water.Conservation / total_score,
    Public.Awareness.Initiatives = Public.Awareness.Initiatives / total_score,
    Carbon.Tax = Carbon.Tax / total_score,
    Biodiversity.Conservation = Biodiversity.Conservation / total_score,
    Energy.Efficiency = Energy.Efficiency / total_score
  ) %>%
  select(-total_score)  # Remove the total_score column if not needed


# UI
ui <- fluidPage(
  titlePanel("World Map - Environmental Policy Focus"),
  tabsetPanel(
    id = "main_tabs",  # Give the tabset an ID
  tabPanel("Policies Map", value = "policy_tab",
      fluidRow(
        column(4,  # New column for the dropdown menu
          style = list(marginTop = "10px"),  # Add margin to the top
          selectInput("topicSelect", "Select Topic:", 
                      choices = c("Renewable.Energy", "Carbon.Emissions", "Carbon.Capture", 
                                  "Waste.Management", "Sustainable.Agriculture", 
                                  "Water.Conservation", "Public.Awareness.Initiatives", 
                                  "Carbon.Tax", "Biodiversity.Conservation", 
                                  "Energy.Efficiency"),
                      selected = "Renewable.Energy")  # Default selection
        )
      )
      ,
      fluidRow(
        column(8,  # Keep the width of the map column
          style = "padding-left: 0;",  # Remove left padding
          plotlyOutput("worldMap")
        ),
        column(4,  # Right panel for displaying information
          wellPanel(
            h4("Country Information"),
            plotlyOutput("countryInfo", height = "250px")  # Output for country information with maximum height of 250px
          )
        )
      )
    ),
    tabPanel("Environmental Statistics", value = "stats_tab",
      fluidRow(
        column(4,  # New column for the dropdown menu
          style = list(marginTop = "10px"),  # Add margin to the top
          selectInput("statsSelect", "Select Topic:", 
                      choices = c("Renewable Energy", "Yearly Temperature Average", "CO2"),
                      selected = "Yearly Temperature Average")  # Default selection
        )
      )
      ,
      fluidRow(
        column(8,  # Keep the width of the map column
          style = "padding-left: 0;",
        ),
        column(4,  # Right panel for displaying information
          wellPanel(
            h4("Environmental Statistics"),
            
          )
        )
      )
      ),
      selected = "policy_tab"
    )
  )


server <- function(input, output, session) {
  output$worldMap <- renderPlotly({
    selected_topic <- input$topicSelect  # Use the selected topic from the dropdown
    # Filter data based on selected topic
    filtered_data <- topic_scores %>%
      select(country_iso, selected_topic)  # Include all policy counts
    
    # Get world map data with iso country codes
    country_codes <- read.csv(file = "data/longitude-latitude.csv", header = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)

    # Merge country code data with topic scores
    map_data <- left_join(country_codes, filtered_data, by = c("ISO.ALPHA.3" = "country_iso"))

    # Drop rows with NA values
    map_data <- map_data[complete.cases(map_data), ]

    # Create the plot using plotly
    p <- plot_ly(data = map_data, 
                  type = 'choropleth', 
                  locations = map_data$ISO.ALPHA.3,
                  locationmode = "ISO-3", 
                  customdata = map_data$ISO.ALPHA.3,
                  z = ~get(selected_topic), 
                  text = ~paste("Country:", Country, " - ", ISO.ALPHA.3),
                  color = ~get(selected_topic), 
                  colors = "Blues", 
                  colorbar = list(title = "Topic Score"),
                  hoverinfo = "text") %>%
      layout(title = paste("World Map -", selected_topic),
             geo = list(showframe = FALSE, 
                        projection = list(type = 'equal earth')))

    # Register the hover event
    event_register(p, 'plotly_hover')
    
    return(p)
  })
  outputOptions(output, "worldMap", suspendWhenHidden = FALSE)

  # Observe hover events to update the country information panel
  observeEvent(event_data("plotly_hover"), {
    hover_data <- event_data("plotly_hover")
    if (!is.null(hover_data) && hover_data$curveNumber == 0) {  # Check if the event is from the first curve (the map)
      country_code <- hover_data$customdata[1]  # Get the country code from the hover data
      country_info <- topic_scores %>%
        filter(country_iso == country_code)  # Filter the data for the selected country
      
      output$countryInfo <- renderPlotly({  # Change renderPlot to renderPlotly
        # Filter for country data
        normalized_scores <- topic_scores %>%
          filter(country_iso == country_code) %>% # pivot table, one column for topic names and 1 for values
          pivot_longer(cols = -country_iso, names_to = "topic_names", values_to = "values")
        # Rename the topics with only their initials (split by '.' and take the first letters)
        normalized_scores$topic_names <- sapply(strsplit(normalized_scores$topic_names, "\\."), function(x) paste0(substr(x, 1, 1), collapse = ""))
        # Plotting the histogram
        h <- plot_ly(normalized_scores, x = ~topic_names, y = ~values, type = 'bar', 
                      marker = list(color = 'rgb(12,48,108)'), hoverinfo = 'none') %>%
          layout(title = "Relative Focus for Each Topic",
                 font = list(color = '#333', size = 10, family = 'Helvetica'),
                 xaxis = list(title = "Topic"), 
                 yaxis = list(title = "Score"),
                 paper_bgcolor = '#f5f5f5',
                 plot_bgcolor = '#f5f5f5')

        h  # Return the plotly object
      })
    }
  })
  
}

shinyApp(ui = ui, server = server)
