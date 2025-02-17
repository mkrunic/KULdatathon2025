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



topic_scores <- read.csv("data/results.csv", header = TRUE, sep = ',')

topic_scores <- topic_scores[, c("country_iso", "Renewable.Energy", "Carbon.Emissions",
                                 "Carbon.Capture", "Waste.Management", "Sustainable.Agriculture",
                                 "Water.Conservation", "Public.Awareness.Initiatives", "Carbon.Tax",
                                 "Biodiversity.Conservation", "Energy.Efficiency")]

# add up scores for each country
topic_scores <- topic_scores %>%
  group_by(country_iso) %>%
  summarise(
    Renewable.Energy = sum(Renewable.Energy, na.rm = TRUE),
    Carbon.Emissions = sum(Carbon.Emissions, na.rm = TRUE),
    Carbon.Capture = sum(Carbon.Capture, na.rm = TRUE),
    Waste.Management = sum(Waste.Management, na.rm = TRUE),
    Sustainable.Agriculture = sum(Sustainable.Agriculture, na.rm = TRUE),
    Water.Conservation = sum(Water.Conservation, na.rm = TRUE),
    Public.Awareness.Initiatives = sum(Public.Awareness.Initiatives, na.rm = TRUE),
    Carbon.Tax = sum(Carbon.Tax, na.rm = TRUE),
    Biodiversity.Conservation = sum(Biodiversity.Conservation, na.rm = TRUE),
    Energy.Efficiency = sum(Energy.Efficiency, na.rm = TRUE)
  )


# UI
ui <- fluidPage(
  titlePanel("World Map by Topic Scores"),
  sidebarLayout(
    sidebarPanel(
      selectInput("topic", "Choose a Topic:", 
                  choices = names(topic_scores)[names(topic_scores) != "country_iso"])
    ),
    mainPanel(
      plotOutput("worldMap")
    )
  )
)

server <- function(input, output) {


  
  output$worldMap <- renderPlot({
    selected_topic <- input$topic
    # Filter data based on selected topic
    filtered_data <- topic_scores %>%
      select(country_iso, selected_topic)
    
    # Get world map data with iso country codes
    country_codes <- read.csv(file = "data/longitude-latitude.csv", header = TRUE,  sep = ",", row.names = NULL,  stringsAsFactors = FALSE)
    
    # Merge country code data with topic scores
    map <- left_join(country_codes, filtered_data, by = c("ISO.ALPHA.3" = "country_iso"))

    # Get world map data for ggplot map
    world_map <- map_data("world")

    # Merge world map data with topic scores
    map <- left_join(world_map, map, by = c("region" = "Country"))

    # write data to a csv file
    # write.csv(map, "data/map_data.csv", row.names = FALSE)


    #print(summary(map_data))
    # Plot the map
    ggplot(data = map, aes(x = long, y = lat, group = group, fill = !!sym(selected_topic))) +
      geom_polygon(color = "black") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
      labs(title = paste("World Map -", selected_topic),
           fill = "Topic Score") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
