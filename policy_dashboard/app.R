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
library(RColorBrewer)


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

  tsne_data <- read.csv("data/embedding.csv", header = TRUE, sep = ',')
  continent_levels <- sort(unique(tsne_data$continent))
  # Use Pastel1 for up to 9 categories; expand if you have >9 continents
  continent_pal <- brewer.pal(n = max(9, length(continent_levels)), name = "Pastel1")[1:length(continent_levels)]
  names(continent_pal) <- continent_levels
  
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
          plotlyOutput("worldMap")  # Add source argument
        ),
        column(4,  # Right panel for displaying information
          wellPanel(style = "height: 575px;",  # Set a specific height for the panel
            h4("Country Information"),
            plotlyOutput("countryInfo", height = "250px"),  # Output for country information with maximum height of 250px
            textOutput("coutrySummary")  # Add text output for country summary
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
        column(12,  # Keep the width of the map column
          style = "padding-left: 0;",
          plotlyOutput("stats_map", height = "600px")  # Set a specific height for the map
        )
      )
    ),
    tabPanel("Analysis", value = "analysis_tab",
      fluidRow(
        fluidRow(
          style = "padding-left: 15px;",
          column(3,  # New column for the dropdown menu
            style = list(marginTop = "10px"),  # Add margin to the top
            selectInput(
              "continent", "Select Continent:",
              choices = continent_levels,
              selected = continent_levels[1],  # Default to first continent
              multiple = TRUE
            )
          ),
          column(3,  # New column for the dropdown menu
            style = list(marginTop = "10px"),  # Add margin to the top
            selectInput(
              "country", "Select Country:",
              choices = sort(unique(topic_scores$country_iso)),
              selected = NULL,
              multiple = TRUE
            )
          )
        )
      ),
      fluidRow(
        column(12,
          plotlyOutput("tsnePlot")  # Output for the t-SNE plot
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
                  hoverinfo = "text",
                  source = "policies_source") %>%
      layout(title = paste("World Map -", selected_topic),
             geo = list(showframe = FALSE, 
                        projection = list(type = 'equal earth')))

    # Register the hover event
    event_register(p, 'plotly_hover')
    
    return(p)
  })
  outputOptions(output, "worldMap", suspendWhenHidden = FALSE)

  # Observe hover events to update the country information panel
  observeEvent(event_data("plotly_hover", source = "policies_source"), {
    hover_data <- event_data("plotly_hover", source = "policies_source")
    if (!is.null(hover_data) && hover_data$curveNumber == 0 ) {  # Check if the event is from the first curve (the map)
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
      
      output$coutrySummary <- renderText({
        # load country summary data
        country_summary <- read.csv("data/concatenated_summaries_per_country.csv", header = TRUE, sep = ',')
        # filter for selected country
        country_summary <- country_summary[country_summary$country_iso == country_code, ]
        # return the summary
        paste("Climate Policy Summary:", country_summary$policy_summary)
      })
    }
  })

  output$stats_map <- renderPlotly({
    selected_stat <- input$statsSelect  # Use the selected topic from the dropdown
    
    # create plot according to selected stat
    if (selected_stat == "Yearly Temperature Average") {

      # Load temperature data
      yearly_avg <- read.csv("data/avg_temp_plotting.csv", header = TRUE, sep = ',')
      
      # Determine the min and max values for the color scale
      zmin <- min(yearly_avg$AverageTemperature, na.rm = TRUE)
      zmax <- max(yearly_avg$AverageTemperature, na.rm = TRUE)

      # plot temperature data
      fig <- plot_ly(
        data = yearly_avg,
        locations = ~Country, 
        locationmode = "country names",
        frame = ~Year,  # Animation frame
        z = ~AverageTemperature,
        colorscale = "RdYlBu",
        type = "choropleth",
        hoverinfo = "text",
        text = ~paste0("Country: ", Country, "<br>Temperature: ", round(AverageTemperature, 2)),
        zmin = zmin,  # Set the minimum value for the color scale
        zmax = zmax   # Set the maximum value for the color scale
      )

      # Customize layout
      fig <- fig %>% layout(
        title = ~paste0("Average Temperature by Country (2000-", max(yearly_avg$Year), ")"),
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "equirectangular")
        )
      )

      # Adjust animation settings
      fig <- fig %>%
        animation_opts(frame = 1000, transition = 500)

      event_register(fig, 'plotly_hover')
      # Display the figure
      fig

    } else if (selected_stat == "CO2") {
      # Load CO2 data
      co2_melted <- read.csv("data/co2_plotting.csv", header = TRUE, sep = ',')

      # Determine the min and max values for the color scale
      zmin <- min(co2_melted$value, na.rm = TRUE)
      zmax <- 25
      
      fig <- plot_ly(
        data = co2_melted, 
        type = "choropleth",
        locations = ~Country_Name,  # Ensure column name is referenced correctly
        locationmode = "country names",
        z = ~value,
        frame = ~Year,  # Animation frame
        colorscale = "RdBu",  # Reverse Blue-Red for emission intensity
        colorbar = list(title = "CO₂ Emissions"),
        hoverinfo = "text",
        text = ~paste("Country:", Country_Name, "<br>Year:", Year, "<br>CO₂ Emission:", round(value, 2)),
        zmin = zmin,  # Set the minimum value for the color scale
        zmax = zmax   # Set the maximum value for the color scale
      ) %>%
        layout(
          title = "CO₂ Emission by Countries by Year",
          geo = list(
            showframe = FALSE,
            showcoastlines = TRUE,
            projection = list(type = "equirectangular")
          ),
          coloraxis = list(cmin = 0, cmax = 200)  # Reverse scale range
        )

      # Display the plot
      fig
    } else if (selected_stat == "Renewable Energy") {
      # Load renewable energy data
      renewable_energy <- read.csv("data/renewables_plotting.csv", header = TRUE, sep = ',')
      
      # Determine the max value for the color scale
      max_value <- max(renewable_energy$Proportion_Renewables, na.rm = TRUE)

      fig <- plot_ly(
        data = renewable_energy, 
        type = "choropleth",
        locations = ~Country,  # Ensure column name matches exactly
        locationmode = "country names",
        z = ~Proportion_Renewables,
        frame = ~Year,  # Animation frame
        colorscale = "Greens",  # Use green color scale for renewable energy
        reversescale = TRUE,  # Reverse the color scale
        colorbar = list(title = "Renewable Energy (%)"),
        hoverinfo = "text",
        text = ~paste("Country:", Country, "<br>Year:", Year, 
                      "<br>Renewable Energy %:", round(Proportion_Renewables, 2))
      ) %>%
        layout(
          title = "Average Proportion of Energy from Renewables by Country Over Time",
          geo = list(
            showframe = FALSE,
            showcoastlines = TRUE,
            projection = list(type = "equirectangular")
          ),
          coloraxis = list(cmin = 0, cmax = max_value)  # Set color range
        )

      # Display the plot
      fig
    }
  }
  )

  # Add the server logic for the t-SNE plot
  output$tsnePlot <- renderPlotly({
    df <- tsne_data  # Use the t-SNE data
    plot_ly(
      data = df,
      x = ~t.SNE.1,
      y = ~t.SNE.2,
      color = ~continent,
      colors = continent_pal,
      symbol = I("circle"),
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

shinyApp(ui = ui, server = server)
