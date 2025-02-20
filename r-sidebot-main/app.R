# library(shiny)
library(bslib)
library(promises)
library(fastmap)
library(duckdb)
library(DBI)
library(fontawesome)
library(reactable)
library(here)
library(plotly)
library(ggplot2)
library(ggridges)
library(dplyr)
library(ellmer)
library(shinychat)

# Open the duckdb database
conn <- dbConnect(duckdb(), dbdir = here("climate.duckdb"), read_only = TRUE)
# Close the database when the app stops
onStop(\() dbDisconnect(conn))

# gpt-4o does much better than gpt-4o-mini, especially at interpreting plots
openai_model <- "gpt-4o"

# Dynamically create the system prompt, based on the real data. For an actually
# large database, you wouldn't want to retrieve all the data like this, but
# instead either hand-write the schema or write your own routine that is more
# efficient than system_prompt().
system_prompt_str <- system_prompt(dbGetQuery(conn, "SELECT * FROM climate"), "climate")

# This is the greeting that should initially appear in the sidebar when the app
# loads.
greeting <- paste(readLines(here("greeting_climate.md")), collapse = "\n")

icon_explain <- tags$img(src = "stars.svg")

ui <- page_sidebar(
  style = "background-color: rgb(248, 248, 248);",
  title = "Climate policies",
  includeCSS(here("styles.css")),
  sidebar = sidebar(
    width = 400,
    style = "height: 100%;",
    chat_ui("chat", height = "100%", fill = TRUE)
  ),
  useBusyIndicators(),
  
  # 🏷️ Header
  textOutput("show_title", container = h3),
  verbatimTextOutput("show_query") |>
    tagAppendAttributes(style = "max-height: 100px; overflow: auto;"),
  
  # 🎯 Value boxes
  layout_columns(
    fill = FALSE,
    value_box(
      "Total number of policies",
      textOutput("total_policies", inline = TRUE)
    ),
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      "Average carbon emissions",
      textOutput("average_carbon", inline = TRUE)
    ),
    value_box(
      "Average renewable energy",
      textOutput("average_renewable", inline = TRUE)
    ),
  ),
  layout_columns(
    style = "min-height: 450px;",
    col_widths = c(6, 6, 12),
    
    # 📊 Scatter plot
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Target end dates for policies",
        span(
          actionLink(
            "interpret_histogram",
            icon_explain,
            class = "me-3 text-decoration-none",
            aria_label = "Explain histogram"
          ),
          popover(
            title = "Add a color variable", placement = "top",
            fa_i("ellipsis"),
            radioButtons(
              "scatter_color",
              NULL,
              c("none", "sex", "smoker", "day", "time"),
              inline = TRUE
            )
          )
        )
      ),
      plotlyOutput("histogram")
    ),
    
    # 📊 Scatter plot
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Renewable energy vs carbon emissions",
        span(
          actionLink(
            "interpret_scatter",
            icon_explain,
            class = "me-3 text-decoration-none",
            aria_label = "Explain scatter plot"
          ),
          popover(
            title = "Add a color variable", placement = "top",
            fa_i("ellipsis"),
            radioButtons(
              "scatter_color",
              NULL,
              c("none", "sex", "smoker", "day", "time"),
              inline = TRUE
            )
          )
        )
      ),
      plotlyOutput("scatterplot")
    ),
      
    # 🔍 Data table
    card(
      style = "height: 500px;",
      card_header("Climate policy data"),
      reactableOutput("table", height = "100%")
      ),
    
    # 📊 Ridge plot
    # card(
    #   card_header(
    #     class = "d-flex justify-content-between align-items-center",
    #     "Country percentages",
    #     span(
    #       actionLink(
    #         "interpret_ridge",
    #         icon_explain,
    #         class = "me-3 text-decoration-none",
    #         aria_label = "Explain ridgeplot"
    #       ),
    #       popover(
    #         title = "Split ridgeplot", placement = "top",
    #         fa_i("ellipsis"),
    #         radioButtons(
    #           "tip_perc_y",
    #           "Split by",
    #           c("sex", "smoker", "day", "time"),
    #           "day",
    #           inline = TRUE
    #         )
    #       )
    #     )
    #   ),
    #   plotOutput("tip_perc")
    # ),
  )
)

server <- function(input, output, session) {
  # 🔄 Reactive state/computation --------------------------------------------
  
  current_title <- reactiveVal(NULL)
  current_query <- reactiveVal("")
  
  # This object must always be passed as the `.ctx` argument to query(), so that
  # tool functions can access the context they need to do their jobs; in this
  # case, the database connection that query() needs.
  ctx <- list(conn = conn)
  
  # The reactive data frame. Either returns the entire dataset, or filtered by
  # whatever Sidebot decided.
  climate_data <- reactive({
    sql <- current_query()
    if (is.null(sql) || sql == "") {
      sql <- "SELECT * FROM climate;"
    }
    dbGetQuery(conn, sql)
  })
  
  
  
  # 🏷️ Header outputs --------------------------------------------------------
  
  output$show_title <- renderText({
    current_title()
  })
  
  output$show_query <- renderText({
    current_query()
  })
  
  
  
  # 🎯 Value box outputs -----------------------------------------------------
  
  output$total_policies <- renderText({
    nrow(climate_data())
  })
  
  output$average_carbon <- renderText({
    x <- mean(climate_data()$Carbon.Emissions)
    formatC(x, format = "f", digits = 2, big.mark = ",")
  })
  output$average_renewable <- renderText({
    x <- mean(climate_data()$Renewable.Energy)
    formatC(x, format = "f", digits = 2, big.mark = ",")
  })
  
  
  
  # 🔍 Data table ------------------------------------------------------------
  
  output$table <- renderReactable({
    reactable(climate_data(),
              pagination = FALSE, compact = TRUE
    )
  })
  
  
  
  # 📊 Scatter plot ----------------------------------------------------------
  
  histogram <- reactive({
    req(nrow(climate_data()) > 0)
    
    color <- input$scatter_color
    
    data <- climate_data()
    
    p <- plot_ly(data, x = ~end_date, type = "histogram", xbins = list(start = 2000, end = 2060, size = 5), mode = "markers")
    
    if (color != "none") {
      p <- plot_ly(data,
                   x = ~end_date, color = as.formula(paste0("~", color)),
                   type = "histogram", mode = "markers"
      )
    }
    
    p <- p |> layout(showlegend = FALSE)
    
    return(p)
  })
  
  scatterplot <- reactive({
    req(nrow(climate_data()) > 0)
    
    color <- input$scatter_color
    
    data <- climate_data()
    
    p <- plot_ly(data, x = ~Renewable.Energy, y = ~Carbon.Emissions, type = "scatter", mode = "markers")
    
    if (color != "none") {
      p <- plot_ly(data,
                   x = ~Renewable.Energy, y = ~Carbon.Emissions, color = as.formula(paste0("~", color)),
                   type = "scatter", mode = "markers"
      )
    }
    
    p <- p |> layout(showlegend = FALSE)
    
    return(p)
  })
  
  output$histogram <- renderPlotly({
    histogram()
  })
  
  output$scatterplot <- renderPlotly({
    scatterplot()
  })
  
  observeEvent(input$interpret_histogram, {
    explain_plot(chat, histogram(), model = openai_model, .ctx = ctx)
  })
  
  observeEvent(input$interpret_scatter, {
    explain_plot(chat, scatterplot(), model = openai_model, .ctx = ctx)
  })
  
  
  
  # 📊 Ridge plot ------------------------------------------------------------
  
  # tip_perc <- reactive({
  #   req(nrow(climate_data()) > 0)
  
  #   df <- climate_data() |> mutate(percent = tip / total_bill)
  
  #   ggplot(df, aes_string(x = "percent", y = input$tip_perc_y, fill = input$tip_perc_y)) +
  #     geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.6) +
  #     scale_fill_viridis_d() +
  #     theme_ridges() +
  #     labs(x = "Percent", y = NULL, title = "Tip Percentages by Day") +
  #     theme(legend.position = "none")
  # })
  
  # output$tip_perc <- renderPlot({
  #   tip_perc()
  # })
  # 
  # observeEvent(input$interpret_ridge, {
  #   explain_plot(chat, tip_perc(), model = openai_model, .ctx = ctx)
  # })
  
  
  
  # ✨ Sidebot ✨ -------------------------------------------------------------
  
  append_output <- function(...) {
    txt <- paste0(...)
    shinychat::chat_append_message(
      "chat",
      list(role = "assistant", content = txt),
      chunk = TRUE,
      operation = "append",
      session = session
    )
  }
  
  #' Modifies the data presented in the data dashboard, based on the given SQL
  #' query, and also updates the title.
  #' @param query A DuckDB SQL query; must be a SELECT statement.
  #' @param title A title to display at the top of the data dashboard,
  #'   summarizing the intent of the SQL query.
  update_dashboard <- function(query, title) {
    append_output("\n```sql\n", query, "\n```\n\n")
    
    tryCatch(
      {
        # Try it to see if it errors; if so, the LLM will see the error
        dbGetQuery(conn, query)
      },
      error = function(err) {
        append_output("> Error: ", conditionMessage(err), "\n\n")
        stop(err)
      }
    )
    
    if (!is.null(query)) {
      current_query(query)
    }
    if (!is.null(title)) {
      current_title(title)
    }
  }
  
  #' Perform a SQL query on the data, and return the results as JSON.
  #' @param query A DuckDB SQL query; must be a SELECT statement.
  #' @return The results of the query as a JSON string.
  query <- function(query) {
    # Do this before query, in case it errors
    append_output("\n```sql\n", query, "\n```\n\n")
    
    tryCatch(
      {
        df <- dbGetQuery(conn, query)
      },
      error = function(e) {
        append_output("> Error: ", conditionMessage(e), "\n\n")
        stop(e)
      }
    )
    
    tbl_html <- df_to_html(df, maxrows = 5)
    append_output(tbl_html, "\n\n")
    
    df |> jsonlite::toJSON(auto_unbox = TRUE)
  }
  
  # Preload the conversation with the system prompt. These are instructions for
  # the chat model, and must not be shown to the end user.
  chat <- chat_openai(model = openai_model, system_prompt = system_prompt_str)
  chat$register_tool(tool(
    update_dashboard,
    "Modifies the data presented in the data dashboard, based on the given SQL query, and also updates the title.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement."),
    title = type_string("A title to display at the top of the data dashboard, summarizing the intent of the SQL query.")
  ))
  chat$register_tool(tool(
    query,
    "Perform a SQL query on the data, and return the results as JSON.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement.")
  ))
  
  # Prepopulate the chat UI with a welcome message that appears to be from the
  # chat model (but is actually hard-coded). This is just for the user, not for
  # the chat model to see.
  chat_append("chat", greeting)
  
  # Handle user input
  observeEvent(input$chat_user_input, {
    # Add user message to the chat history
    chat_append("chat", chat$stream_async(input$chat_user_input)) %...>% {
      # print(chat)
    }
  })
}

df_to_html <- function(df, maxrows = 5) {
  df_short <- if (nrow(df) > 10) head(df, maxrows) else df
  
  tbl_html <- capture.output(
    df_short |>
      xtable::xtable() |>
      print(type = "html", include.rownames = FALSE, html.table.attributes = NULL)
  ) |> paste(collapse = "\n")
  
  if (nrow(df_short) != nrow(df)) {
    rows_notice <- glue::glue("\n\n(Showing only the first {maxrows} rows out of {nrow(df)}.)\n")
  } else {
    rows_notice <- ""
  }
  
  paste0(tbl_html, "\n", rows_notice)
}

shinyApp(ui, server)
