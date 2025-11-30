## Some notes about this example:
## - This example uses the ellmer package to define a data model and
##   perform structured chat with the model.
## - This example requires an Anthrophic API key. See
##   https://platform.claude.com/docs/en/home for more information.
## - Make sure to set the environment variable: ANTHROPIC_API_KEY=[your_key]
## - Note that Anthrophic unified its branding into Claude (September 2025).
##   See more here: https://platform.claude.com/docs/en/release-notes/overview#september-16-2025
## - The chat is divided into two tasks, because we can't call a tool
##   from within a structured chat. The first task is a regular chat
##   that calls the tool to get the weather. The second task is a
##   structured chat that generates the activities based on the weather
##   and baby age.
## - We're using an ExtendedTask to avoid blocking the session and
##   we start a fresh chat session each time. For a feedback loop,
##   we would use a persistent chat session.
## - Looking for more backround information about ellmer?
##   See https://ellmer.tidyverse.org
## - Keep an eye on the Shiny blog for "The Shiny Side of LLMs" series!
##   Accompanying repo: https://github.com/hypebright/the-shiny-side-of-llms

library(shiny) # at least version 1.8.1
library(bslib) # at least version 0.7.0
library(ellmer) # at least version 0.4.0
library(httr2)
library(dplyr)

system_prompt <- "
You are an assistant that performs two tasks:

1. Retrieve the current weather for {{ city }}. Return only the JSON results, nothing else.  
2. Generate a list of 10 baby activities appropriate for a baby that's {{ age }} months old and the current weather. Each activity must include a 'fun score', estimated duration in minutes, and a short description.

Always respond strictly in the JSON format defined by the provided data model.
"

data_model <- type_object(
  city = type_string(
    description = "The city for which the weather was checked."
  ),
  baby_age_months = type_integer(
    description = "The age of the baby in months."
  ),
  temperature_c = type_number(description = "Current temperature in Celsius."),
  condition = type_string(
    description = "Current weather condition (e.g., sunny, rainy, cloudy, snowy, windy)."
  ),
  activities = type_array(
    description = "List of 10 suggested activities for the baby based on the weather and age.",
    type_object(
      name = type_string(description = "Short title for the activity."),
      fun_score = type_integer(
        description = "Score from 1–10 indicating how fun the activity is likely to be."
      ),
      duration_minutes = type_integer(
        description = "Estimated activity length in minutes."
      ),
      description = type_string(
        description = "One-sentence description of the activity."
      )
    )
  )
)

#' Get the current weather for a given city using Open-Meteo API
#'
#' @param city The name of the city
#' @return A tibble with city, temperature in Celsius, windspeed, and weather code
get_current_weather <- function(city) {
  # Geocoding API to get latitude/longitude from city name
  geo_resp <- request("https://geocoding-api.open-meteo.com/v1/search") %>%
    req_url_query(name = city, count = 1) %>%
    req_perform() %>%
    resp_body_json()

  if (is.null(geo_resp$results)) {
    stop("City not found: ", city)
  }

  lat <- geo_resp$results[[1]]$latitude
  lon <- geo_resp$results[[1]]$longitude

  # Weather API for current weather at that location
  weather_resp <- request("https://api.open-meteo.com/v1/forecast") %>%
    req_url_query(latitude = lat, longitude = lon, current_weather = "true") %>%
    req_perform() %>%
    resp_body_json()

  current <- weather_resp$current_weather

  return(
    tibble(
      city = city,
      temperature_c = current$temperature,
      windspeed = current$windspeed,
      weather_code = current$weathercode
    )
  )
}

get_current_weather <- tool(
  get_current_weather,
  "Returns the current weather for a given city",
  city = type_string(
    "The name of the city for which to get the weather",
    required = TRUE
  )
)

ui <- page_fillable(
  ## Options
  ## 1. Bootswatch theme
  theme = bs_theme(bootswatch = "sketchy"),
  ## 2. Custom CSS
  tags$style(HTML(
    "
    .bounce {
      animation: bounce 2s infinite;
    }
    @keyframes bounce {
      0%, 100% {
        transform: translateY(0);
      }
      50% {
        transform: translateY(-20px);
      }
    }
  "
  )),
  ## Layout
  layout_sidebar(
    ## Sidebar content
    sidebar = sidebar(
      width = 400,
      # Open sidebar on mobile devices and show above content
      open = list(mobile = "always-above"),
      textInput(
        "city",
        "City",
        value = "Rotterdam",
        placeholder = "Enter city name"
      ),
      numericInput(
        "age",
        "Baby Age (months)",
        value = 11,
        min = 0,
        max = 24,
        step = 1
      ),
      input_task_button(
        id = "submit",
        label = shiny::tagList(
          icon("baby"),
          "Get Baby Activities"
        ),
        label_busy = "Activities loading...",
        type = "default"
      )
    ),
    ## Main content
    textOutput("time"),
    uiOutput("results", height = "100%")
  )
)

server <- function(input, output, session) {
  output$time <- renderText({
    invalidateLater(1000, session)
    format(Sys.time(), "%H:%M:%S %p")
  })

  chat_task <- ExtendedTask$new(function(
    full_system_prompt,
    data_model
  ) {
    # We're using an Extended Task to avoid blocking the session and
    # we start a fresh chat session each time.
    # For a feedback loop, we would use a persistent chat session.
    chat <- chat("claude/claude-sonnet-4-5", system_prompt = full_system_prompt)

    # Register the tool with the chat
    chat$register_tool(get_current_weather)

    # Start conversation with the chat
    # Task 1: regular chat to extract meta-data
    chat_res <- chat$chat_async(
      "Execute Task 1 (get weather)"
    )

    chat_res$then(function(res) {
      # Print the response from Task 1
      cat("Response from Task 1:\n")
      cat(res, "\n\n")

      # Execute next task
      # Task 2: structured chat to further analyse the slides
      chat$chat_structured_async(
        "Execute Task 2 (activity suggestions)",
        type = data_model
      )
    })
  }) |>
    bind_task_button("submit")

  observe({
    req(input$city)
    req(input$age)

    full_system_prompt <- interpolate(
      system_prompt,
      city = input$city,
      age = input$age
    )

    chat_task$invoke(
      full_system_prompt = full_system_prompt,
      data_model = data_model
    )
  }) |>
    bindEvent(input$submit)

  output$results <- renderUI({
    if (chat_task$status() == "running") {
      div(
        class = "text-center d-flex flex-column justify-content-center align-items-center",
        style = "height: 100%;",
        icon(
          "baby-carriage",
          style = "font-size: 6em;",
          class = "bounce"
        ),
        br(),
        # random messages while waiting
        sample(
          c(
            "Thinking about fun activities...",
            "Checking the weather...",
            "Finding the best activities for your baby...",
            "Almost there..."
          ),
          1
        )
      )
    } else if (chat_task$status() == "success") {
      tagList(
        # value boxes with summary info
        layout_column_wrap(
          fill = FALSE,
          ### Value boxes for metrics
          value_box(
            title = "Top activity",
            value = textOutput("top_activity"),
            showcase = icon("trophy"),
            theme = "info"
          ),
          value_box(
            title = "Average fun score",
            value = textOutput("avg_fun_score"),
            showcase = icon("face-smile"),
            theme = "info"
          ),
          value_box(
            title = "Average duration (min)",
            value = textOutput("avg_duration"),
            showcase = icon("clock"),
            theme = "info"
          )
        ),
        # table with
        tableOutput("activities_table")
      )
    }
  })

  activities_result <- reactive({
    req(chat_task$result())
    chat_task$result()$activities
  })

  output$top_activity <- renderText({
    req(activities_result())

    top_activity <- activities_result() |>
      slice_max(fun_score, with_ties = FALSE)

    top_activity$name
  })

  output$avg_fun_score <- renderText({
    req(activities_result())

    avg_fun_score <- activities_result() |>
      summarise(avg = mean(fun_score)) |>
      pull(avg)

    round(avg_fun_score, 1)
  })

  output$avg_duration <- renderText({
    req(activities_result())

    avg_duration <- activities_result() |>
      summarise(avg = mean(duration_minutes)) |>
      pull(avg)

    round(avg_duration, 1)
  })

  output$activities_table <- renderTable({
    req(activities_result())

    activities_df <- activities_result() |>
      arrange(desc(fun_score))

    activities_df
  })
}

shinyApp(ui, server)
