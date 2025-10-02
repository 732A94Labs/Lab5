# app.R
library(shiny)
library(sf)          # for plotting sf objects
# library(ggplot2)   # optional if you prefer ggplot, not needed for base plot()
library(bslib)

# Assumes you already have:
# fetch_countries(): data.frame with columns "@id" and "name:en"
# fetch_country_geom_by_relation(relation_id): returns sf with geometry for that country

ui <- page_sidebar(
  title = "Country Guessr",
  sidebar = sidebar(
    div(class = "text-sm text-muted",
        "Guess the country from its outline. You have 3 health."
    ),
    h3(textOutput("score")),
    h3(textOutput("health")),
    textInput("guess", label = NULL, placeholder = "Type country name…"),
    actionButton("guess_btn", "Guess!", class = "btn-primary"),
    actionButton("reset_btn", "New Game", class = "ms-2"),
    br(), br(),
    textOutput("feedback")
  ),
  plotOutput("countryPlot", height = 500)
)

server <- function(input, output, session) {

  # ---------- Game state ----------
  rv <- reactiveValues(
    countries = NULL,         # full pool
    current = NULL,           # one-row data.frame for the current country
    geom = NULL,              # sf geometry of current country
    score = 0L,
    health = 3L,
    game_over = FALSE
  )

  # Helper to pick a new country and load its geometry
  pick_new_country <- function() {
    req(rv$countries)
    choice <- rv$countries[sample.int(nrow(rv$countries), 1), , drop = FALSE]
    rv$current <- choice
    # Access column with a colon safely using [[ ]]
    rel_id <- rv$current[["@id"]]
    rv$geom <- fetch_country_geom_by_relation(rel_id)
  }

  # Start (or restart) a game
  start_game <- function() {
    rv$countries <- fetch_countries()[, c("@id", "name:en")]
    rv$score <- 0L
    rv$health <- 3L
    rv$game_over <- FALSE
    output$feedback <- renderText("")
    pick_new_country()
    updateTextInput(session, "guess", value = "")
  }

  # Initialize game on app load
  start_game()

  # ---------- Outputs ----------
  output$score  <- renderText(sprintf("Score: %d", rv$score))
  output$health <- renderText(sprintf("Health: %d", rv$health))

  output$countryPlot <- renderPlot({
    req(rv$geom, !rv$game_over)
    plot(st_geometry(rv$geom), axes = FALSE, main = "")
    box(lwd = 1)
  })

  # ---------- Guess handling ----------
  observeEvent(input$guess_btn, {
    req(!rv$game_over, rv$current)

    user_guess <- trimws(tolower(input$guess))
    answer     <- tolower(rv$current[["name:en"]])

    if (identical(user_guess, "") ) {
      output$feedback <- renderText("Type a guess before clicking Guess!")
      return(invisible())
    }

    if (user_guess == answer) {
      rv$score <- rv$score + 1L
      output$feedback <- renderText(sprintf("✅ CORRECT! It was %s. +1 point.", rv$current[["name:en"]]))
      pick_new_country()
      updateTextInput(session, "guess", value = "")
    } else {
      rv$health <- rv$health - 1L
      if (rv$health <= 0L) {
        rv$game_over <- TRUE
        output$feedback <- renderText(sprintf(
          "❌ WRONG. It was %s. Game over! Final score: %d.",
          rv$current[["name:en"]], rv$score
        ))
      } else {
        output$feedback <- renderText(sprintf(
          "❌ WRONG. It was %s. You lost 1 health. %d health remaining.",
          rv$current[["name:en"]], rv$health
        ))
        pick_new_country()
        updateTextInput(session, "guess", value = "")
      }
    }
  })

  # ---------- Reset ----------
  observeEvent(input$reset_btn, start_game)
}

shinyApp(ui, server)
