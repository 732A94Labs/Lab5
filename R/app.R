# app.R
library(shiny)
library(sf)
library(bslib)
library(shinyjs)

ui <- page_sidebar(
  title = "Country Guessr",
  sidebar = sidebar(
    useShinyjs(),  # activate shinyjs in the UI
    div(class = "text-sm text-muted",
        "Guess the country from its outline. You have 3 health."
    ),
    h3(textOutput("score")),
    h3(textOutput("health")),
    textInput("guess", label = NULL, placeholder = "Type country name…"),
    actionButton("guess_btn", "Guess!", class = "btn-primary"),
    actionButton("next_btn", "Next Country"),
    actionButton("reset_btn", "New Game", class = "ms-2"),
    br(), br(),
    textOutput("feedback")
  ),
  plotOutput("countryPlot", height = 500),
  tags$script(HTML("
  $(document).on('keyup', function(e) {
    if (e.key === 'Enter' && $('#guess').is(':focus')) {
      $('#guess_btn').click();
    }
  });
  "))
)

server <- function(input, output, session) {
  # Load once, non-reactive (plain data)
  countries_df <- fetch_countries()[, c("@id", "name:en")]

  # Game state (no countries here!)
  rv <- reactiveValues(
    current = NULL,
    geom = NULL,
    score = 0L,
    health = 3L,
    game_over = FALSE
  )

  pick_new_country <- function() {
    choice <- countries_df[sample.int(nrow(countries_df), 1), , drop = FALSE]
    rel_id <- choice[["@id"]]

    rv$current <- choice
    rv$geom <- fetch_country_geom_by_relation(rel_id)

    output$feedback <- renderText("")
    updateTextInput(session, "guess", value = "")
    shinyjs::enable("guess_btn")
    shinyjs::enable("guess")
  }


  start_game <- function() {
    rv$score <- 0L
    rv$health <- 3L
    rv$game_over <- FALSE
    pick_new_country()
  }

  start_game()

  # Outputs
  output$score  <- renderText(sprintf("Score: %d", rv$score))
  output$health <- renderText(sprintf("Health: %d", rv$health))

  output$countryPlot <- renderPlot({
    req(rv$geom)
    plot(st_geometry(rv$geom), axes = FALSE, main = "")
    box(lwd = 1)
  })

  # Next Country
  observeEvent(input$next_btn, {
    if (!rv$game_over) {
      pick_new_country()
    } else {
      start_game()
    }
  })

  # Guess handling
  observeEvent(input$guess_btn, {
  req(!rv$game_over, rv$current)

  user_guess <- trimws(tolower(input$guess))
  answer <- tolower(rv$current[["name:en"]])

  shinyjs::disable("guess_btn")
  shinyjs::disable("guess")

  if (identical(user_guess, "")) {
    output$feedback <- renderText("Type a guess before clicking Guess!")
    shinyjs::enable("guess_btn")
    shinyjs::enable("guess")
    return(invisible())
  }

  is_correct <- identical(user_guess, answer)

  if (is_correct) {
    rv$score <- rv$score + 1L
    output$feedback <- renderText(sprintf(
      "✅ CORRECT! It was %s. Loading next country…",
      rv$current[["name:en"]]
    ))

    shinyjs::delay(5000, pick_new_country())
  } else {
    rv$health <- rv$health - 1L
    if (rv$health <= 0L) {
      rv$game_over <- TRUE
      output$feedback <- renderText(sprintf(
        "❌ WRONG! It was %s. You lost 1 health and have 0 left. Game over! Final score: %d.",
        rv$current[["name:en"]], rv$score
      ))
    } else {
      output$feedback <- renderText(sprintf(
        "❌ WRONG! It was %s. You lost 1 health. %d health remaining. Loading next country…",
        rv$current[["name:en"]], rv$health
      ))
      shinyjs::delay(5000, pick_new_country())
    }
  }
})

  observeEvent(input$reset_btn, start_game)
}

shinyApp(ui, server)
