start <- function() {
  countries <- fetch_countries()[, c("@id","name:en")]

  score <- 0
  health <- 3
  repeat {
    random_country <- select_random_country(countries)

    country_sf <- fetch_country_geom_by_relation(random_country$"@id")
    plot(st_geometry(country_sf))

    guess <- readline(prompt="Write your guess: ")

    if(guess != random_country$"name:en") {
      if(health == 1) {
        break
      }
      health <- health - 1
      cat("WRONG! This was: ", random_country$"name:en", "\nYou lost 1 health. You have ", health, " health remaining.")
    } else {
      score <- 1 + score
      cat("CORRECT! You gain one point!\nCurrent Score: ", score)
    }
  }
  cat("Game done! \nYour Score:", score, "\nThe correct country would have been:",random_country$"name:en","\n")
}

select_random_country <- function(countries) {
  choice <- countries[sample.int(nrow(countries), 1), , drop = FALSE]

  return(choice)
}

run_app <- function() {
  shiny::shinyApp(ui, server)
}
