test_that("set_api_base_url stores value in option and env var", {
  old_option <- getOption("lab5.base_url")
  old_env <- Sys.getenv("LAB5_API_BASE_URL", unset = NA_character_)
  on.exit({
    if (is.null(old_option)) {
      options(lab5.base_url = NULL)
    } else {
      options(lab5.base_url = old_option)
    }
    if (is.na(old_env)) {
      Sys.unsetenv("LAB5_API_BASE_URL")
    } else {
      Sys.setenv(LAB5_API_BASE_URL = old_env)
    }
  }, add = TRUE)

  set_api_base_url("https://api.example.com")

  expect_equal(getOption("lab5.base_url"), "https://api.example.com")
  expect_equal(Sys.getenv("LAB5_API_BASE_URL"), "https://api.example.com")
})

test_that("get_api_base_url falls back to env var", {
  old_option <- getOption("lab5.base_url")
  old_env <- Sys.getenv("LAB5_API_BASE_URL", unset = NA_character_)
  on.exit({
    if (is.null(old_option)) {
      options(lab5.base_url = NULL)
    } else {
      options(lab5.base_url = old_option)
    }
    if (is.na(old_env)) {
      Sys.unsetenv("LAB5_API_BASE_URL")
    } else {
      Sys.setenv(LAB5_API_BASE_URL = old_env)
    }
  }, add = TRUE)

  options(lab5.base_url = NULL)
  Sys.setenv(LAB5_API_BASE_URL = "https://env.example.com")

  expect_equal(get_api_base_url(), "https://env.example.com")
})

test_that("fetch_api_resource reminds students to implement logic", {
  expect_error(
    fetch_api_resource("status"),
    "placeholder",
    fixed = FALSE
  )
})
