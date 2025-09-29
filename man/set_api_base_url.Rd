% Generated for Lab5 package scaffold
\name{set_api_base_url}
\alias{set_api_base_url}
\title{Configure the API base URL}
\description{Stores the API base URL in an option and environment variable so the rest of the package can reuse it.}
\usage{set_api_base_url(base_url)}
\arguments{
  \item{base_url}{Non-empty character scalar with the base URL.}
}
\value{Returns the configured URL invisibly.}
\examples{
set_api_base_url("https://api.example.com")
}
