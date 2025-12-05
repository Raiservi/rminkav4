#' Get information on a specific project seletec by name
#'
#' @export
#' @importFrom utils read.csv
#' @import httr dplyr jsonlite
#' @param id a single id for a Minka observation record
#' @return a list with full details on a given record
#' @examples \dontrun{
#' m_obs <- get_minka_project_byname(query="Biomarato 2025")
#' get_minka_obs_id(m_obs$id[1])
#' }
#'

library(httr)
library(jsonlite)
library(dplyr)


get_minka_project_byname <- function(query) {

  base_url <- "https://minka-sdg.org/"

  # Check access to Minka

  if (httr::http_error(base_url)) {
    message("Minka API is unavailable.")
    return(invisible(NULL))
  }

  query<- as.character( "Biomarato 2025 Girona")

  query_parsed<- str_replace_all(query, " ", "%20")

  q_path <- paste("projects/search.json?q=", as.character(query_parsed), sep = "")

  q_path

  df <- jsonlite::fromJSON(httr::content(httr::GET(base_url,path = q_path), as = "text"))

  View(df)

  return(df)
}


library(rminkav3)


