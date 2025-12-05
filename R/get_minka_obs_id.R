#' Get information on a specific observation
#'
#' @export
#' @importFrom utils read.csv
#' @import httr dplyr jsonlite
#' @param id a single id for a Minka observation record
#' @return a list with full details on a given record
#' @examples \dontrun{
#' m_obs <- get_minka_obs(query="Boops boops")
#' get_minka_obs_id(m_obs$id[1])
#' }
#'

 get_minka_obs_id <- function(id) {

  base_url <- "https://minka-sdg.org/"

  # Check access to Minka

  if (httr::http_error(base_url)) {
                                    message("Minka API is unavailable.")
                                    return(invisible(NULL))
                                  }

  q_path <- paste("observations/", as.character(id), ".json", sep = "")

  df <- jsonlite::fromJSON(httr::content(httr::GET(base_url,path = q_path), as = "text"))

  return(df)
}


