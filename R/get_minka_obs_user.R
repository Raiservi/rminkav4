#' Download observations for a user
#'
#' @description Get all the observations of a specific Minka user.
#' @param username username of the Minka user to fetch records
#' @param maxresults the maximum number of results to return
#' @return a list with full details on a given record
#' @examples \dontrun{
#'   m_obs <- get_minka_obs(query="Seabream")
#'   username<- m_obs$user_login[1]
#'   get_minka_obs_user(as.character(username))
#' }
#' @importFrom utils read.csv
#' @import httr jsonlite dplyr
#' @export


get_minka_obs_user <- function(username, maxresults = 1000){



  base_url <- "https://minka-sdg.org/"


            if (http_error(base_url)) {
              message("Minka API is unavailable.")
              return(invisible(NULL))
            }

  q_path <- paste0(username, ".csv")
  ping_path <- paste0(username, ".json")

  ping_query <- "&per_page=1&page=1"

  ping <- GET(base_url, path = paste0("observations/", ping_path), query = ping_query)

  total_res <- as.numeric(ping$headers$`x-total-entries`)

                      if(total_res == 0){
                        stop("Your search returned zero results. Perhaps your user does not exist.")
                      }

                      page_query <-"&per_page=200&page=1"
                      dat <-  GET(base_url, path = paste0("observations/", q_path), query = page_query)
                      data_out <- read.csv(textConnection(content(dat, as = "text")))

                    if(maxresults > 200){
                      for(i in 2:ceiling(total_res/200)){
                        page_query <- paste0("&per_page=200&page=", i)
                        dat <- GET(base_url, path = paste0("observations/", q_path), query = page_query)
                        data_out <- rbind(data_out, read.csv(textConnection(content(dat, as = "text"))))
                      }
                      }


                    if(maxresults < dim(data_out)[1]){
                      data_out <- data_out[1:maxresults,]
                    }

                    return(data_out)
}
