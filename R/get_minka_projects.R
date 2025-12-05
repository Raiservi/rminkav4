#' Download observations or info from a project
#'
#' Retrieve observations from a particular Minka project. This function can be used to get either observations or information from a project by project name or ID.
#'
#' @param grpid Name of the group as an Minka slug or group ID.
#' @param type Character, either "observations" or "info". "observations" returns all observations, and "info" returns project details similar to what you can find on a project's page.
#' @param raw Logical. If TRUE and searching for project info, returns the raw output of parsed JSON for that project. Otherwise just some basic information is returned as a list.
#' @details A Minka slug is usually the project name as a single string with words separated by hyphens. For instance, the project "biomarato-2022-girona" has a slug of "world-oceans-week-2022", which you can find by searching projects on Minka and looking at the \href{https://minka-sdg.org/}{project's page's URL}.
#'
#' @examples \dontrun{
#'  get_minka_obs_project(8, type = "observations")
#'  get_minka_obs_project("urbamarbio", type="info",raw=FALSE)
#'}
#'@importFrom utils read.csv
#' @import httr jsonlite dplyr
#' @export

library(tidyverse)

library(httr2)

library(jsonlite)


#Per decomentar un cop actiu
# get_minka_obs_project <- function(grpid = NULL, type = c("info","observations") , raw = FALSE){


  base_url <- "http://www.minka-sdg.org"

  # check access to  Minka

            if (httr::http_error(base_url))
             { message("Minka API is unavailable.")
              return(invisible(NULL))}

  url <- paste0(base_url, "/projects", ".json")
  xx <- jsonlite::fromJSON(httr::content(httr::GET(url), as = "text"))
  recs <- count(xx)
  recs <- as.integer(recs[[1]])

  class(recs)
  recs
  names(xx)

            dat <- NULL
            if(is.null(recs)){
              (return(dat))
            message(paste(recs,"records\n"))
            }

            per_page <- 200

            recs %/% per_page # regitres per pagina
    #---------------------------------------------------------------------------------------
            if (recs %% per_page == 0) {    # Si no hi han registres
              loopval <- recs %/% per_page   #loopval =0
            }
    #----------------------------------------------------------------------------------------
            if (recs >= 200000) {
              warning(
                "Number of projects greater than current API limit.\nReturning the first 10000.\n"
              )
              loopval <- 200000 / per_page
    #--------------------------------------------------------------------------------------------
            } else {
              loopval <- (recs %/% per_page) + 1
            }
            class(loopval)
       #--------------------Definim el bucle------------------------------------------------

            obs_list <- vector("list", loopval) # creem el vector  de cada loop amb unn index loopval

            for (i in 1:loopval) {
              url1 <-
                paste0(
                  base_url, "/projects",
                  ".json?page=", i,
                  "&per_page=", per_page
                )

  #----------------------------aixo es pels missatges-------------------------------------

              if (i == 1) {
                message(paste0("Getting records 0-", per_page))
              }
              if (i > 1) {
                message(paste0("Getting records up to ", i * per_page))
              }
#-----------------------------------------------------------------------

              obs_list[[i]] <-
                fromJSON(httr::content(httr::GET(url1), as = "text"), flatten = TRUE)
            }

            View(obs_list)

            # aqui acaba le bucle

            message("Done.\n")

            if (length(obs_list[[loopval]]) == 0) {
              obs_list[[i]] <- NULL
            }
            project_obs <- do.call("bind_rows", obs_list)
            if (recs != nrow(project_obs)) {
              message("Note: mismatch between number of observations reported and returned by the API.")
            }





                      {
            output <- list()
            output[["title"]] <- xx$title
            output[["description"]] <- xx$description
            output[["slug"]] <- xx$slug
            output[["created_at"]] <- xx$created_at
            output[["id"]] <- xx$id
            output[["location"]] <- c(as.numeric(xx$lat),as.numeric(xx$long))
            output[["place_id"]] <- xx$place_id
            output[["taxa_number"]] <- xx$observed_taxa_count
            output[["taxa_count"]] <- xx$project_observations_count
                        if(raw){
                          output[["raw"]] <- xx
                        }
                        return(xx)



  } else if (argstring == "obs") {



    return(project_obs)

  } # tancament de l if


#    }  tancament de la funcio


