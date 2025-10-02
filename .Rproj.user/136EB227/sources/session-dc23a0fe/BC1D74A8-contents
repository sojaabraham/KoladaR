#' Get Kolada API base URL
#'
#' Returns the base URL of Kolada API
#'
#' @return A character string containing the base URL of Kolada API
#'
#' @examples
#' kolada_base()
kolada_base <- function(){
  "https://api.kolada.se/v3"
}

#' GET request to Kolada API
#'
#' Sends a get request to a specified api endpoint and returns the parsed json to list.
#'
#' @param path Character string. API endpoint (e.g., "/kpi")
#' @param query List. Optional parameters to include in request
#' @param base Character string. Base URL of Kolada API
#'
#' @return A list containing the parsed JSON response
#'
#' @examples
#' kolada_get_list("/kpi")
#'
kolada_get_list <- function(path, query = list(), base = kolada_base()){
  url <- paste0(base, path)
  response <- httr::GET(url, query = query, httr::add_headers(Accept = "application/json"))
  httr::stop_for_status(response)
  text <- httr::content(response, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

#' Fetch paginated results from Kolada API
#'
#' Handles paginated API endpoints and returns all pages as a list.
#'
#' @param path Character string. API endpoint (e.g., "/kpi")
#' @param query List. Optional parameters to include in request
#'
#' @return List containing all pages of results.
#' @examples
#' kolada_get_paginated("/kpi")
#'
kolada_get_paginated <- function(path, query = list()){
  api_url = paste0(kolada_base(), path)
  api_data <- list()

  repeat{
    response <- httr::GET(api_url, query = query, httr::add_headers(Accept = "application/json"))
    httr::stop_for_status(response)
    content_text <- httr::content(response, as = "text", encoding = "UTF-8")
    obj <- jsonlite::fromJSON(content_text, simplifyVector = FALSE)

    api_data[[length(api_data) + 1]] <- obj

    #check if there's a next page
    if(is.null(obj$next_page) || obj$next_page == ""){
      break
    }

    #if next page exists, update the url for the next page
    api_url <- obj$next_page
  }
  #Return all the combined pages
  api_data
}

#' Retrieve KPI list from Kolada API
#'
#' Fetches all KPIs from the Kolada API, combines paginated results, and returns a tibble.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{KPI ID}
#'   \item{title}{KPI title}
#'   \item{description}{KPI description}
#'   \item{municipality_type}{Municipality type associated with the KPI}
#' }
#'
#' @examples
#' kpi <- get_kpi()
#' head(kpi)
get_kpi <- function(){
  pages <- kolada_get_paginated("/kpi")
  kpi_list <- lapply(pages, function(page) page$values)
  kpi_list <- do.call(c, kpi_list)

  #Convert to tibble
  tibble::tibble(
    id = vapply(kpi_list, function(x) x$id %||% NA_character_, character(1)),
    title = vapply(kpi_list, function(x) x$title %||% NA_character_, character(1)),
    description = vapply(kpi_list, function(x) x$description %||% NA_character_, character(1)),
    municipality_type = vapply(kpi_list, function(x) x$municipality_type %||% NA_character_, character(1))
  )
}



