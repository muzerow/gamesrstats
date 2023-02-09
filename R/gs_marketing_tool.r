#' Get Data from Steam Marketing Tool
#' @description Get Data from Steam Marketing Tool <https://games-stats.com/steam>
#' @importFrom dplyr %>% case_when mutate select
#' @importFrom rvest html_element html_table
#' @importFrom stringr str_detect str_replace_all str_split str_to_lower str_trim
#'
#' @param tags Steam tags vector
#' @param page Page number
#' @param platforms Platforms vector
#' @param vr VR filter
#' @param title Game title
#' @param developers Game developers vector
#' @param publishers Game publishers vector
#'
#' @export

gs_marketing_tool <- function(tags = NULL, page = NULL, platforms = NULL, vr = NULL,
                              title = NULL, developers = NULL, publishers = NULL) {
  base_url <- "https://games-stats.com/steam/?"
  query_url <- ""

  multiple_params <- list("tag" = tags, "page" = page, "platform" = platforms, "vr" = vr,
                          "title" = title ,"developer" = developers, "publisher" = publishers)

  for (i in names(multiple_params)) {
    if (!is.null(multiple_params[i][[1]])) {
      param <- names(multiple_params[i])
      value <- str_replace_all(str_replace_all(str_to_lower(multiple_params[i][[1]]), "[^-[:^punct:]]", ""), " ", "-")

      param_url <- paste0(param, "=", paste0(value, collapse = paste0("&", param, "=")))
      query_url <- paste(query_url, param_url, sep = "&")
    }
  }

  if (substr(query_url, 1, 1) == "&") { query_url <- substr(query_url, 2, nchar(query_url)) }

  url <- paste0(base_url, query_url)

  web_page <- read_html(url) %>%
    html_element("table") %>%
    html_table()

  colnames(web_page)[2] <- "description"

  Sys.setlocale("LC_TIME", "English")

  web_page %>%
    mutate(description = sub("\n.*", "", description),
           title = str_trim(str_split(Title, "\n", simplify = T)[,1]),
           publisher = str_trim(str_split(Title, "\n", simplify = T)[,7]),
           developer = str_trim(str_split(Title, "\n", simplify = T)[,11]),
           release_date = as.Date(sub("\n.*", "", Release), "%b %d, %Y"),
           price = as.numeric(gsub("\\$", "", sub("\n.*", "", Price))),
           followers = as.numeric(gsub("\\D+", "", Followers)),
           reviews = as.numeric(gsub("\\D+", "", Reviews)),
           score = as.numeric(sub("/.*", "", Score)),
           net_revenue = case_when(str_detect(`Net Revenue`, "billion") ~ as.numeric(gsub("\\D+", "", `Net Revenue`)) * 1000000000,
                                   str_detect(`Net Revenue`, "million") ~ as.numeric(gsub("\\D+", "", `Net Revenue`)) * 1000000,
                                   !is.na(`Net Revenue`) ~ as.numeric(gsub("\\D+", "", `Net Revenue`)))) %>%
    select(id = `#`, description, title, publisher, developer, release_date,
           price, tags = Tags, followers, reviews, score, net_revenue)
}
