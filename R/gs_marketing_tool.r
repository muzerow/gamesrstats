#' Get Data from Steam Marketing Tool
#' @description Get Data from Steam Marketing Tool <https://games-stats.com/steam>
#' @importFrom dplyr %>% case_when mutate select
#' @importFrom rvest html_element read_html html_table
#' @importFrom stringr str_detect str_glue str_split str_trim
#'
#' @param tags Steam tags vector
#' @param page Page number
#' @param vr VR filter
#'
#' @export

gs_marketing_tool <- function(tags, page, vr = "all") {
  tags_url <- paste0("tag=", tags, collapse = "&")

  web_page <- read_html(str_glue("https://games-stats.com/steam/?{tags_url}&page={page}&vr={vr}")) %>%
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
