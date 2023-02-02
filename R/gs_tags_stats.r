#' Get Data from Steam Tags Stats
#' @description Get Data from Steam Tags Stats <https://games-stats.com/steam/tags>
#' @importFrom dplyr %>% case_when mutate select
#' @importFrom rvest html_element read_html html_table
#' @importFrom stringr str_detect
#'
#' @export

gs_tags_stats <- function() {
  web_page <- read_html("https://games-stats.com/steam/tags") %>%
    html_element("table") %>%
    html_table()

  web_page %>%
    mutate(games_count = as.numeric(gsub("\\D+", "", `Games Count`)),
           across(c(`Revenue Total`, `Revenue Average`, `Revenue Median`),
                  ~ case_when(str_detect(., "billion") ~ as.numeric(gsub("\\D+", "", .)) * 1000000000,
                              str_detect(., "million") ~ as.numeric(gsub("\\D+", "", .)) * 1000000,
                              !is.na(.) ~ as.numeric(gsub("\\D+", "", .)))),
           across(c(`$0-$5k`, `$5k-$25k`, `$25k-$100k`, `$100k-$1M`, `$1M+`),
                  ~ as.numeric(gsub("\\D+", "", .)) / 100)) %>%
    select(id = `#`, tag = Tag, games_count, revenue_total = `Revenue Total`,
           revenue_average = `Revenue Average`, revenue_median = `Revenue Median`,
           `$0-$5k`, `$5k-$25k`, `$25k-$100k`, `$100k-$1M`, `$1M+`)
}
