
# regulation 2019/1793

library(rvest)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)



readhtml <- read_html("https://www.legislation.gov.uk/eur/2019/1793")

# remove footnote letters in tables
xml_remove(html_nodes(readhtml, ".LegTableFootnoteRef"))
# remove footnotes from the tables
xml_remove(html_nodes(readhtml, "tfoot"))

# list of intended uses for removing
foodhead <- readhtml %>% html_nodes("table strong em") %>% html_text() %>% unique() %>% trimws() %>% .[grepl("\\(", .)]

# cleaning function
cleanup <- function(table) {
  table %>%
    # fill in country, hazard and check frequency for all rows
    mutate_at(vars(`Country of origin`, Hazard, `Frequency of identity and physical checks (%)`), list(~na_if(., ""))) %>%
    fill(c(`Country of origin`, Hazard, `Frequency of identity and physical checks (%)`)) %>%

    # create CN/TARIC column

    mutate(cn = str_split(`CN code`, "\r"),
           taric = str_split(`TARIC sub-division`, "\r")) %>%
    unnest(cols = c("cn", "taric")) %>%
    mutate(cn = str_remove_all(cn, ";")) %>%
    mutate(taric = str_replace_all(taric, ";", ",")) %>%
    mutate(`CN code / TARIC` = str_squish(paste(cn, taric))) %>%
    group_by(`Food and feed (intended use)`, `Country of origin`, Hazard) %>%
    mutate(`CN code / TARIC` = paste(`CN code / TARIC`, collapse = " ; ")) %>%
    select(-`CN code`, -`TARIC sub-division`, -cn, -taric) %>%
    unique() %>%
    ungroup() %>%

    # squish and get rid of extra space inside brackets
    mutate_all(., ~str_squish(.)) %>%
    mutate(`Food and feed (intended use)` = str_replace_all(`Food and feed (intended use)`, "\\( ", "\\(")) %>%
    mutate(`Food and feed (intended use)` = str_replace_all(`Food and feed (intended use)`, " \\)", "\\)")) %>%

    # separate product from intended use and fill intended use to all rows

    rowwise() %>%
    mutate(`Intended use` = paste(str_extract(`Food and feed (intended use)`, fixed(foodhead)), collapse = " ")) %>%
    mutate(`Intended use` = str_remove_all(`Intended use`, "NA |NA$")) %>%
    mutate_at(vars(`Intended use`), list(~na_if(., ""))) %>%
    ungroup() %>%
    fill(`Intended use`, .direction = "up") %>%
    mutate(`Intended use` = trimws(`Intended use`)) %>%
    mutate(Product = str_remove(`Food and feed (intended use)`, fixed(`Intended use`))) %>%
    mutate(`Intended use` = str_remove_all(`Intended use`, "[\\(\\)]")) %>%
    select(-`Food and feed (intended use)`)
}

# scrape table
a1table <- readhtml %>%
  html_nodes(xpath = '//*[@id="tgp1"]') %>%
  html_nodes("table") %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  as_tibble()

# check the right table has probably been ingested

assertthat::assert_that(sum(names(a1table) == c("Food and feed (intended use)",
                                                "CN code", "TARIC sub-division",
                                                "Country of origin", "Hazard",
                                                "Frequency of identity and physical checks (%)")) == 6,
                        msg = "Table header names have changed!")
assertthat::assert_that(nrow(a1table) > 50,
                        msg = "either a lot of products have been removed or you have the wrong table")

a1table %>%
  cleanup() %>%
  write_excel_csv(., "annexI.csv")

# annex II table
a2table <- readhtml %>%
  html_nodes(xpath = '//*[@id="tgp2"]') %>%
  html_nodes("table") %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  as_tibble()

assertthat::assert_that(sum(names(a2table) == c("Food and feed (intended use)",
                                                "CN code", "TARIC sub-division",
                                                "Country of origin", "Hazard",
                                                "Frequency of identity and physical checks (%)")) == 6,
                        msg = "Table header names have changed!")
assertthat::assert_that(nrow(a2table) > 50,
                        msg = "either a lot of products have been removed or you have the wrong table")

a2table %>%
  cleanup() %>%
  write_excel_csv(., "annexII.csv")


















