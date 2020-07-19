library(rvest)

test <- xml2::read_html("https://www.charitynavigator.org/index.cfm?bay=search.summary&orgid=17475")
charity3 <- read_html("https://www.charitynavigator.org/index.cfm?keyword_list=&bay=search.results&EIN=&cgid=3&location=2&state=&city=&size=1&scopeid=2&overallrtg=3&cuid=40")
charity5 <- read_html("https://www.charitynavigator.org/index.cfm?keyword_list=&bay=search.results&EIN=&cgid=3&location=2&state=&city=&size=1&scopeid=2&cuid=40&overallrtg=4")


get_charity <- function(charities_page){
  name <- charities_page %>% html_node("table") %>% html_nodes("h3") %>% html_text() %>% str_squish()
  tagline <- charities_page %>% html_node("table") %>% html_nodes("p.tagline") %>% html_text() %>% str_squish()
  location <- charities_page %>% html_node("table") %>%
    html_nodes("p.category") %>% html_text() %>% str_squish() %>%
    str_remove_all("LOCATION: ") %>%
    str_remove_all(" CATEGORY:(.)+") %>%
    {.}
  grant_making <- charities_page %>% html_node("table") %>%
    html_nodes("p.category") %>% html_text() %>%
    str_detect("Grantmaking") %>%
    {.}
  ein <- charities_page %>% html_node("table") %>%
    html_nodes("tr") %>% html_attr("data-ein")

  return(tibble(name,tagline,location,ein,grant_making))
}

charities <- bind_rows(get_charity(charity3),get_charity(charity4))

charities_page <- charity4
write_csv(charities,"charities.csv")
