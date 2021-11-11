library(rvest)
library(tidyverse)

### which seasons?  Need a sequence of the 2 digit start year
season.start <- seq(11,19) ### 2020-2021 and 2021-2022 are not avaiable yet. 
### function to get urls for each season - they are standard on website. 
urlz <- sapply(season.start, function(i) paste0("https://www.cdc.gov/flu/about/burden/20", i, "-20", i+1, ".html"))
### fix urls
#urlz[7] <- "https://www.cdc.gov/flu/about/burden/2017-2018.htm"
urlz <- urlz[1:6]
### find xpath using css selector tool
xpathz <- '//*[contains(concat( " ", @class, " " ), concat( " ", "valign-text-top", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "table-responsive", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//td'
### 7th url, 17-18 season has different URL and different xpath
#xpathz[7] <- '//*[contains(concat( " ", @class, " " ), concat( " ", "table-responsive", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//tr[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//td | //*[contains(concat( " ", @class, " " ), concat( " ", "table-responsive", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "bg-gray-l2", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "text-center", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "table-responsive", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//strong'

######### ######### ######### ######### ######### 
######### ######### create function to scrape
######### ######### ######### ######### ######### 
scrape_flu_hosp <- function(url){

### read site and export text
  url %>%
    read_html(.) %>%
    html_nodes(xpath = xpathz) %>%
    html_text() -> df
  
  ### create df from it 
  df <- data.frame(matrix(df, nrow=7, ncol=9, byrow=T))[-1,]
  ### fix names
  names(df) <- c("age_group",
                   "symptomatic_illness_estimate",
                   "symptomatic_illness_ci",
                   "med_visit_estimate",
                   "med_visit_ci",
                   "hosp_estimate",
                   "hosp_ci",
                   "death_estimate",
                   "death_ci")
  
  return(df)
}

######### ######### ######### END SCRAPE FUNCTION ######### ######### ######### 

yo <- lapply(urlz, function(x) scrape_flu_hosp(x))

