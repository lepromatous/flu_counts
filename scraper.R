library(rvest)
library(tidyverse)
library(stringi)
library(tidyr)

### which seasons?  Need a sequence of the 2 digit start year
season.start <- seq(10,19) ### 2020-2021 and 2021-2022 are not avaiable yet. 
### function to get urls for each season - they are standard on website. 
urlz <- sapply(season.start, function(i) paste0("https://www.cdc.gov/flu/about/burden/20", i, "-20", i+1, ".html"))
### fix urls
#urlz[7] <- "https://www.cdc.gov/flu/about/burden/2017-2018.htm"
urlz <- urlz[1:7]
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

### scrape 2010-2016 and clean
yo <- lapply(urlz, function(x) scrape_flu_hosp(x))

test1 <- data.table::rbindlist(yo)
test1$season <- rep(paste0(seq(2010,2016), "-", seq(2011,2017)), each=6)
test1$death_estimate<-gsub("—",NA, test1$death_estimate)
test1$death_ci<-gsub("—",NA, test1$death_ci)

test1 %>%
  map_dfr(~gsub(",", "", .)) %>%
  map_dfr(~gsub("\\(", "", .)) %>%
  map_dfr(~gsub("\\)", "", .)) -> test1

test1 %>%
  separate(., col=symptomatic_illness_ci, into=c("symptomatic_illness_ci_lower", "symptomatic_illness_ci_upper")) %>%
  separate(., col=med_visit_ci, into=c("med_visit_ci_lower", "med_visit_ci_upper")) %>%
  separate(., col=hosp_ci, into=c("hosp_ci_lower", "hosp_ci_upper")) %>%
  separate(., col=death_ci, into=c("death_ci_lower", "death_ci_upper")) -> test1

###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### done through 2016-2017  
###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 



###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### begin 2017-2018, 
###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
###### 2017-2018 complete



###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## 2018-2019 Start
###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
xpath17 <- '/html/body/div[6]/main/div[3]/div/div[4]/div[3]/div/div[1]/table/tbody'

'https://www.cdc.gov/flu/about/burden/2017-2018.htm' %>%
  read_html(.) %>%
  html_nodes(xpath = xpath17) %>%
  html_table() -> df
df18 <- data.frame(matrix(unlist(df), nrow=8, ncol=9, byrow=F))
df18 <- df18[-1,]
names(df18) <- c("age_group",
                 "symptomatic_illness_estimate",
                 "symptomatic_illness_ci",
                 "med_visit_estimate",
                 "med_visit_ci",
                 "hosp_estimate",
                 "hosp_ci",
                 "death_estimate",
                 "death_ci")
df18 <- df18[-1,]

df18 %>%
  map_dfr(~gsub(",", "", .)) %>%
  map_dfr(~gsub("\\(", "", .)) %>%
  map_dfr(~gsub("\\)", "", .)) -> df18
df18 %>%
  separate(., col=symptomatic_illness_ci, into=c("symptomatic_illness_ci_lower", "symptomatic_illness_ci_upper")) %>%
  separate(., col=med_visit_ci, into=c("med_visit_ci_lower", "med_visit_ci_upper")) %>%
  separate(., col=hosp_ci, into=c("hosp_ci_lower", "hosp_ci_upper")) %>%
  separate(., col=death_ci, into=c("death_ci_lower", "death_ci_upper")) -> df18
df17$season <- "2018-2019"

###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## 2018-2019 Complete
###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 






###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## 2019-2020 Start
###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## 22019-2020 Complete
###### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 