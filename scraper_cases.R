fluscrape_upd <- function(urlz, xpathz, df, seasonz){

  urlz2 = urlz
  xpathz2 = xpathz
  
  urlz2 %>%
    read_html(.) %>%
    html_nodes(xpath = xpathz2) %>%
    html_table() -> df
  df <- data.frame(matrix(unlist(df), nrow=8, ncol=9, byrow=F))
  df <- df[-1,]
  names(df) <- c("age_group",
                   "symptomatic_illness_estimate",
                   "symptomatic_illness_ci",
                   "med_visit_estimate",
                   "med_visit_ci",
                   "hosp_estimate",
                   "hosp_ci",
                   "death_estimate",
                   "death_ci")
  df <- df[-1,]
  
  df %>%
    map_dfr(~gsub(",", "", .)) %>%
    map_dfr(~gsub("\\(", "", .)) %>%
    map_dfr(~gsub("\\)", "", .)) -> df
  df %>%
    separate(., col=symptomatic_illness_ci, into=c("symptomatic_illness_ci_lower", "symptomatic_illness_ci_upper")) %>%
    separate(., col=med_visit_ci, into=c("med_visit_ci_lower", "med_visit_ci_upper")) %>%
    separate(., col=hosp_ci, into=c("hosp_ci_lower", "hosp_ci_upper")) %>%
    separate(., col=death_ci, into=c("death_ci_lower", "death_ci_upper")) -> df
  df$season <- seasonz
  return(df)
}