# This code will scrape Yelp data (Name, location, review, rating and date) 
# using package rvest
# Credit: https://stat4701.github.io/edav/2015/04/02/rvest_tutorial/
# Credit: https://stackoverflow.com/questions/35247033/using-rvest-to-extract-links

library(tidyverse)
library(lubridate)
library(rvest)

url <- "Insert URL here"

# Scrape the reviewer name
scrape_review_name <- function(link) {
  name <- link %>%
    read_html() %>%
    html_nodes(".review-sidebar") %>%
    html_nodes(".review-sidebar-content") %>%
    html_nodes(".user-passport-info") %>%
    html_nodes(".user-name") %>%
    html_text
 return(name)
}

# Scrape the reviewer location
scrape_review_location <- function(link) {
  location <- link %>%
    read_html() %>%
    html_nodes(".review-sidebar")
    html_nodes(".review-sidebar-content") %>%
    html_nodes(".user-passport-info") %>%
    html_nodes(".user-location responsive-hidden-small") %>%
      \\d{1,2}
    html_text
  return(location)
}

# Scrape the review text
scrape_review_text <- function(link) {                                                                            
  text <- link %>%                                                                
    read_html() %>%                                                                 
    html_nodes(".review-content") %>% # content of reviews   
    html_text() %>%                                                                 
    str_remove("\\d{1,2}/\\d{1,2}/\\d{4}") %>% # clean strings                
    str_remove_all("\n") %>%                                                       
    str_remove("\\d{1,2} check-in") %>%                                           
    str_trim()                                                                      
  return(text)                                                                    
}          

# Scrape the review ratings
scrape_review_rating <- function(link) {                                                          
  rating <- link %>%                                                              
    read_html() %>%     
    html_nodes(".review-content") %>% 
    html_nodes(".rating-large") %>%                                                 
    html_attr("title") %>%                                                          
    str_extract("\\d.\\d") %>%                                                  
    as.numeric()                                                                    
  
  return(rating)                                                                  
}                                                                               

# Scrape the review date    
scrape_review_date <- function(link) {                                                                                            
  review_dates <- link %>%                                                        
    read_html() %>%                                                                 
    html_nodes(".review-content") %>%                                               
    html_text()  %>%                                                                
    str_extract("\\d{1,2}/\\d{1,2}/\\d{4}") %>%                               
    mdy()                                                                           
  
  return(review_dates)                                                            
}                                                                               

# apply functions  
review_name <- scrape_review_name(url)
review_text <- scrape_review_text(url)                                          
review_rating <- scrape_review_rating(url)                                      
review_date <- scrape_review_date(url)     
review_location <- scrape_review_location(url)

# create a data frame                                        
df <- tibble(name = review_name[1],
             date = review_date[1],                                             
             rating = review_rating[1],                                                      
             text = review_text[1])                                                          

for (i in 2:length(review_text)) {                                                                       
  df[i, "name"] <- review_name[i]
  df[i, "date"] <- review_date[i]                                                 
  df[i, "rating"] <- review_rating[i]                                             
  df[i, "text"] <- review_text[i]                                                 
  
}
