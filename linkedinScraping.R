#webscraper

#libraries
library(tidyverse)
library(RSelenium)
library(netstat)


link <- "https://www.linkedin.com/jobs/search?keywords=&location=Turkey&locationId=&geoId=102105699&f_TPR=r86400&f_E=2&position=1&pageNum=0"

#selenium ----
#start a server
rs_driver_object <- rsDriver(browser = 'firefox',
                             verbose = FALSE,
                             version = 'latest',
                             chromever = NULL,
                             port = free_port())

#client object
remDr <- rs_driver_object$client

#navigate
remDr$navigate(link)

#scroll to the end of it ----
#job counts

scrollToBottom <- function(){
  job_counts   <- remDr$findElement(using = 'xpath',
                                  '/html/body/div[1]/div/main/div/h1/span[1]')
  job_counts   <- as.numeric(job_counts$getElementText())
  
  job_cards <- remDr$findElements(using = 'xpath',
                           '/html/body/div[1]/div/main/section/ul/li')
  card_amounts <- length(job_cards)
  
  more_jobs <- remDr$findElement(using = 'xpath', '//*[@id="main-content"]/section/button')
  t <- more_jobs$getElementText()
  while(job_counts > card_amounts){
    if(t == ""){ 
      remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
    } else {
      more_jobs$clickElement()
    }
    t <- more_jobs$getElementText()
    job_cards <- remDr$findElements(using = 'xpath',
                             '/html/body/div[1]/div/main/section/ul/li')
    card_amounts <- length(job_cards)
  }
}

scrollToBottom()

#getting links----
job_cards <- remDr$findElements(using = 'tag name', 'a') 
job_links <- lapply(job_cards, function(x) x$getElementAttribute('href')) %>%
  unlist()

#filtering only the job links
job_links <- as.data.frame(job_links)

job_links <- job_links %>% 
  filter(str_detect(string = job_links, pattern = "https://tr.linkedin.com/jobs/view"))

#creating an empty df for criteria
criteria <- character(0)
url <- character(0) 
explanation <- character(0)
title <- character(0)
company <- character(0)

kriterler <- tibble(criteria, url, explanation, title)

#functions for scraping ----
getCriteria <- function(link, kriterler){
  remDr$navigate(link)
  Sys.sleep(1) #wait for it to open properly
  #get criteria card
  try({
    job_criteria <- remDr$findElements(using = 'class name', 'description__job-criteria-list')
    job_criteria <- lapply(job_criteria, function(x) x$getElementText()) %>% unlist()
    
    show_more    <- remDr$findElement(using = 'xpath',
                                      '//*[@id="main-content"]/section[1]/div/div/section[1]/div/div/section/button[1]')
    show_more$clickElement()
    
    explanation <- remDr$findElements( using = "class name", 'description__text--rich')
    explanation <- lapply(explanation, function(x) x$getElementText()) %>% unlist()
    
    title <- remDr$findElement(using = 'tag name', 'h1')
    title <- title$getElementText() %>% unlist()
    
    holder_vec <- c(job_criteria, link, explanation, title)
    holder_df  <- as.data.frame(t(holder_vec))
    colnames(holder_df) <- c("criteria", "url", "explanation", "title")
    kriterler  <- rbind(kriterler, holder_df)}, silent = T)
  
  return(kriterler)
}

scraping <-function(url, kriterler) {
  i <- 1
  while(length(url) != i){
    link = url[i]
    kriterler <- getCriteria(link, kriterler)
    i <- i + 1
  }
  return(kriterler)
}
