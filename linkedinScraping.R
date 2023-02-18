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