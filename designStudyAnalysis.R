library(curl)
library(rvest)
library(stringr)
library(dplyr)
library(rcrossref)
library(crminer)
library(profvis)

allPapers<-read.csv(file="AcceptedDesignStudyPapers-Title+DOI.csv",header=TRUE)

#note to self : it stopped at index 22 (continue from there tomorrow)

#download all of the design study paper from TVCG (2015 - PAST)
dl<-sapply(allPapers$DOI[84:nrow(allPapers)],function(x){
  links <- crm_links(x)

  articleNum<-str_extract(as.character(links$unspecified),"arnumber\\=([0-9]+)$") %>% gsub("arnumber=","",.)
  url<-paste0("https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=",articleNum)


  webpage <- read_html(url)
  pdfurl<-html_nodes(webpage,'iframe') %>% as.character() %>% str_extract("src\\=.*frameborder") %>% str_split(.,'\\"') %>% unlist()
  pdfurl<-pdfurl[2]
  
  # if(is.null(pdfurl)){
  #   #might be locked out, wait five minutes then try again
  #   profvis::pause(600) #pause so the website doesn't feel I am attacking it
  #   links <- crm_links(x)
  #   
  #   articleNum<-str_extract(as.character(links$unspecified),"arnumber\\=([0-9]+)$") %>% gsub("arnumber=","",.)
  #   url<-paste0("https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=",articleNum)
  #   
  #   
  #   webpage <- read_html(url)
  #   pdfurl<-html_nodes(webpage,'iframe') %>% as.character() %>% str_extract("src\\=.*frameborder") %>% str_split(.,'\\"') %>% unlist()
  #   pdfurl<-pdfurl[2]
  # }

  curl_download(pdfurl,destfile=paste0("designStudies/",gsub("\\/","\\.",x),".pdf"))

})
