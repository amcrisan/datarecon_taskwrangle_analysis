#Script that scrapes IEEE VIS conference website and looks for design studies and application papers

library(rvest)
library(dplyr)
library(stringr)
library(magrittr)

#trying to find all the possible design study papers

years<-2018:2014
visPubData<-read.csv(file="visPubData.csv",header=TRUE)


allPapers<-c()

for(year in years){
  print(year)
  if(year == 2018){
    url<-paste0("http://ieeevis.org/year/",year,"/info/papers-sessions")
  }else if(year<2018 & year >=2015){
    url <- paste0('http://ieeevis.org/year/',year,'/info/overview-amp-topics/papers-sessions',collapse = "")
   
  }else{
    url <- paste0('http://ieeevis.org/year/',year,'/info/overview-amp-topics/paper-sessions',collapse = "")    
  }
 
  print(url)
  webpage <- read_html(url)
  
  
  #need both HTML and Text verions
  content_data_html<-html_nodes(webpage,'.content.clear-block p')
  content_data<- html_text(content_data_html)
  
  #identify the sessions
  session_info<-sapply(content_data,function(x){
    tmp<-str_extract(x,'((SciVis|InfoVis|VAST|SCIVIS|INFOVIS|G\\&A)).*([a-zA-Z0-9]*)Session') %>% strsplit(.,'\\:') %>% unlist()
    sessionTitle<-tmp[length(tmp)] %>% trimws() %>% gsub('Session','',.)
    track<-tmp[1]
    c(x,track,sessionTitle)
  }) %>% unname() %>% t() %>% as.data.frame()
  
  colnames(session_info)<-c("sessionInfo","track","title")
  
  session_info<- filter(session_info,!is.na(track)) %>% 
    mutate(year = year)
  
  #get papers that are associated with each sessions
  #session_info_index<-sapply(session_info$sessionInfo,function(x){grep(x,content_data)}) %>% unlist()
  
  session_info_index <-match(session_info$sessionInfo,content_data)
  
  paperInfo<-c()
  for(i in 1:length(session_info_index)){
    start <- session_info_index[i] + 1
    end<-ifelse(i != length(session_info_index),session_info_index[i+1] -1 ,length(content_data))
      
    
    #*sigh* before 2015 everything was one big blob
    if(year <=2015 | year %in% c(2018,2016)){
     papers<-content_data_html[start:end] %>% as.character() %>% strsplit(.,"<br>") %>% unlist()
     
     
     paperTitle<-papers[grepl("<strong>",papers)] %>%str_extract("g>.*<") %>% gsub('g>|<','',.)%>% trimws()
     if(year == 2018){
       authors<-papers[grepl("Authors",papers)] %>% str_extract('\\:[a-zA-Z\\,\\s+\u00F0-\u02AF\\:\\-\\,\\.\\’\\–]+') %>% gsub('\\:\\s+','',.) %>% trimws()
     }else{
       authors<-papers[grepl("Authors",papers)] %>% str_extract('\\\t[a-zA-Z\\,\\s+\u00F0-\u02AF\\.\\-]+') %>% gsub('\\\t','',.)
     }
    
     if(year %in% c(2018,2016)){
       videoPreview<-rep(NA,length(paperTitle))
      doi<-rep(NA,length(paperTitle))
     }else{
       #find missing video and doi
       info_ind<-grep("^$",papers)
       videoPreview<-papers[grepl("Video Preview",papers)] %>% str_match(.,'http[s]?://[\\.a-zA-Z0-9\\/]+')
     }
     
     papers<-cbind(as.character(rep(session_info$sessionInfo[i]),length(paperTitle)),paperTitle,authors, videoPreview,doi)
     paperInfo<-rbind(paperInfo,papers)
     
    }else{
      papers<-content_data[start:end]
      paperTitle<-str_extract(papers,"(^[a-zA-Z0-9\\s+\\:\\-\\,\\.\\’\\–]+)") %>% trimws()
      #will miss some authors
      authors<-str_extract(papers,'Authors\\:\\s+([a-zA-Z\\,\\s+\u00F0-\u02AF\\.]+)Video') %>% gsub('Authors\\: ','',.) %>% gsub('Video','',.)
      
      
      #sometimes a video preview or doi is missing, so I need find it on a paper by paper basis
      videoPreview<-c()
      doi<-c()
      for(j in start:end){
        papersMeta<-html_nodes(content_data_html[j],"a") %>% as.character()
        tmpDoi<-papersMeta[which(grepl("(DOI|Full Paper URL)",papersMeta))]%>% str_match(.,'http[s]?://[\\.a-zA-Z0-9\\/]+')
        tmpDoi<-ifelse(is.na(tmpDoi[1]),NA,tmpDoi)
        tmpVid<-papersMeta[which(grepl("Video Preview",papersMeta))]%>% str_match(.,'http[s]?://[\\.a-zA-Z0-9\\/]+')
        tmpVid<-ifelse(is.na(tmpVid[1]),NA,tmpVid)
        
        videoPreview<-c(videoPreview,tmpVid)
        doi<-c(doi,tmpDoi)
      }
      
      papers<-cbind(as.character(rep(session_info$sessionInfo[i]),length(papers)),paperTitle,authors,videoPreview,doi)
      paperInfo<-rbind(paperInfo,papers)
    }
  }
    paperInfo<-data.frame(paperInfo)
    colnames(paperInfo)<- c("sessionInfo","paperTitle","authors","videoPreview","doi")      
    paperInfo<-inner_join(session_info,paperInfo)
  #}
  allPapers<-rbind(allPapers,paperInfo)

}

#do a little clearning up of effects that remian
allPapers %<>% mutate(paperTitle = gsub("\\([a-zA-Z]\\)","",paperTitle)) %>%
  mutate(paperTitle=gsub("\\/stron.*","",paperTitle)) %>%
  mutate(paperTitle = trimws(paperTitle))

#Trying to suss out what the design study papers are
designStudySession<-filter(allPapers, !is.na(str_match(title,'[Dd]esign\\s+[Ss]tud')))
designStudyTitle<-filter(allPapers,!is.na(str_match(paperTitle,'[Dd]esign\\s+[Ss]tud')))

designStudyConf<-full_join(designStudySession,designStudyTitle)

#get visPub data stuff
visPubData %<>% filter(Year>2013)

designStudyKeyWords<-filter(visPubData,!is.na(str_match(Author.Keywords,'[Dd]esign\\s+[Ss]tud')))
designStudyAbstract<-filter(visPubData,!is.na(str_match(Abstract,'[Dd]esign\\s+[Ss]tud')))

designStudyVisPub<-full_join(designStudyKeyWords,designStudyAbstract) %>%
  dplyr::select("Year","Conference","Paper.Title","Paper.DOI","Author.Names")


colnames(designStudyVisPub)<-c("year","track","paperTitle","doi","authors")

#see if I can join on paper titles
designStudies<-full_join(designStudyVisPub,designStudyConf)

#paper Titles aren't playing nice so just de-dup in excel later. The point is to get uniqe DOIs



# --- Now run the same code as for the PCS documents --- 
library(curl)
library(rvest)
library(stringr)
library(dplyr)
library(rcrossref)
library(crminer)
library(profvis)


doi<-designStudies$doi[!is.na(designStudies$doi)] %>% unique()
doi<-gsub("https:\\/\\/doi\\.org\\/","",doi)

dl<-sapply(doi,function(x){
  links <- crm_links(x)
  
  articleNum<-str_extract(as.character(links$unspecified),"arnumber\\=([0-9]+)$") %>% gsub("arnumber=","",.)
  url<-paste0("https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=",articleNum)
  
  
  webpage <- read_html(url)
  pdfurl<-html_nodes(webpage,'iframe') %>% as.character() %>% str_extract("src\\=.*frameborder") %>% str_split(.,'\\"') %>% unlist()
  pdfurl<-pdfurl[2]
  
  if(is.null(pdfurl)){
    #might be locked out, wait five minutes then try again
    profvis::pause(600) #pause so the website doesn't feel I am attacking it
    links <- crm_links(x)
    
    articleNum<-str_extract(as.character(links$unspecified),"arnumber\\=([0-9]+)$") %>% gsub("arnumber=","",.)
    url<-paste0("https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=",articleNum)
    
    
    webpage <- read_html(url)
    pdfurl<-html_nodes(webpage,'iframe') %>% as.character() %>% str_extract("src\\=.*frameborder") %>% str_split(.,'\\"') %>% unlist()
    pdfurl<-pdfurl[2]
  }
  
  curl_download(pdfurl,destfile=paste0("designStudies/",gsub("\\/","\\.",x),".pdf"))
  
})






