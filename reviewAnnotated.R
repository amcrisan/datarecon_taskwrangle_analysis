library(readxl)
library(dplyr)

#from webscrape
designStudy<-read.csv(file="DesignStudiesCaptured.csv",header=T) %>%
  mutate(ID = gsub("\\/","\\.",doi))

#from list
allPapers<-read.csv(file="AcceptedDesignStudyPapers-Title+DOI.csv",header=TRUE) %>%
  mutate(ID = gsub("\\/","\\.",DOI))

#annotations that I have made
annotatedStudies<-read_excel("../notes/TaskAnalysisRetrospective.xlsx")

tmp<-full_join(designStudy,allPapers,by="ID") %>% full_join(.,annotatedStudies,by="ID")

#write out the full list for a closer manual inspect and clean up
#add new title that might be missing
write.csv(file="fullList.csv",tmp,quote=T)


#Read the revised version back in and get some numbers
clean_data<-read_excel("DesignStudy_Analysis_Master_List.xlsx")

unique(clean_data$DOI) %>% length() #148 unique design study papers total

clean_data %>% filter(Status!="NA") %>% select(Status,DOI,TASKS_LISTED) %>% distinct() %>% group_by(Status,TASKS_LISTED) %>% tally()

#total to 111 papers review so far (only stuff from 2010 is outstanding)
# 46 had no task listed
# 8 had not tasks listed, but was used in the Tasks to Goals paper - so I included those
# 52 had tasks listed, either as task list, goals, or design requirements
# 5 had tasks that were difficult to parse




