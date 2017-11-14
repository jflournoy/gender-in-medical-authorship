## ---- loadIndivData ----

require(tidyverse)
require(data.table)

if(!file.exists('./RData/indivLevelDat.RDS')){
  indivLevelDat <- data.table(filename = inputFiles, stringsAsFactors = F) %>%
    group_by(filename) %>%
    do({
      aDT<-fread(as.character(.$filename[[1]]), colClasses = authorColClasses)
      result<-aDT %>%
        mutate(simple_sex=sub('.*?((fe)*male)','\\1',Author_Sex)) %>%
        filter(simple_sex %in% c('male','female'),pubDate >= 2002) %>%
        group_by(PubMed_ID) %>%
        mutate(last=Author_Rank_in_Paper == max(Author_Rank_in_Paper) &
                 Author_Rank_in_Paper != 1,
               first_author=Author_Rank_in_Paper %in% 1,
               last_author=last %in% TRUE,
               middle_author=!(Author_Rank_in_Paper %in% 1) & !(last %in% TRUE),
               male=as.numeric(simple_sex=='male'))
      result
    }) %>%
    mutate(term=sub('author_centric_(.*)\\.csv','\\1',filename))
  saveRDS(indivLevelDat, './RData/indivLevelDat.RDS')
} else {
  indivLevelDat <- readRDS('./RData/indivLevelDat.RDS')
}