#'---
#'title: Summaries for gender and author position pubmed data
#'output: html_document
#'---

#+ 'The Main Code 1',echo=F,results='hide'

library(data.table)
library(knitr)
library(dplyr)
library(tidyr)
library(broom)
library(printr)

opts_chunk$set(dev='svg')

setwd('/home/jflournoy/Documents/Win/UO/UCLA_Gender_Pub/output/output')
input_files<-data.frame(filename=dir(pattern='author.*csv'),stringsAsFactors=F)
author_col_classes<-c(
		'logical',
		'character',
		'character',
		'character',
		'character',
		'character',
		'integer',
		'character',
		'character')

summaries<-input_files %>%
# 	slice(c(1,10)) %>%
group_by(filename) %>%
do({
	# data.frame(file=.$filename[[1]],class=class(.$filename[[1]]))
	aDT<-fread(as.character(.$filename[[1]]),colClasses=author_col_classes)
	result<-aDT %>%
	mutate(simple_sex=sub('.*?((fe)*male)','\\1',Author_Sex)) %>%
	filter(simple_sex %in% c('male','female'),pubDate >= 2002) %>%
	group_by(PubMed_ID) %>%
	mutate(last=Author_Rank_in_Paper == max(Author_Rank_in_Paper) & Author_Rank_in_Paper != 1) %>%
	group_by(simple_sex,pubDate) %>%
	summarise(
		n_tot=n(),
		first_author=sum(Author_Rank_in_Paper %in% 1),
		last_author=sum(last %in% TRUE),
		middle_author=sum(!(Author_Rank_in_Paper %in% 1) & !(last %in% TRUE))
		) %>%
	mutate(
		first_author_prop=first_author/n_tot,
		last_author_prop=last_author/n_tot,
		middle_author_prop=middle_author/n_tot,
		n_check=first_author+last_author+middle_author)
	result
}) %>%
mutate(term=sub('author_centric_(.*)\\.csv','\\1',filename))

XsqTest <- summaries %>% group_by(term, pubDate) %>%
	select(first_author, middle_author, last_author) %>%
	do({
		Xsq <- chisq.test(.[,c('first_author', 'middle_author', 'last_author')])
# 		data_frame(test=list(Xsq))
		cbind(tidy(Xsq), data_frame(test=list(Xsq)))
	})

#'
#' ## $\chi^2$ tests contingency table tests with 2 df
#'
#+ results='asis'
thing <- XsqTest %>% group_by(term) %>%
	do({
		theCaption=as.character(.$term[[1]])
		aTable <- select(., pubDate, statistic, p.value) %>%
			kable(caption=theCaption, 
			      col.names=c('Year', '$\\chi^2$', '*p* value'),
			      digits=2)
		print(aTable)
		data_frame(table=list(aTable))
	})

summaries %>% select(simple_sex) %>% distinct(simple_sex)

library(ggplot2)

aPlot<-summaries %>%
select(filename:n_tot,matches('.*_prop'),term) %>%
gather(stat,value,first_author_prop:middle_author_prop,convert=T) %>%
mutate(stat_fact=factor(
	stat,
	levels=c('first_author_prop','middle_author_prop','last_author_prop'))) %>%
filter(!(term %in% c('podiatry','plastic surgery','optometry'))) %>%
ggplot(aes(x=as.numeric(pubDate),y=value))+
geom_point(aes(size=1/log(n_tot),color=simple_sex),alpha=.6)+
geom_smooth(method='loess',aes(color=simple_sex),se=F)+
facet_grid(term~stat_fact)+
theme(panel.background=element_rect(fill='white'))

anotherPlot<-summaries %>%
select(filename:n_tot,matches('.*_prop'),term) %>%
gather(stat,value,first_author_prop:middle_author_prop,n_tot,convert=T) %>%
unite(stat_sex,stat,simple_sex) %>%
spread(stat_sex,value) %>%
transmute(
	filename=filename,
	term=term,
	pubDate=pubDate,
	first_au_fm_ratio=first_author_prop_female/first_author_prop_male,
	middle_au_fm_ratio=middle_author_prop_female/middle_author_prop_male,
	last_au_fm_ratio=last_author_prop_female/last_author_prop_male,
	n_tot=n_tot_female+n_tot_male) %>%
gather(stat,value,first_au_fm_ratio:last_au_fm_ratio) %>%
mutate(stat_fact=factor(
	stat,
	levels=c('first_au_fm_ratio','middle_au_fm_ratio','last_au_fm_ratio'))) %>%
filter(!(term %in% c('podiatry','plastic surgery','optometry'))) %>%
ggplot(aes(x=as.numeric(pubDate),y=log(value)))+
geom_point(aes(size=1/log(n_tot)),alpha=.25)+
geom_point(alpha=.8)+
geom_smooth(method='loess',se=F,alpha=.25)+
facet_grid(term~stat_fact)+
geom_hline(yintercept=0,color='red')+
theme(panel.background=element_rect(fill='white'))

#' # Summary CSV file
#+echo=F
setwd('/home/jflournoy/Documents/Win/UO/UCLA_Gender_Pub/code/ucla_pubmed_gender_bias/R_summary')

summaries_output<-summaries %>% ungroup() %>%
mutate(
	n_check_passed= n_tot==n_check,
	pubDate=as.numeric(pubDate)) %>%
select(-filename,-n_check) %>%
gather(var,value,-simple_sex,-pubDate,-term) %>%
unite(col_name,var,simple_sex) %>%
select(term,pubDate,col_name,value) %>%
spread(col_name,value) 

summaries_output %>%
summarise(problem = sum(!n_check_passed_male)+sum(!n_check_passed_female)) %>% 
(function(thing) if(thing[[1]] != 0 ) warning('Problem with counting up authors...dig back in'))

summaries_output %>%
select(-matches('n_check_passed.*')) %>%
write.csv('summary_output.csv',row.names=F,quote=F)

#' [Click here to open the CSV summary](summary_output.csv)
#'
#' # Some Graphs
#+fig.height=60,fig.width=10,echo=F
print(anotherPlot)
ggsave(plot=anotherPlot, file='plot1.svg', limitsize=F)

#+fig.height=60,fig.width=10,echo=F
print(aPlot)
ggsave(plot=aPlot, file='plot2.svg', limitsize=F)

#' # The code for the above

#+ 'The Main Code 1',echo=T,eval=F
