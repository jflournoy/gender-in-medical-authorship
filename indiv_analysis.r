#'---
#'title: Individual level analysis 
#'output: html_document
#'---

#+ 'The Main Code 1',echo=F,results='hide'

library(data.table)
library(knitr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmer2stan)
library(sjPlot)

opts_chunk$set(dev='svg', results='markup', echo='T', fig.width=10, fig.height=10,
	       cache=T)

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

indivLevelDat <- input_files %>%
# 	slice(c(1,10)) %>%
	group_by(filename) %>%
	do({
		# data.frame(file=.$filename[[1]],class=class(.$filename[[1]]))
		aDT<-fread(as.character(.$filename[[1]]),colClasses=author_col_classes)
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

indivLevelDat$pubyear_c <- as.numeric(indivLevelDat$pubDate)-2014

#+ 'The first model'
bigModel <- glm(male ~ 1 + first_author*pubyear_c + last_author*pubyear_c, data=indivLevelDat, 
		family='binomial')

summary(bigModel)
library(boot)
inv.logit(coef(bigModel))


dummyDat <- expand.grid(pubyear_c=(2002:2014-2014), 
			first_author=c(T, F), 
			last_author=c(T, F))[-(1:length(2002:2014)), ]

dummyDat_outcome <- cbind(dummyDat, 
			  predict(bigModel, newdata=dummyDat, type='response', se.fit=T))
dummyDat_group <- dummyDat_outcome %>% mutate(authorPos=ifelse(!first_author, 
				     ifelse(!last_author, 
					    'middle', 
					    'last'), 
				     'first'),
				      pubyear=pubyear_c+2014,
				      upper=fit+1.96*se.fit,
				      lower=fit-1.96*se.fit)


ggplot(dummyDat_group, aes(x=pubyear, y=fit))+
	geom_line(aes(color=authorPos))+
	coord_cartesian(y=c(.45, .7))+
	geom_hline(yintercept=.5, color='red')+
	geom_ribbon(aes(ymin=lower, ymax=upper, fill=authorPos), alpha=.3)+
	labs(x='Publication Year', y='Probability author is male', legend='Author Position')+
	theme(panel.background=element_rect(fill='white'))



#'
#' L1: $\text{male}_{ij} = \beta_{0j} + \beta_{1j}\text{1stAu}_{ij} + \beta_{2j}\text{LstAu}_{ij} + \beta_{3j}\text{Year}_{ij} + \beta_{4j}\text{1stAu}_{ij}\text{Year}_{ij} + \beta_{5j}\text{LstAu}_{ij}\text{Year}_{ij} + \epsilon_{ij}$
#' 
#' L2: 
#'
#' - $\beta_{0j} = \gamma_{00} + u_{0j}$
#' - $\beta_{3j} = \gamma_{30} + u_{3j}$
#'
#'

library(lme4)

indivLevelDat$groupNum <- as.numeric(as.factor(indivLevelDat$term))

indivLevDatSample <- sample_frac(group_by(indivLevelDat, term),
				 size=.1)

nullModel <- glmer(male ~ 1 + (1|term), 
			data=indivLevDatSample, 
			family='binomial')
summary(nullModel)
sjp.glmer(nullModel, sort.coef='(Intercept)')

slopeModel <- glmer(male ~ 1 + first_author*pubyear_c + last_author*pubyear_c + (1+pubyear_c|term), 
			data=indivLevDatSample, 
			family='binomial')
summary(slopeModel)
sjp.glmer(slopeModel, sort.coef='sort.all', facet.grid=T, free.scale=F)
sjp.glmer(slopeModel, type='ri.pc')



nullModelStan <- glmer2stan(male ~ 1 + (1|groupNum), 
			data=indivLevDatSample, 
			family='binomial', sample=F)
