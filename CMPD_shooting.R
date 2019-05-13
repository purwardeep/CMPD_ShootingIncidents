install_github("bstewart/stm",dependencies=TRUE)
library(devtools)
install_github("mroberts/stmBrowser",dependencies=TRUE)
library(stmBrowser)
library(tm)
library(topicmodels)
library(data.table)
library(slam)
library(ggplot2)
library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(janeaustenr)
library(stringr)
library(gender)
library(zoo)
library(Hmisc)
library(lmtest)
library(plm)
library(vars)

library(SparseM)
library(Matrix)
library(stm)
library(igraph)
library(wordcloud)
library(Rmpfr)
library(quanteda)
library(SnowballC)
library(RTextTools)

getwd()
data <- read.csv("CMPD_OfficerInvolved_Shootings__Incidents.csv")
wordstoremove <- c("officers", "officer","individual", "individuals")
data_sub <- as.data.frame(sapply(data, function(x) gsub(paste(wordstoremove, collapse = '|'), '', x)))
data <-data_sub

processed <- textProcessor(data$NARRATIVE, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

out <- prepDocuments(processed$documents, processed$vocab,
                     processed$meta, lower.thresh = 1)
# Keeping  thresh = 1 because we are dealing small amount of data 

shooting_prevfit <- stm(out$documents, out$vocab, K=3, 
                        max.em.its=200, data=out$meta, init.type="Spectral", 
                        seed=749590)

mod.out.corr <- topicCorr(shooting_prevfit)
plot(mod.out.corr)
plot(shooting_prevfit, type="summary", xlim=c(0,1.6))
plot(shooting_prevfit, type="labels", topics=c(1,2,3))
plot(shooting_prevfit, type="perspectives", topics=c(1,3))


#MODEL SELECTION
shootingSelect <- selectModel(out$documents, out$vocab, K=4,
                              max.em.its=200, data=meta, runs=20, seed=749590)

plotModels(shootingSelect)
selectedmodel <- shootingSelect$runout[[3]]
topicQuality(model=selectedmodel, documents=docs)

kResult <- searchK(out$documents, out$vocab, K=c(2,3,4,5),
                   data=meta)
plot(kResult)

#ESTIMATING STM 
shooting_fit <- stm(out$documents, out$vocab, K=4, 
                    max.em.its=200, data=out$meta, init.type="Spectral", 
                    seed=749590)


#INTERPRETING STM OUTPUT
TOPIC_WORDS <- labelTopics(shooting_fit, c(1,2,3,4))
TOPIC_WORDS

thought1 <- findThoughts(shooting_fit, texts=data$NARRATIVE, n=2, topics=1)$docs[[1]]
thought2 <- findThoughts(shooting_fit, texts=data$NARRATIVE, n=2, topics=2)$docs[[1]]
thought3 <- findThoughts(shooting_fit, texts=data$NARRATIVE, n=2, topics=3)$docs[[1]]
thought4 <- findThoughts(shooting_fit, texts=data$NARRATIVE, n=2, topics=4)$docs[[1]]
plotQuote(thought1, width=100, main="Topic 1")
plotQuote(thought2, width=100, main="Topic 2")
plotQuote(thought3, width=100, main="Topic 3")
plotQuote(thought4, width=100, main="Topic 4")

mod.out.corr <- topicCorr(shooting_fit)
plot(mod.out.corr)

plot(shooting_fit, type="perspectives", topics=c(1,3))

cloud(shooting_fit, topic=1)
cloud(shooting_fit, topic=2)
cloud(shooting_fit, topic=3)
cloud(shooting_fit, topic=4)
topic_prob <- shooting_fit$theta
