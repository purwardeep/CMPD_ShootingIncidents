#Estimate Coefficient
# Please read http://davidmeza1.github.io/2015/07/20/topic-modeling-in-R.html
# library(devtools)
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
library(mice)
library(e1071)
library(SparseM)
library(caret)
library(Matrix)
library(stm)
library(igraph)
library(wordcloud)
library(Rmpfr)
library(quanteda)
library(SnowballC)
library(RTextTools)
library(ROCR)
getwd()

data <- read.csv("MERGED_SHOOTING_FINAL.csv")
str(data)
wordstoremove <- c("officers", "officer","individual", "individuals")
data_sub <- as.data.frame(sapply(data, function(x) gsub(paste(wordstoremove, collapse = '|'), '', x)))
data <-data_sub

processed <- textProcessor(data$NARRATIVE, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

out <- prepDocuments(processed$documents, processed$vocab,
                     processed$meta, lower.thresh = 1)
# USING COVARIATES
shooting_prevfit_coeff_est <- stm(out$documents, out$vocab, K=4, prevalence =~ OFFICER_POLICY_VIOLATION + OI_BW_RACE + OI_SAME_RACE + OPP_GENDER + SAME_GENDER + DA_LEGAL_REVIEW  + INDIVIDUAL_INJURY_TYPE ,
                                  max.em.its=200, data=out$meta, init.type="Spectral",
                                  seed=749590)

esteff_shooting = estimateEffect(formula = 1:4 ~ OFFICER_POLICY_VIOLATION + OI_BW_RACE + OI_SAME_RACE + OPP_GENDER + SAME_GENDER + DA_LEGAL_REVIEW  + INDIVIDUAL_INJURY_TYPE , stmobj = shooting_prevfit_coeff_est,
                                 metadata = out$meta, uncertainty = "Global")

summary(esteff_shooting)
print(summary(esteff_shooting))
