library(ca)
library(dplyr)
library(magrittr)
library(glmnet)
library(quanteda)
library(sophistication)
library(tidyr)
library(wordcloud)

data("data_corpus_presdebates2016")
presDebateCorpus2016seg <- cbind(speech = texts(data_corpus_presdebates2016), docvars(data_corpus_presdebates2016),
                      stringsAsFactors = FALSE)

# additional custom stopwords
extra <- c('going', 'said', 'need', 'know', 'say', 'many', 'now', 'will', 'actually', 'want', 'like',
           'mean', 'well', 'can')

####
# 0: Correspondence analysis
#    Shows document positions and comparison cloud of word positions in 2D correspondence analysis
#
####

pcaCorpus <- corpus_subset(data_corpus_presdebates2016, tag %in% c('SANDERS', 'CLINTON', 'O\'MALLEY', 'TRUMP', 'RUBIO', 'CRUZ', 'BUSH'))
toks1 <- tokens(pcaCorpus, remove_punct=TRUE, 
                remove_symbols=TRUE, remove_hyphens=TRUE,
                remove_url=TRUE, remove_numbers=TRUE) %>%
    tokens_remove(c(stopwords(), extra)) %>% tokens_ngrams(1:2)
dfm1 <- dfm(toks1, group='tag') %>% dfm_trim(min_termfreq = 15)


dim(dfm1)

res <- ca(as.matrix(t(dfm1)))
plot(res, what = c('none', 'active'))

wordWeights <- data.frame(res$rowcoord[,1:2])
wordWeights$Republican <- ifelse(wordWeights$Dim1 <0, wordWeights$Dim1, 0)
wordWeights$Democrat <- ifelse(wordWeights$Dim1 > 0, wordWeights$Dim1, 0)
wordWeights$Neg <- ifelse(wordWeights$Dim2 < 0, wordWeights$Dim2, 0)
wordWeights$Pos <- ifelse(wordWeights$Dim2 > 0, wordWeights$Dim2, 0)
pcaDimOneWeights <- data.frame(Republican=wordWeights$Republican , Democrat=wordWeights$Democrat, words=row.names(wordWeights))
rownames(pcaDimOneWeights) <- rownames(wordWeights)
wordcloud::comparison.cloud((as.matrix(pcaDimOneWeights)),title.size=1.5,
                            random.order=FALSE, rot.per=0, scale=c(1,3))

pcaDimTwoWeights <- data.frame(Negative=wordWeights$Neg , Positive=wordWeights$Pos)
rownames(pcaDimTwoWeights) <- rownames(wordWeights)
wordcloud::comparison.cloud((as.matrix(pcaDimTwoWeights)),title.size=1.5,
                            random.order=FALSE, rot.per=0, scale=c(1,2))

############
# 1.  Simple comparison clouds
#
# The color and size of the word is determined by the difference in rates of occurrence across the
# two categories (see ?comparison.cloud)
# 
# These are just relative word-rates.
############

# compare parties


toks1 <- tokens(data_corpus_presdebates2016, remove_punct=TRUE, 
                remove_symbols=TRUE, remove_hyphens=TRUE,
                remove_url=TRUE, remove_numbers=TRUE) %>%
    tokens_remove(c(stopwords(), extra)) %>% tokens_ngrams(1:2)
dfm1 <- dfm(toks1, groups = 'party') %>% dfm_trim(min_termfreq = 150) %>% as.matrix

wordcloud::comparison.cloud(t(dfm1), title.size=1.3, random.order=FALSE, rot.per = 0, max.words = 200)


# democratic candidates
demCorpus <- corpus_subset(data_corpus_presdebates2016, tag %in% c('SANDERS', 'CLINTON', 'O\'MALLEY'))
toks1 <- tokens(demCorpus, remove_punct=TRUE, 
                remove_symbols=TRUE, remove_hyphens=TRUE,
                remove_url=TRUE, remove_numbers=TRUE) %>%
    tokens_remove(c(stopwords(), extra)) %>% tokens_ngrams(1:2)
dfm1 <- dfm(toks1, groups = 'tag') %>% dfm_trim(min_termfreq = 5) %>% as.matrix #%>% dfm_weight("logcount") %>%  as.matrix

wordcloud::comparison.cloud(t(dfm1), title.size=1.3, random.order=FALSE, rot.per = 0, max.words = 200)

# republicans

toks1 <- tokens(demCorpus, remove_punct=TRUE, 
                remove_symbols=TRUE, remove_hyphens=TRUE,
                remove_url=TRUE, remove_numbers=TRUE) %>%
    tokens_remove(c(stopwords(), extra)) %>% tokens_ngrams(1:2)
dfm1 <- dfm(toks1, groups = 'tag') %>% dfm_trim(min_termfreq = 5) %>% as.matrix #%>% dfm_weight("logcount") %>%  as.matrix

wordcloud::comparison.cloud(t(dfm1), title.size=1.3, random.order=FALSE, rot.per = 0, max.words = 200)



############
# 2. Regressions and comparison clouds of coefficients
#
############

#######
# predict candidate
#######
mainCorpus <- corpus_subset(data_corpus_presdebates2016, tag %in% c('SANDERS', 'CLINTON', 'TRUMP', 'RUBIO', 'CRUZ'))

#remove very short utterances (this also balances number of speeches in each class somewhat)
mainDf <- cbind(speech = texts(mainCorpus), docvars(mainCorpus), stringsAsFactors = FALSE)
mainDf <- filter(mainDf, nchar(speech) > 80)
table(mainDf$tag)

#
# A unigram, bigram and trigram model removing standard English stopwords + the extra ones at the 
# start of this file, removing features that occur fewer than five times overall. TF-IDF is not effective
# because it can't be applied properly to isolated out-of-sample texts.
#
toks1 <- tokens(mainDf$speech, remove_punct=TRUE, 
                remove_symbols=TRUE, remove_hyphens=TRUE,
                remove_url=TRUE, remove_numbers=TRUE) %>%
    tokens_remove(c(stopwords(), extra)) %>% tokens_ngrams(1:2)
dfm1 <- dfm(toks1) %>% dfm_trim(min_termfreq = 15)

dim(dfm1)
mainMat <- as.matrix(dfm1) # 3170 features

save(mainDfm,file='mainDfmN123MinCount5.RData')

trueClass <- as.factor(mainDf$tag)
dim(mainMat)
length(trueClass)
table(trueClass)

# Elastic net model, five-class logistic regression. Closer to ridge than lasso but some parameters will be zero
eModelMain <- cv.glmnet(mainMat, y=trueClass,  family = 'multinomial', alpha=0.01, type.measure='class', standardize=TRUE)
plot(eModelMain)
min(eModelMain$cvm)
eModelMain$glmnet.fit$dfmat


#save(eModelMain,file='CandidateN123a.01.RData')
# Error rates for different values of alpha
# 0.00 : .226
# 0.01 : .199
# 0.05 : .221

# testing new text prediction
nt <- "A conservative movement committed to the cause of free enterprise, 
the only economic model where everyone can climb without anyone falling. "
 nt <- "I believe that American families have a right to hard-working immigrants. "
# nt <- Senator Sanders is a good man. When I am president I will say more nice things about him.

# need to specify 'keptFeatures' from training Dfm so that dimensions of in-sample and out-of-sample dfm match.
nd1 <- dfm(nt, ngrams=c(1,2,3)) %>% dfm_select(dfm1) %>% as.matrix
nd1[is.nan(nd1)] = 0
predict(eModelMain, nd1, s = "lambda.min", type = "response")

#######
# model for Democrat vs Republican
#######

partyClass <- as.factor(mainDf$party)
dim(mainMat)
length(partyClass)
table(partyClass)
# Elastic net model, two-class logistic regression. Closer to ridge than lasso but some coefficients will be zero
eModelParty <- cv.glmnet(mainMat, y=partyClass,  family = 'binomial', alpha=0.01, type.measure='class', standardize=TRUE)
plot(eModelParty)
min(eModelParty$cvm)
#plot(eModelParty$glmnet.fit)
#eModelParty$lambda.min
#eModelParty$nzero # 2369 nonzero coefficients

# save model for loading with Shiny app
save(eModelParty,file='PartyN123a.01.RData')

#
# Comparison cloud coefficients for Dem vs Repub.
#
cmat <- as.matrix(coef(eModelParty, s = "lambda.min"))
coes <- data.frame(estimate=cmat[,1], words=row.names(cmat) ) %>% arrange(estimate)
wordWeights<- data.frame(word=coes$words, co=coes[1])  %>% filter(estimate !=0) %>% filter(word!='(Intercept)')
wordWeights$Republican <- ifelse(wordWeights$estimate > 0, wordWeights$estimate, 0)
wordWeights$Democrat <- ifelse(wordWeights$estimate < 0, wordWeights$estimate, 0)

wct <- data.frame(Republican=wordWeights$Republican , Democrat=wordWeights$Democrat)

rownames(wct) <- wordWeights$word

wordcloud::comparison.cloud((as.matrix(wct)),title.size=1.5,
                            random.order=FALSE, rot.per=0, scale=c(1,3), max.words = 200)

# predict new text
nt <- "A conservative movement committed to the cause of free enterprise, the only economic model where everyone can climb without anyone falling. "

# need to specify 'keptFeatures' from training Dfm so that dimensions of in-sample and out-of-sample dfm match.
nd1 <- dfm(nt, keptFeatures = mainDfm, ngrams=c(1,2,3)) %>% removeFeatures(c(stopwords('english'), extra)) %>% as.matrix

tmp <- predict(eModelParty, nd1, s = "lambda.min", type = "response")


#############
# shiny app
############
setwd("~/Dropbox/hackathon")
require(shiny)
runApp('shinyHack')