require(quanteda)
require(magrittr)
require(dplyr)
require(tidyr)
require(wordcloud)
require(glmnet)
load(url("http://www.kenbenoit.net/files/presDebateCorpus2016seg.RData"))

# put data in dataframe too
presDebateDf <- cbind(speech = texts(presDebateCorpus2016seg), docvars(presDebateCorpus2016seg), stringsAsFactors = FALSE)

extra <- c('going', 'said', 'need', 'know', 'say', 'many', 'now', 'will', 'actually', 'want', 'like')


####
#0: PCA
#
####
require(ca)
pcaCorpus <- subset(presDebateCorpus2016seg, tag %in% c('SANDERS', 'CLINTON', 'O\'MALLEY', 'TRUMP', 'RUBIO', 'CRUZ', 'BUSH'))
pcaDfm <- dfm(pcaCorpus, groups = 'tag') %>% removeFeatures(c(stopwords('english'), extra)) %>%
    trim(minCount = 6) %>% weight('logFreq')

res <- ca(as.matrix(t(pcaDfm)))
plot(res, what = c('none', 'active'))

wordWeights <- data.frame(res$rowcoord[,1:2])

wordWeights$Republican <- ifelse(wordWeights$Dim1 <0, wordWeights$Dim1, 0)
wordWeights$Democrat <- ifelse(wordWeights$Dim1 > 0, wordWeights$Dim1, 0)
wordWeights$Neg <- ifelse(wordWeights$Dim2 < 0, wordWeights$Dim2, 0)
wordWeights$Pos <- ifelse(wordWeights$Dim2 > 0, wordWeights$Dim2, 0)
wct <- data.frame(Republican=wordWeights$Republican , Democrat=wordWeights$Democrat)
rownames(wct) <- rownames(wordWeights)
wordcloud::comparison.cloud((as.matrix(wct)),title.size=1.5,
                            random.order=FALSE, rot.per=0, scale=c(1,3))

wct <- data.frame(Negative=wordWeights$Neg , Positive=wordWeights$Pos)
rownames(wct) <- rownames(wordWeights)
wordcloud::comparison.cloud((as.matrix(wct)),title.size=1.5,
                            random.order=FALSE, rot.per=0, scale=c(1,2))

############
# 1.  Simple comparison clouds
#
# The color and size of the word is determined by the difference in rates of occurrence across the
# two categories (see ?comparison.cloud)
############

# compare parties
presDebateMat <- dfm(presDebateCorpus2016seg, groups = 'party') %>% 
    removeFeatures(stopwords('english')) %>% trim(minCount = 5) %>% as.matrix
wordcloud::comparison.cloud(t(presDebateMat), title.size=1.3, random.order=FALSE, rot.per = 0, max.words = 100)

# democratic candidates
demCorpus <- subset(presDebateCorpus2016seg, tag %in% c('SANDERS', 'CLINTON', 'O\'MALLEY'))
demMat <- dfm(demCorpus, groups = 'tag') %>% 
    removeFeatures(c(stopwords('english'), extra)) %>% trim(minCount = 5) %>% as.matrix
wordcloud::comparison.cloud(t(demMat), title.size=1.3, random.order=FALSE, rot.per = 0, max.words = 200)

# republicans

repCorpus <- subset(presDebateCorpus2016seg, tag %in% c('TRUMP', 'RUBIO', 'CRUZ', 'BUSH'))
repMat <- dfm(repCorpus, groups = 'tag') %>% 
    removeFeatures(c(stopwords('english'), extra)) %>% trim(minCount = 5) %>% as.matrix
wordcloud::comparison.cloud(t(repMat), title.size=1.3, random.order=FALSE, rot.per = 0, max.words = 200)

# Trump v others
trumpFactor <- ifelse(docvars(repCorpus, 'tag') == 'TRUMP', 'TRUMP', 'OTHER')
docvars(repCorpus, 'trumpFactor') <- trumpFactor
repMat <- dfm(repCorpus, groups = 'trumpFactor') %>% 
    removeFeatures(c(stopwords('english'), extra)) %>% trim(minCount = 5) %>% as.matrix
wordcloud::comparison.cloud(t(repMat), title.size=1.3, random.order=FALSE, rot.per = 0, max.words = 200)

############
# 2. Regressions and coefficients
#
# The color and size of the word is determined by the difference in rates of occurrence across the
# two categories (see ?comparison.cloud)
############

#remove very short utterances
repDf <- cbind(speech = texts(repCorpus), docvars(repCorpus), stringsAsFactors = FALSE)
repDf <- filter(repDf, nchar(speech) > 30)



repDfm <- dfm(repDf$speech) %>% trim(minDoc=8) %>% 
    removeFeatures(c(stopwords('english'), extra)) 
repMat <- as.matrix(repDfm)

trueClass <- as.factor(repDf$trumpFactor)
dim(repMat)
length(trueClass)
table(trueClass)
eModelTrump <- cv.glmnet(repMat, y=trueClass,  family = 'binomial', alpha=0.04, type.measure='class', standardize=TRUE)
plot(eModelTrump)
plot(eModelTrump$glmnet.fit, xvar='dev')
min(eModelTrump$cvm)

cmat <- as.matrix(coef(eModel, s = "lambda.min"))
coes <- data.frame(estimate=cmat[,1], words=row.names(cmat) ) %>% arrange(estimate)
coDict<- data.frame(word=coes$words, co=coes[1])  %>% filter(estimate !=0) %>% filter(word!='(Intercept)')
coDict$one<- ifelse(coDict$estimate > 0, coDict$estimate, 0)
coDict$zero <- ifelse(coDict$estimate < 0, coDict$estimate, 0)
wct <- data.frame(Trump=coDict$one, Other=coDict$zero)
rownames(wct) <- coDict$word
wordcloud::comparison.cloud((as.matrix(wct)),title.size=1.5, random.order=FALSE, rot.per=0, max.words=200)


# predict Trumpiness
nt <- "I have a tremendous brain. I know many of the finest businesses. "
#nt <- "I believe that American families have a right to hard-working immigrants. "
nd1 <- dfm(nt, keptFeatures = repDfm) %>% removeFeatures(c(stopwords('english'), extra)) %>% as.matrix
nd1[is.nan(nd1)] = 0
predict(eModel, nd1, s = "lambda.min", type = "response")

save(eModel, file='trumpModel.RData')

#############
# shiny app
############
setwd("~/Dropbox/hackathon")
require(shiny)
runApp('shinyHack')



########
# old code here
#########

#remove very short utterances
demDf <- cbind(speech = texts(demCorpus), docvars(demCorpus), stringsAsFactors = FALSE)
demDf <- filter(demDf, nchar(speech) > 30)
trueClass <- as.factor(demDf$tag)

demDfm <- dfm(demDf$speech) %>% trim(minDoc=4) %>% removeFeatures(stopwords('english')) %>% weight(type="logFreq")
dmat <- as.matrix(demDfm)
dim(dmat)
length(trueClass)

e1 <- cv.glmnet(dmat, y=trueClass,  family = 'binomial', alpha=0.05, type.measure='class', standardize=TRUE)
plot(e1)
min(e1$cvm)
summary(e1)
cmat <- as.matrix(coef(e1, s = "lambda.min"))
coes <- data.frame(estimate=cmat[,1], words=row.names(cmat) ) %>% arrange(estimate)
coDict<- data.frame(word=coes$words, co=coes[1])  %>% filter(estimate !=0) %>% filter(word!='(Intercept)')
coDict$one<- ifelse(coDict$estimate > 0, coDict$estimate, 0)
coDict$zero <- ifelse(coDict$estimate < 0, coDict$estimate, 0)
wct <- data.frame(one=coDict$one, zero=coDict$zero)
rownames(wct) <- coDict$word
wordcloud::comparison.cloud((as.matrix(wct)),title.size=1.5,
                            random.order=FALSE, rot.per=0.01)
rownames(wct) <- coDict$word

#newt <- "millions of jails control"
newt <- "Sanders is a good man. When I am president I will patronize him some more."
#newt <- demDf$speech[1]

nd1 <- dfm(newt, keptFeatures = demDfm) %>% removeFeatures(stopwords('english')) %>% as.matrix
nd1[is.nan(nd1)] = 0
predict(e1, nd1, s = "lambda.min", type = "response")

res <- predict(e1, dmat[1,,drop=FALSE], s = "lambda.min", type = "class")

rdv <- docvars(presDebateCorpus2016seg)
te <- texts(presDebateCorpus2016seg)
bySpeakerDfm <- dfm(presDebateCorpus2016seg, groups='tag', removePunct = FALSE)


testDf <- as.data.frame(bySpeakerDfm)

dems <- subset(presDebateCorpus2016seg, tag == 'SANDERS' | tag == 'CLINTON')
demMat <- dfm(dems, groups='tag') %>% tfidf
wordcloud::comparison.cloud(t(as.matrix(demMat)), title.size=1.5, random.order=FALSE)

reps <- subset(presDebateCorpus2016seg, tag %in% c('CRUZ', 'TRUMP'))
reps <- corpus(collocations(texts(reps)), docvars(reps))
repDfm <- dfm(reps, groups='tag', ngrams=c(1,2)) %>% removeFeatures(stopwords('english')) %>% tfidf
wordcloud::comparison.cloud(t(as.matrix(repDfm)), title.size = 1, rot.per = 0, random.order=FALSE)


