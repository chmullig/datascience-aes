source("ASAP-AES/Evaluation_Metrics/R/quadratic_weighted_kappa.R")
require(plyr)

library(doMC)
library(foreach)
library(caret)
registerDoMC(2)

training <- read.csv("train_tagged_tfidf.csv")
nrow(training)

training$spell_mistakes <- training$num_words - training$num_correctly_spelled
training$avg_length <- training$num_chars / training$num_words
training$avg_syls <- training$num_syl / training$num_words
training$spell_pct <- training$num_correctly_spelled / training$num_words
training$has_a_quote <- as.numeric(training$num_quotes >= 2)


#section just temp for this data run
training$pos_WRB[is.na(training$pos_WRB)] <- 0
training$pos_PRP.[is.na(training$pos_PRP.)] <- 0
training$pos_VBG[is.na(training$pos_VBG)] <- 0
training$pos_FW[is.na(training$pos_FW)] <- 0
training$pos_CC[is.na(training$pos_CC)] <- 0
training$pos_PDT[is.na(training$pos_PDT)] <- 0
training$pos_RBS[is.na(training$pos_RBS)] <- 0
training$pos_PRP[is.na(training$pos_PRP)] <- 0
training$pos_CD[is.na(training$pos_CD)] <- 0
training$`pos_WP.`[is.na(training$pos_WP.)] <- 0
training$pos_VBP[is.na(training$pos_VBP)] <- 0
training$pos_VBN[is.na(training$pos_VBN)] <- 0
training$pos_EX[is.na(training$pos_EX)] <- 0
training$pos_JJ[is.na(training$pos_JJ)] <- 0
training$pos_IN[is.na(training$pos_IN)] <- 0
training$pos_WP[is.na(training$pos_WP)] <- 0
training$pos_VBZ[is.na(training$pos_VBZ)] <- 0
training$pos_DT[is.na(training$pos_DT)] <- 0
training$pos_MD[is.na(training$pos_MD)] <- 0
training$pos_NNPS[is.na(training$pos_NNPS)] <- 0
training$pos_RP[is.na(training$pos_RP)] <- 0
training$pos_NN[is.na(training$pos_NN)] <- 0
training$pos_POS[is.na(training$pos_POS)] <- 0
training$pos_RBR[is.na(training$pos_RBR)] <- 0
training$pos_VBD[is.na(training$pos_VBD)] <- 0
training$pos_JJS[is.na(training$pos_JJS)] <- 0
training$pos_JJR[is.na(training$pos_JJR)] <- 0
training$pos_VB[is.na(training$pos_VB)] <- 0
training$pos_TO[is.na(training$pos_TO)] <- 0
training$pos_UH[is.na(training$pos_UH)] <- 0
training$pos_LS[is.na(training$pos_LS)] <- 0
training$pos_RB[is.na(training$pos_RB)] <- 0
training$pos_WDT[is.na(training$pos_WDT)] <- 0
training$pos_NNS[is.na(training$pos_NNS)] <- 0
training$pos_NNP[is.na(training$pos_NNP)] <- 0


training$ner_caps_pct = training$ner_caps / training$num_words
training$ner_date_pct = training$ner_date / training$num_words
training$ner_location_pct = training$ner_location / training$num_words
training$ner_money_pct = training$ner_money / training$num_words
training$ner_month_pct = training$ner_month / training$num_words
training$ner_num_pct = training$ner_num / training$num_words
training$ner_organization_pct = training$ner_organization / training$num_words
training$ner_percent_pct = training$ner_percent / training$num_words
training$ner_person_pct = training$ner_person / training$num_words
training$ner_time_pct = training$ner_time / training$num_words

training$num_adjectives_pct = training$num_adjectives / training$num_words
training$num_adverbs_pct = training$num_adverbs / training$num_words
training$num_nouns_pct = training$num_nouns / training$num_words
training$num_quotes_pct = training$num_quotes / training$num_words
training$num_superlatives_pct = training$num_superlatives / training$num_words
training$num_syl_pct = training$num_syl / training$num_words
training$num_verbs_pct = training$num_verbs / training$num_words

training$pos_CC_pct = training$pos_CC / training$num_words
training$pos_CD_pct = training$pos_CD / training$num_words
training$pos_DT_pct = training$pos_DT / training$num_words
training$pos_EX_pct = training$pos_EX / training$num_words
training$pos_FW_pct = training$pos_FW / training$num_words
training$pos_IN_pct = training$pos_IN / training$num_words
training$pos_JJ_pct = training$pos_JJ / training$num_words
training$pos_JJR_pct = training$pos_JJR / training$num_words
training$pos_JJS_pct = training$pos_JJS / training$num_words
training$pos_LS_pct = training$pos_LS / training$num_words
training$pos_MD_pct = training$pos_MD / training$num_words
training$pos_NN_pct = training$pos_NN / training$num_words
training$pos_NNP_pct = training$pos_NNP / training$num_words
training$pos_NNPS_pct = training$pos_NNPS / training$num_words
training$pos_NNS_pct = training$pos_NNS / training$num_words
training$pos_PDT_pct = training$pos_PDT / training$num_words
training$pos_POS_pct = training$pos_POS / training$num_words
training$pos_PRP_pct = training$pos_PRP / training$num_words
training$pos_RB_pct = training$pos_RB / training$num_words
training$pos_RBR_pct = training$pos_RBR / training$num_words
training$pos_RBS_pct = training$pos_RBS / training$num_words
training$pos_RP_pct = training$pos_RP / training$num_words
training$pos_TO_pct = training$pos_TO / training$num_words
training$pos_UH_pct = training$pos_UH / training$num_words
training$pos_VB_pct = training$pos_VB / training$num_words
training$pos_VBD_pct = training$pos_VBD / training$num_words
training$pos_VBG_pct = training$pos_VBG / training$num_words
training$pos_VBN_pct = training$pos_VBN / training$num_words
training$pos_VBP_pct = training$pos_VBP / training$num_words
training$pos_VBZ_pct = training$pos_VBZ / training$num_words
training$pos_WDT_pct = training$pos_WDT / training$num_words
training$pos_WP_pct = training$pos_WP / training$num_words
training$pos_WRB_pct = training$pos_WRB / training$num_words

training$holdout <- 0
training$holdout[sample(nrow(training), as.integer(nrow(training)*.1))] <- 1



#Caret feature selection...
X <- subset(training, select=-c(grade, essay, rate1, rate2, holdout, pos_UH, pos_NNPS, X, rfprediction, rfscorehat))
print(paste(c("Starting columns:", ncol(X))))
nzv <- nearZeroVar(X)
filteredX <- X[, -nzv]
print(paste(c("Filtered by near zero variance:", ncol(filteredX))))

Xcor <- cor(filteredX)
highlyCorX <- findCorrelation(Xcor, cutoff=.75)
filteredX <- filteredX[, -highlyCorX]
print(paste(c("Filtered by near very high correlation:", ncol(filteredX))))
filteredX$set <- training$set

set.seed(1)
inTrain <- sample(seq(along = training$grade), length(training$grade)/2)
trainX <- filteredX[inTrain,]
testX <- filteredX[-inTrain,]
trainGrade <- training$grade[inTrain]
testGrade <- training$grade[-inTrain]
print(length(trainGrade))

fitControl <- trainControl(method="repeatedcv", number=8, repeats=3, returnResamp="all")
gbmGrid <- expand.grid(.interaction.depth=1:4,
                       .n.trees = c(1000, 5000, 10000),
                       .shrinkage=c(0.05, 0.1, 0.15)
                       )

gbmFit1 <- train(trainX, trainGrade, method="gbm", trControl = fitControl, verbose=FALSE, tuneGrid=gbmGrid)
gbmFit1
