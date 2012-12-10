source("ASAP-AES/Evaluation_Metrics/R/quadratic_weighted_kappa.R")
require(plyr)
require(MASS)
require(ggplot2)
require(reshape)

options(scipen=3)

training <- read.csv("train_tagged_tfidf.csv")
nrow(training)

training$spell_mistakes <- training$num_words - training$num_correctly_spelled
training$avg_length <- training$num_chars / training$num_words
training$avg_syls <- training$num_syl / training$num_words
training$spell_pct <- training$num_correctly_spelled / training$num_words
training$has_a_quote <- as.numeric(training$num_quotes >= 2)
training$rel_distinct <- training$distinct_words / training$num_words
training$nouns_to_adjs <- training$num_nouns/training$num_adjectives
training$nouns_to_adjs[is.nan(training$nouns_to_adjs) | is.infinite(training$nouns_to_adjs)] <- 0
training$nouns_to_verbs <- training$num_nouns/training$num_verbs
training$nouns_to_verbs[is.nan(training$nouns_to_verbs) | is.infinite(training$nouns_to_verbs)] <- 0
training$nouns_to_adverbs <- training$num_nouns/training$num_adverbs
training$nouns_to_adverbs[is.nan(training$nouns_to_adverbs) | is.infinite(training$nouns_to_adverbs)] <- 0
training$adjs_to_verbs <- training$num_adjectives/training$num_verbs
training$adjs_to_verbs[is.nan(training$adjs_to_verbs) | is.infinite(training$adjs_to_verbs)] <- 0


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

qplot(grade, data=training, facets=set~., binwidth=1)


#Run Linear Model
models <- dlply(training[training$holdout==0,], .(set), lm,
                formula = grade ~ num_chars + log(num_chars) + avg_length + avg_syls  + spell_pct + starts_with_dear + spell_mistakes + sentance_length + num_superlatives + rel_distinct + nouns_to_adjs + nouns_to_verbs + nouns_to_adverbs + adjs_to_verbs + has_semicolon + has_exclamation + has_questionmark + num_quotes + proper_quote_punc + has_a_quote 
                +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
                +pos_CC+pos_CD+pos_DT+pos_EX+pos_IN+pos_JJ+pos_JJR+pos_JJS+pos_MD+pos_NN+pos_NNP+pos_NNS+pos_PDT+pos_POS+pos_PRP+`pos_PRP.`+pos_RB+pos_RBR+pos_RBS+pos_RP+pos_TO+pos_VB+pos_VBD+pos_VBG+pos_VBN+pos_VBP+pos_VBZ+pos_WDT+pos_WP+pos_WRB
)
lapply(models, summary)

for (i in 1:max(training$set)) {
  training$scorehat[training$set==i] <- predict(models[[i]], training[training$set==i,])
  training$prediction <- round(training$scorehat)
  training$prediction[training$set==i & training$prediction < min(training$grade[training$set==i])] <- min(training$grade[training$set==i])
  training$prediction[training$set==i & training$prediction > max(training$grade[training$set==i])] <- max(training$grade[training$set==i])
}

training$residual <- training$grade - training$prediction
qplot(prediction, data=training, facets=set~., binwidth=1)

kappas <- dlply(training[!is.na(training$grade),], .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$prediction[X$holdout==1]))
print(round(unlist(kappas), 4))
print(MeanQuadraticWeightedKappa(kappas))

qplot(training$grade, training$prediction)
qplot(training$residual[training$holdout==1])



#randomForest
print("randomForest!")
require(randomForest)
rfms = list()
for (i in 1:max(training$set)) {
  print(paste(c("RFM: set", i)))
  rfms[[i]] <- randomForest(grade ~ num_chars + log(num_chars) + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_superlatives + avg_length + spell_pct + rel_distinct + has_semicolon + has_exclamation + has_questionmark + has_questionmark + num_quotes + proper_quote_punc
                            +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
                            +pos_CC+pos_CD+pos_DT+pos_EX+pos_IN+pos_JJ+pos_JJR+pos_JJS+pos_MD+pos_NN+pos_NNP+pos_NNS+pos_PDT+pos_POS+pos_PRP+`pos_PRP.`+pos_RB+pos_RBR+pos_RBS+pos_RP+pos_TO+pos_VB+pos_VBD+pos_VBG+pos_VBN+pos_VBP+pos_VBZ+pos_WDT+pos_WP+pos_WRB
                            ,
                            data=training[training$set == i & training$holdout==0,], importance=TRUE, ntree=5000, nodesize=15)
  training$rfscorehat[training$set==i] <- predict(rfms[[i]], training[training$set==i,])
}
training$rfprediction <- round(training$rfscorehat)
training$rfresidual <- training$grade - training$rfprediction

kappasrfm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$rfprediction))
print(round(unlist(kappasrfm), 4))
print(MeanQuadraticWeightedKappa(kappasrfm))
kappasrfm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$rfprediction[X$holdout==1]))
print(round(unlist(kappasrfm), 4))
print(MeanQuadraticWeightedKappa(kappasrfm))

qplot(training$grade, training$rfprediction, position=position_jitter(width=.2, height=.2))
qplot(grade - rfprediction, facets=set~., data=training)



training[training$holdout==1 && training$grade != training$rfprediction,]

table(training$grade)



##gbm
print("GBM!")
library(gbm)
gbms = list()
best.iter = list()
for (i in 1:max(training$set)) {
  print(paste(c("GBM: set", i)))
  gbms[[i]] <- gbm(grade ~ num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_superlatives + avg_length + spell_pct + rel_distinct + nouns_to_adjs + nouns_to_verbs + nouns_to_adverbs + adjs_to_verbs + has_semicolon + has_exclamation + has_questionmark + has_questionmark + num_quotes + proper_quote_punc
                   +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
                   +pos_CC+pos_CD+pos_DT+pos_EX+pos_IN+pos_JJ+pos_JJR+pos_JJS+pos_MD+pos_NN+pos_NNP+pos_NNS+pos_PDT+pos_POS+pos_PRP+`pos_PRP.`+pos_RB+pos_RBR+pos_RBS+pos_RP+pos_TO+pos_VB+pos_VBD+pos_VBG+pos_VBN+pos_VBP+pos_VBZ+pos_WDT+pos_WP+pos_WRB
                   ,
                   data=training[training$set==i,], distribution="gaussian", verbose=FALSE,
                   n.trees=25000, cv.folds=8, interaction.depth=5, n.minobsinnode=15)
  best.iter[[i]] <- gbm.perf(gbms[[i]], method="cv")
  print(best.iter[[i]])
  training$gbmscore[training$set==i] <- predict.gbm(gbms[[i]], training[training$set==i,], best.iter[[i]])
}
training$gbmprediction <- round(training$gbmscore)
training$gbmresidual <- training$grade - training$gbmprediction

kappasgbm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$gbmprediction))
#kappasgbm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$gbmprediction[X$holdout==1]))
print(round(unlist(kappasgbm), 4))
print(MeanQuadraticWeightedKappa(kappasgbm))

##Save Models
save(models, rfms, gbms, best.iter, file="models.Rdata")

gbmsumsdf <- lapply(lapply(gbms, summary), as.data.frame)
gbmsumsdf[[1]] <- rename(gbmsumsdf[[1]], c(rel.inf="rel.inf.1"))
gbmsumsdf[[2]] <- rename(gbmsumsdf[[2]], c(rel.inf="rel.inf.2"))
gbmsumsdf[[3]] <- rename(gbmsumsdf[[3]], c(rel.inf="rel.inf.3"))
gbmsumsdf[[4]] <- rename(gbmsumsdf[[4]], c(rel.inf="rel.inf.4"))
gbmsumsdf[[5]] <- rename(gbmsumsdf[[5]], c(rel.inf="rel.inf.5"))
rel.inf <- merge_recurse(gbmsumsdf, by="var")
rel.inf$avg <- rowMeans(rel.inf[,-1])
print(rel.inf[order(-rel.inf$avg), ])








##  SCORE  ##
#############
testing <- read.csv("test_tagged_tfidf.csv")

testing$spell_mistakes <- testing$num_words - testing$num_correctly_spelled
testing$avg_length <- testing$num_chars / testing$num_words
testing$avg_syls <- testing$num_syl / testing$num_words
testing$spell_pct <- testing$num_correctly_spelled / testing$num_words
testing$has_a_quote <- as.numeric(testing$num_quotes >= 2)
testing$rel_distinct <- testing$distinct_words / testing$num_words
testing$nouns_to_adjs <- testing$num_nouns/testing$num_adjectives
testing$nouns_to_adjs[is.nan(testing$nouns_to_adjs) | is.infinite(testing$nouns_to_adjs)] <- 0
testing$nouns_to_verbs <- testing$num_nouns/testing$num_verbs
testing$nouns_to_verbs[is.nan(testing$nouns_to_verbs) | is.infinite(testing$nouns_to_verbs)] <- 0
testing$nouns_to_adverbs <- testing$num_nouns/testing$num_adverbs
testing$nouns_to_adverbs[is.nan(testing$nouns_to_adverbs) | is.infinite(testing$nouns_to_adverbs)] <- 0
testing$adjs_to_verbs <- testing$num_adjectives/testing$num_verbs
testing$adjs_to_verbs[is.nan(testing$adjs_to_verbs) | is.infinite(testing$adjs_to_verbs)] <- 0


#section just temp for this data run
testing$pos_WRB[is.na(testing$pos_WRB)] <- 0
testing$pos_PRP.[is.na(testing$pos_PRP.)] <- 0
testing$pos_VBG[is.na(testing$pos_VBG)] <- 0
testing$pos_FW[is.na(testing$pos_FW)] <- 0
testing$pos_CC[is.na(testing$pos_CC)] <- 0
testing$pos_PDT[is.na(testing$pos_PDT)] <- 0
testing$pos_RBS[is.na(testing$pos_RBS)] <- 0
testing$pos_PRP[is.na(testing$pos_PRP)] <- 0
testing$pos_CD[is.na(testing$pos_CD)] <- 0
testing$`pos_WP.`[is.na(testing$pos_WP.)] <- 0
testing$pos_VBP[is.na(testing$pos_VBP)] <- 0
testing$pos_VBN[is.na(testing$pos_VBN)] <- 0
testing$pos_EX[is.na(testing$pos_EX)] <- 0
testing$pos_JJ[is.na(testing$pos_JJ)] <- 0
testing$pos_IN[is.na(testing$pos_IN)] <- 0
testing$pos_WP[is.na(testing$pos_WP)] <- 0
testing$pos_VBZ[is.na(testing$pos_VBZ)] <- 0
testing$pos_DT[is.na(testing$pos_DT)] <- 0
testing$pos_MD[is.na(testing$pos_MD)] <- 0
testing$pos_NNPS[is.na(testing$pos_NNPS)] <- 0
testing$pos_RP[is.na(testing$pos_RP)] <- 0
testing$pos_NN[is.na(testing$pos_NN)] <- 0
testing$pos_POS[is.na(testing$pos_POS)] <- 0
testing$pos_RBR[is.na(testing$pos_RBR)] <- 0
testing$pos_VBD[is.na(testing$pos_VBD)] <- 0
testing$pos_JJS[is.na(testing$pos_JJS)] <- 0
testing$pos_JJR[is.na(testing$pos_JJR)] <- 0
testing$pos_VB[is.na(testing$pos_VB)] <- 0
testing$pos_TO[is.na(testing$pos_TO)] <- 0
testing$pos_UH[is.na(testing$pos_UH)] <- 0
testing$pos_LS[is.na(testing$pos_LS)] <- 0
testing$pos_RB[is.na(testing$pos_RB)] <- 0
testing$pos_WDT[is.na(testing$pos_WDT)] <- 0
testing$pos_NNS[is.na(testing$pos_NNS)] <- 0
testing$pos_NNP[is.na(testing$pos_NNP)] <- 0

for (i in 1:max(testing$set)) {
  testing$scorehat[testing$set==i] <- predict(models[[i]], testing[testing$set==i,])
  testing$rfscorehat[testing$set==i] <- predict(rfms[[i]], testing[testing$set==i,])
  testing$gbmscorehat[testing$set==i] <-predict.gbm(gbms[[i]], testing[testing$set==i,], best.iter[[i]])
}
testing$scorehat[is.na(testing$scorehat)] = 1
testing$rfscorehat[is.na(testing$rfscorehat)] = 1
testing$gbmscorehat[is.na(testing$gbmscorehat)] = 1
testing$prediction <- round(testing$scorehat)
testing$rfprediction <- round(testing$rfscorehat)
testing$gbmprediction <- round(testing$gbmscore)

# testing$rfascorehat <- predict(rf_alt, testing)
# testing$rfaprediction <- as.numeric(levels(testing$rfascorehat))[testing$rfascorehat]
# testing$gbmascore <- predict.gbm(gbm_alt, testing, best.iter.gbm_alt)
# testing$gbmaprediction <- as.numeric(levels(testing$gbmascore))[testing$gbmascore]

testing$weight = 1
write.csv(testing[, c("id", "set", "weight", "prediction")], "testing_predicted_lm.csv", row.names=FALSE, na="")
write.csv(testing[, c("id", "set", "weight", "rfprediction")], "testing_predicted_rf.csv", row.names=FALSE, na="")
#write.csv(testing[, c("id", "set", "weight", "rfaprediction")], "testing_predicted_rfa.csv", row.names=FALSE, na="")
write.csv(testing[, c("id", "set", "weight", "gbmprediction")], "testing_predicted_gbm.csv", row.names=FALSE, na="")
#write.csv(testing[, c("id", "set", "weight", "gbmaprediction")], "testing_predicted_gbma.csv", row.names=FALSE, na="")
