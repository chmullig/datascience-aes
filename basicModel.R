source("ASAP-AES/Evaluation_Metrics/R/quadratic_weighted_kappa.R")
require(plyr)
require(MASS)
require(ggplot2)

library(doMC)
library(foreach)
library(caret)
registerDoMC()


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

qplot(grade, data=training, facets=set~., binwidth=1)


#Run Linear Model
models <- dlply(training[training$holdout==0,], .(set), lm,
                formula = grade ~ num_chars + log(num_chars) + avg_length + avg_syls  + spell_pct + starts_with_dear + spell_mistakes + sentance_length + num_superlatives + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs) + has_semicolon + has_exclamation + has_questionmark + num_quotes + proper_quote_punc + has_a_quote + pos_CC
                +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
                )
lapply(models, summary)

for (i in 1:max(training$set)) {
    training$scorehat[training$set==i] <- predict(models[[i]], training[training$set==i,])
}
training$prediction <- round(training$scorehat)
training$residual <- training$grade - training$prediction
#training$prediction[training$set==i && training$prediction < min(training$grade[training$set==i])] <- min(training$grade[training$set==i])
#training$prediction[training$set==i && training$prediction > max(training$grade[training$set==i])] <- max(training$grade[training$set==i])
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
    rfms[[i]] <- randomForest(grade ~ num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs)  + has_semicolon + has_exclamation + has_questionmark + has_questionmark + num_quotes + proper_quote_punc + has_a_quote
        +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
        +ner_person+ner_organization+ner_location+ner_date+ner_time+ner_money+ner_percent+ner_caps+ner_num+ner_month
        +pos_CC+pos_CD+pos_DT+pos_EX+pos_IN+pos_JJ+pos_JJR+pos_JJS+pos_MD+pos_NN+pos_NNP+pos_NNS+pos_PDT+pos_POS+pos_PRP+`pos_PRP.`+pos_RB+pos_RBR+pos_RBS+pos_RP+pos_TO+pos_VB+pos_VBD+pos_VBG+pos_VBN+pos_VBP+pos_VBZ+pos_WDT+pos_WP+pos_WRB
          
          #+ ner_caps_pct+ ner_date_pct+ ner_location_pct+ ner_money_pct+ ner_month_pct+ ner_num_pct+ ner_organization_pct+ ner_percent_pct+ ner_person_pct+ ner_time_pct
          #+ num_adjectives_pct+ num_adverbs_pct+ num_nouns_pct+ num_quotes_pct+ num_superlatives_pct+ num_syl_pct+ num_verbs_pct
          #+ pos_CC_pct+ pos_CD_pct+ pos_DT_pct+ pos_EX_pct+ pos_FW_pct+ pos_IN_pct+ pos_JJ_pct+ pos_JJR_pct+ pos_JJS_pct+ pos_LS_pct+ pos_MD_pct+ pos_NN_pct+ pos_NNP_pct+ pos_NNPS_pct+ pos_NNS_pct+ pos_PDT_pct+ pos_POS_pct+ pos_PRP_pct+ pos_RB_pct+ pos_RBR_pct+ pos_RBS_pct+ pos_RP_pct+ pos_TO_pct+ pos_UH_pct+ pos_VB_pct+ pos_VBD_pct+ pos_VBG_pct+ pos_VBN_pct+ pos_VBP_pct+ pos_VBZ_pct+ pos_WDT_pct+ pos_WP_pct+ pos_WRB_pct
    ,
        data=training[training$set == i & training$holdout==0,], importance=TRUE, ntree=5000)
    training$rfscorehat[training$set==i] <- predict(rfms[[i]], training[training$set==i,])
}
training$rfprediction <- round(training$rfscorehat)
training$rfresidual <- training$grade - training$rfprediction

kappasrfm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$rfprediction))
kappasrfm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$rfprediction[X$holdout==1]))
print(round(unlist(kappasrfm), 4))
print(MeanQuadraticWeightedKappa(kappasrfm))

qplot(training$grade, training$rfprediction, position=position_jitter(width=.2, height=.2))
qplot(grade - rfprediction, facets=set~., data=training)


#rf alt
# rf_alt <- randomForest(as.factor(grade) ~ set + num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_conjunctions + num_prepositions + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs)  + has_semicolon + has_exclamation + has_questionmark + num_foreign
#                        +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
#                        +ner_person+ner_organization+ner_location+ner_date+ner_time+ner_money+ner_percent+ner_caps+ner_num+ner_month
#                        ,
#                        data=training[training$holdout==0,], importance=TRUE, ntree=5000, nodesize=10, mtry=10)
# training$rfascorehat <- predict(rf_alt, training)
# training$rfaprediction <- as.numeric(levels(training$rfascorehat))[training$rfascorehat]
# 
# kappasrfa <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$rfaprediction))
# print(paste(c("RF Alt, Kappas, ALL:", round(unlist(kappasrfa), 4))))
# kappasrfa <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$rfaprediction[X$holdout==1]))
# print(paste(c("RF Alt, Kappas, Holdout:", round(unlist(kappasrfa), 4))))
# print(paste(c("RF Alt, MQWK:", MeanQuadraticWeightedKappa(kappasrfa))))


training[training$holdout==1 && training$grade != training$rfprediction,]

table(training$grade)



##gbm
print("GBM!")
library(gbm)
gbms = list()
best.iter = list()
for (i in 1:max(training$set)) {
    print(paste(c("GBM: set", i)))
   gbms[[i]] <- gbm(grade ~ num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs)  + has_semicolon + has_exclamation + has_questionmark + has_questionmark + num_quotes + proper_quote_punc + has_a_quote
                    +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
                    +ner_person+ner_organization+ner_location+ner_date+ner_time+ner_money+ner_percent+ner_caps+ner_num+ner_month
                    +pos_CC+pos_CD+pos_DT+pos_EX+pos_IN+pos_JJ+pos_JJR+pos_JJS+pos_MD+pos_NN+pos_NNP+pos_NNS+pos_PDT+pos_POS+pos_PRP+`pos_PRP.`+pos_RB+pos_RBR+pos_RBS+pos_RP+pos_TO+pos_VB+pos_VBD+pos_VBG+pos_VBN+pos_VBP+pos_VBZ+pos_WDT+pos_WP+pos_WRB
                ,
            data=training[training$set==i,], distribution="gaussian", verbose=FALSE,
            n.trees=50000, cv.folds=8, interaction.depth=3)
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

# 
# gbm_alt <- gbm(grade ~ num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_conjunctions + num_prepositions + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs)  + has_semicolon + has_exclamation + has_questionmark + num_foreign
#                  +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
#                  +ner_person+ner_organization+ner_location+ner_date+ner_time+ner_money+ner_percent+ner_caps+ner_num+ner_month
#                  ,
#                  data=training, distribution="gaussian", n.trees=25000, cv.folds=10)
# gbm_alt <- gbm.more(gbm_alt, 20000)
# best.iter.gbm_alt <- gbm.perf(gbm_alt, method="cv")
# training$gbmascore <- predict.gbm(gbm_alt, training, best.iter.gbm_alt)
# training$gbmaprediction <- round(training$gbmascore)
# 
# kappasgbma <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$gbmaprediction))
# kappasgbma <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$gbmaprediction[X$holdout==1]))
# print(round(unlist(kappasgbma), 4))
# print(MeanQuadraticWeightedKappa(kappasgbma))


##Save Models
save(models, rfms, gbms, best.iter, file="models.Rdata")




##SCORE
testing <- read.csv("test_tagged_tfidf.csv")

testing$spell_mistakes <- testing$num_words - testing$num_correctly_spelled
testing$avg_length <- testing$num_chars / testing$num_words
testing$avg_syls <- testing$num_syl / testing$num_words
testing$spell_pct <- testing$num_correctly_spelled / testing$num_words
testing$has_a_quote <- as.numeric(testing$num_quotes >= 2)


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