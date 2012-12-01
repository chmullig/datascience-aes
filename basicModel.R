source("ASAP-AES/Evaluation_Metrics/R/quadratic_weighted_kappa.R")
require(plyr)
require(MASS)
require(ggplot2)

training <- read.csv("train_tagged_tfidf.csv")
training_ner <- read.csv("train_ner.csv")
training_ner$essay <- NULL
training <- merge(training, training_ner, by=c("id", "set", "rate1", "rate2", "grade"))
nrow(training)

training$spell_mistakes <- training$num_words - training$num_correctly_spelled
training$avg_length <- training$num_chars / training$num_words
training$avg_syls <- training$num_syl / training$num_words
training$spell_pct <- training$num_correctly_spelled / training$num_words

training$holdout <- 0
training$holdout[sample(nrow(training), as.integer(nrow(training)*.1))] <- 1

qplot(grade, data=training, facets=set~., binwidth=1)
#cor(training, use="complete.obs")



#Run Linear Model
models <- dlply(training[training$holdout==0,], .(set), lm,
                formula = grade ~ num_chars + log(num_chars) + avg_length + avg_syls  + spell_pct + starts_with_dear + spell_mistakes + sentance_length + num_superlatives + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs) + has_semicolon + has_exclamation + has_questionmark + num_foreign 
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
    rfms[[i]] <- randomForest(grade ~ num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_conjunctions + num_prepositions + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs)  + has_semicolon + has_exclamation + has_questionmark + num_foreign
        +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
        +ner_person+ner_organization+ner_location+ner_date+ner_time+ner_money+ner_percent+ner_caps+ner_num+ner_month
        ,
        data=training[training$set == i & training$holdout==0,], importance=TRUE, ntree=2000)
    training$rfscorehat[training$set==i] <- predict(rfms[[i]], training[training$set==i,])
}
training$rfprediction <- round(training$rfscorehat)
training$rfresidual <- training$grade - training$rfprediction

kappasrfm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$rfprediction))
kappasrfm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$rfprediction[X$holdout==1]))
print(round(unlist(kappasrfm), 4))
print(MeanQuadraticWeightedKappa(kappasrfm))

qplot(training$grade, training$rfprediction, position=position_jitter(width=.2, height=.2))
qplot(training$grade - training$rfprediction)


#rf alt
rf_alt <- randomForest(as.factor(grade) ~ set + num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_conjunctions + num_prepositions + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs)  + has_semicolon + has_exclamation + has_questionmark + num_foreign
            +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
            +ner_person+ner_organization+ner_location+ner_date+ner_time+ner_money+ner_percent+ner_caps+ner_num+ner_month
            ,
            data=training[training$holdout==0,], importance=TRUE, ntree=5000, nodesize=10, mtry=10)
training$rfascorehat <- predict(rf_alt, training)
training$rfaprediction <- as.numeric(levels(training$rfascorehat))[training$rfascorehat]

kappasrfa <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$rfaprediction))
print(paste(c("RF Alt, Kappas, ALL:", round(unlist(kappasrfa), 4))))
kappasrfa <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$rfaprediction[X$holdout==1]))
print(paste(c("RF Alt, Kappas, Holdout:", round(unlist(kappasrfa), 4))))
print(paste(c("RF Alt, MQWK:", MeanQuadraticWeightedKappa(kappasrfa))))


training[training$holdout==1 && training$grade != training$rfprediction,]

table(training$grade)



##gbm
print("GBM!")
library(gbm)
gbms = list()
best.iter = list()
for (i in 1:max(training$set)) {
    print(paste(c("GBM: set", i)))
#    gbms[[i]] <- gbm(grade ~ num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_conjunctions + num_prepositions + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs)  + has_semicolon + has_exclamation + has_questionmark + num_foreign
#                +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
#                +ner_person+ner_organization+ner_location+ner_date+ner_time+ner_money+ner_percent+ner_caps+ner_num+ner_month
#                ,
#              data=training[training$set==i,], distribution="gaussian", n.trees=25000, cv.folds=10)
#    gbms[[i]] <- gbm.more(gbms[[i]], 5000)
#    best.iter[[i]] <- gbm.perf(gbms[[i]], method="cv")
#    print(best.iter[[i]])
    training$gbmscore[training$set==i] <- predict.gbm(gbms[[i]], training[training$set==i,], best.iter[[i]])
}
training$gbmprediction <- round(training$gbmscore)
training$gbmresidual <- training$grade - training$gbmprediction

kappasgbm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$gbmprediction))
#kappasgbm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$gbmprediction[X$holdout==1]))
print(round(unlist(kappasgbm), 4))
print(MeanQuadraticWeightedKappa(kappasgbm))


ggplot(training, aes(x=grade), facets=set~., binwidth=1) + geom_histogram(


gbm_alt <- gbm(grade ~ num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_conjunctions + num_prepositions + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs)  + has_semicolon + has_exclamation + has_questionmark + num_foreign
                 +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
                 +ner_person+ner_organization+ner_location+ner_date+ner_time+ner_money+ner_percent+ner_caps+ner_num+ner_month
                 ,
                 data=training, distribution="gaussian", n.trees=25000, cv.folds=10)
gbm_alt <- gbm.more(gbm_alt, 20000)
best.iter.gbm_alt <- gbm.perf(gbm_alt, method="cv")
training$gbmascore <- predict.gbm(gbm_alt, training, best.iter.gbm_alt)
training$gbmaprediction <- round(training$gbmascore)

kappasgbma <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$gbmaprediction))
kappasgbma <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$gbmaprediction[X$holdout==1]))
print(round(unlist(kappasgbma), 4))
print(MeanQuadraticWeightedKappa(kappasgbma))





##SCORE
testing <- read.csv("test_tagged_tfidf.csv")
testing_ner <- read.csv("test_ner.csv")
testing_ner$essay <- NULL
testing <- merge(testing, testing_ner, by=c("id", "set", "rate1", "rate2", "grade"))

essays <- testing$essay
testing$essay = 0
testing$spell_mistakes <- testing$num_words - testing$num_correctly_spelled
testing$avg_length <- testing$num_chars / testing$num_words
testing$avg_syls <- testing$num_syl / testing$num_words
testing$spell_pct <- testing$num_correctly_spelled / testing$num_words

for (i in 1:max(testing$set)) {
    #testing$scorehat[testing$set==i] <- predict(models[[i]], testing[testing$set==i,])
    #testing$rfscorehat[testing$set==i] <- predict(rfms[[i]], testing[testing$set==i,])
    testing$gbmscorehat[testing$set==i] <-predict.gbm(gbms[[i]], testing[testing$set==i,], best.iter.i)
}
testing$scorehat[is.na(testing$scorehat)] = 1
testing$rfscorehat[is.na(testing$rfscorehat)] = 1
testing$gbmscorehat[is.na(testing$gbmscorehat)] = 1
testing$prediction <- round(testing$scorehat)
testing$rfprediction <- round(testing$rfscorehat)
testing$gbmprediction <- round(testing$gbmscore)

testing$rfascorehat <- predict(rf_alt, testing)
testing$rfaprediction <- as.numeric(levels(testing$rfascorehat))[testing$rfascorehat]
testing$gbmascore <- predict.gbm(gbm_alt, testing, best.iter.gbm_alt)
testing$gbmaprediction <- as.numeric(levels(testing$gbmascore))[testing$gbmascore]

testing$weight = 1
write.csv(testing[, c("id", "set", "weight", "prediction")], "testing_predicted_lm.csv", row.names=FALSE, na="")
write.csv(testing[, c("id", "set", "weight", "rfprediction")], "testing_predicted_rf.csv", row.names=FALSE, na="")
write.csv(testing[, c("id", "set", "weight", "rfaprediction")], "testing_predicted_rfa.csv", row.names=FALSE, na="")
write.csv(testing[, c("id", "set", "weight", "gbmprediction")], "testing_predicted_gbm.csv", row.names=FALSE, na="")
write.csv(testing[, c("id", "set", "weight", "gbmaprediction")], "testing_predicted_gbma.csv", row.names=FALSE, na="")