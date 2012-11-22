source("ASAP-AES/Evaluation_Metrics/R/quadratic_weighted_kappa.R")
require(plyr)
require(MASS)
require(ggplot2)

training <- read.csv("train_tagged_tfidf.csv")
training <- training[!is.na(training$grade),]
nrow(training)

essays <- training$essay
training$essay = 0
training$spell_mistakes <- training$num_words - training$num_correctly_spelled
training$avg_length <- training$num_chars / training$num_words
training$avg_syls <- training$num_syl / training$num_words
training$spell_pct <- training$num_correctly_spelled / training$num_words

training$holdout <- 0
training$holdout[sample(nrow(training), as.integer(nrow(training)*.2))] <- 1

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
require(randomForest)
rfms = list()
for (i in 1:max(training$set)) {
    rfms[[i]] <- randomForest(grade ~ num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + distinct_words + starts_with_dear + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_conjunctions + num_prepositions + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs)
        +tfidfpca_0+tfidfpca_1+tfidfpca_2+tfidfpca_3+tfidfpca_4+tfidfpca_5+tfidfpca_6+tfidfpca_7+tfidfpca_8+tfidfpca_9+tfidfpca_10+tfidfpca_11+tfidfpca_12+tfidfpca_13+tfidfpca_14+tfidfpca_15+tfidfpca_16+tfidfpca_17+tfidfpca_18+tfidfpca_19+tfidfpca_20+tfidfpca_21+tfidfpca_22+tfidfpca_23+tfidfpca_24+tfidfpca_25+tfidfpca_26+tfidfpca_27+tfidfpca_28+tfidfpca_29+tfidfpca_30+tfidfpca_31+tfidfpca_32+tfidfpca_33+tfidfpca_34+tfidfpca_35+tfidfpca_36+tfidfpca_37+tfidfpca_38+tfidfpca_39+tfidfpca_40+tfidfpca_41+tfidfpca_42+tfidfpca_43+tfidfpca_44+tfidfpca_45+tfidfpca_46+tfidfpca_47+tfidfpca_48+tfidfpca_49
        ,
        data=training[training$set == i & training$holdout==0,], importance=TRUE, ntree=3000, mtry=5)
    training$rfscorehat[training$set==i] <- predict(rfms[[i]], training[training$set==i,])
}
training$rfprediction <- round(training$rfscorehat)

kappasrfm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$rfprediction))
kappasrfm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$rfprediction[X$holdout==1]))
print(round(unlist(kappasrfm), 4))
print(MeanQuadraticWeightedKappa(kappasrfm))

qplot(training$grade, training$rfprediction, position=position_jitter(width=.2, height=.2))
qplot(training$grade - training$rfprediction)

training[training$holdout==1 && training$grade != training$rfprediction,]


table(training$grade)



##SCORE
testing <- read.csv("test_tagged_tfidf.csv")
testing <- testing[order(testing$set, testing$essay_id), ]

essays <- testing$essay
testing$essay = 0
testing$spell_mistakes <- testing$num_words - testing$num_correctly_spelled
testing$avg_length <- testing$num_chars / testing$num_words
testing$avg_syls <- testing$num_syl / testing$num_words
testing$spell_pct <- testing$num_correctly_spelled / testing$num_words

for (i in 1:max(testing$set)) {
    testing$scorehat[testing$set==i] <- predict(models[[i]], testing[testing$set==i,])
    testing$rfscorehat[testing$set==i] <- predict(rfms[[i]], testing[testing$set==i,])
}
testing$scorehat[is.na(testing$scorehat)] = 1
testing$rfscorehat[is.na(testing$rfscorehat)] = 1
testing$prediction <- round(testing$scorehat)
testing$rfprediction <- round(testing$rfscorehat)

testing$weight = 1
write.csv(testing[, c("id", "set", "weight", "prediction")], "testing_predicted_lm.csv", row.names=FALSE, na="")
write.csv(testing[, c("id", "set", "weight", "rfprediction")], "testing_predicted_rf.csv", row.names=FALSE, na="")

testing[testing$id==11832,]