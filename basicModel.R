source("ASAP-AES/Evaluation_Metrics/R/quadratic_weighted_kappa.R")
require(plyr)
require(MASS)
require(ggplot2)

training <- read.csv("train_tagged.csv")
training <- training[!is.na(training$grade),]
nrow(training)

essays <- training$essay
training$essay = 0
training$spell_mistakes <- training$num_words - training$num_correctly_spelled
training$avg_length <- training$num_chars / training$num_words
training$avg_syls <- training$num_syl / training$num_words
training$spell_pct <- training$num_correctly_spelled / training$num_words

training$grade
training$grade_log <- log(training$grade)
qplot(grade, data=training, facets=set~., binwidth=1)


cor(training, use="complete.obs")

training$holdout <- 0
training$holdout[sample(nrow(training), as.integer(nrow(training)*.2))] <- 1


models <- dlply(training[training$holdout==0,], .(set), lm, formula = grade ~ num_chars + log(num_chars) + avg_length + avg_syls  + spell_pct + starts_with_dear + spell_mistakes + sentance_length + num_superlatives + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs) + has_semicolon + has_exclamation + has_questionmark + num_foreign)
lapply(models, summary)
#steps <- lapply(models, function(X) stepAIC(X, direction="both"))
#lapply(steps, anova)
#mapply(predict, models, ddply(training, .(set)))

#m <- lm(grade ~ num_chars + avg_syls  + spell_mistakes + sentance_length +  (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs),
#    data=training[training$set==3, ])
#stepAIC(m, direction="both")

#replace with something more elegant :(
for (i in 1:max(training$set)) {
    training$scorehat[training$set==i] <- predict(models[[i]], training[training$set==i,])
}
training$prediction <- round(training$scorehat)
training$residual <- training$grade - training$prediction
#training$prediction[training$set==i && training$prediction < min(training$grade[training$set==i])] <- min(training$grade[training$set==i])
#training$prediction[training$set==i && training$prediction > max(training$grade[training$set==i])] <- max(training$grade[training$set==i])


kappas <- dlply(training[!is.na(training$grade),], .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$prediction[X$holdout==1]))
kappas
MeanQuadraticWeightedKappa(kappas)

table(training$set[training$holdout==1])

plot(training$grade, training$prediction)
hist(training$residual[training$holdout==1])

#dlply(training, .(set), function(x) mean(abs(x$residual)))
summary(training)


#randomForest
require(randomForest)
#rfm <- randomForest(grade ~ num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + starts_with_dear + distinct_words + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_conjunctions + num_prepositions + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs),
#              strata=set, data=training[training$holdout==0,], importance=TRUE, ntree=2500, mtry=5)
#training$rfscorehat <- predict(rfm, training)


rfms = list()
for (i in 1:max(training$set)) {
    rfms[[i]] <- randomForest(grade ~ num_chars + num_sents + num_words + num_syl + sentance_length + avg_syls + spell_mistakes + fk_grade_level + distinct_words + end_with_preposition + num_nouns + num_verbs + num_adjectives + num_adverbs + num_conjunctions + num_prepositions + num_superlatives + avg_length + spell_pct + (distinct_words / num_words) + (num_nouns/num_adjectives) + (num_nouns/num_verbs) + (num_nouns/num_adverbs),
        data=training[training$set == i & training$holdout==0,], importance=TRUE, ntree=2000, mtry=5)
    training$rfscorehat[training$set==i] <- predict(rfms[[i]], training[training$set==i,])
}
training$rfprediction <- round(training$rfscorehat)
rfms

kappasrfm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$rfprediction))
kappasrfm <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$rfprediction[X$holdout==1]))
kappasrfm
MeanQuadraticWeightedKappa(kappasrfm)


training$avgpred <- round((training$scorehat + training$rfscorehat)/2)
kappasavg <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade, X$avgpred))
kappasavg <- dlply(training, .(set), function(X) ScoreQuadraticWeightedKappa(X$grade[X$holdout==1], X$avgpred[X$holdout==1]))
kappasavg
MeanQuadraticWeightedKappa(kappasavg)


training[training$holdout==1 && training$grade != training$rfprediction,]



##SCORE
testing <- read.csv("test_tagged.csv")
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
#testing$scorehat[is.na(testing$scorehat)] = 1
testing$prediction <- round(testing$scorehat)
testing$rfprediction <- round(testing$rfscorehat)

testing$weight = 1
write.csv(testing[, c("id", "set", "weight", "prediction")], "testing_predicted_lm.csv", row.names=FALSE, na="")
write.csv(testing[, c("id", "set", "weight", "rfprediction")], "testing_predicted_rf.csv", row.names=FALSE, na="")

