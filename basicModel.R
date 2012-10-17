source("ASAP-AES/Evaluation_Metrics/R/quadratic_weighted_kappa.R")
require(plyr)
require(MASS)

training <- read.csv("train_tagged.csv")
training <- training[order(training$essay_set, training$essay_id), ]

essays <- training$essay
training$essay = 0

training$spell_mistakes <- training$num_words - training$num_correctly_spelled
training$avg_length <- training$num_chars / training$num_words
training$avg_syls <- training$num_syl / training$num_words
training$spell_pct <- training$num_correctly_spelled / training$num_words

training$domain1_score_log <- log(training$domain1_score)
hist(training$domain1_score_log)
plot(training$spell_pct, training$domain1_score)

cor(training, use="complete.obs")

training$holdout <- 0
training$holdout[sample(nrow(training), as.integer(nrow(training)*.1))] <- 1



models <- dlply(training[training$holdout==0,], .(essay_set), lm, formula = domain1_score ~ num_chars + log(num_chars) + avg_length + avg_syls  + spell_pct + starts_with_dear + spell_mistakes + sentance_length + num_superlatives + (distinct_words / num_words))
lapply(models, summary)
#steps <- lapply(models, stepAIC, direction="both")
#lapply(steps, anova)
#mapply(predict, models, ddply(training, .(essay_set)))


#replace with something more elegant :(
for (i in 1:max(training$essay_set)) {
    training$scorehat[training$essay_set==i] <- mean(training$domain1_score[training$essay_set==i])
    training$scorehat[training$essay_set==i] <- predict(models[[i]], training[training$essay_set==i,])
    training$scorehat[training$essay_set==i && is.na(training$scorehat)] <- mean(training$domain1_score[training$essay_set==i])
}

training$prediction <- round(training$scorehat)
training$residual <- training$domain1_score - training$prediction
#training$prediction[training$essay_set==i && training$prediction < min(training$domain1_score[training$essay_set==i])] <- min(training$domain1_score[training$essay_set==i])
#training$prediction[training$essay_set==i && training$prediction > max(training$domain1_score[training$essay_set==i])] <- max(training$domain1_score[training$essay_set==i])

hist(training$residual)
table(training$domain1_score, training$prediction, useNA="always")



kappas <- dlply(training, .(essay_set), function(X) ScoreQuadraticWeightedKappa(X$domain1_score, X$prediction))
kappas
MeanQuadraticWeightedKappa(kappas)

table(training$essay_set[training$holdout==0])

plot(training$domain1_score, training$prediction)
hist(training$residual)
