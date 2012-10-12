training <- read.csv("training_set_rel3_tagged.csv")

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

model <- lm(domain1_score ~ num_chars + log(num_chars) + avg_length + avg_syls  + spell_pct + starts_with_dear + spell_mistakes, data=training)
model
summary(model)

step <- stepAIC(model, direction="both")
step$anova