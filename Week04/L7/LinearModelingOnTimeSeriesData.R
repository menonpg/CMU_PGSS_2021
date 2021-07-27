setwd("/cloud/project/TimeSeriesAnalysis/")

library(readr)
df_original <- read_csv("session4_train_2018.csv")
plot(df_original$S, type="l")


fs = 256 # 256 samples every 1 sec
df_original$time <- seq(from = 0, length.out = NROW(df_original),  by = 1/fs)

plot(df_original$time, df_original$S, xlab="Time in secs", ylab= "EEG voltage in mV", type="l")

df_original$seizure <-as.factor(as.numeric(df_original$time>62))

table(df_original$seizure)


# EDA:
boxplot(S ~ seizure, data=df_original )


# Feature engineering:

# library(roll)
# rolling standard deviations with complete windows
# roll_sd(df_original$S, width = 5)

width = 5
sdList <- c()
for (i in width:NROW(df_original)) {
  sdList <- c(sdList, sd(df_original$S[(i-width+1):i]))
}

df_original$SD5 <- NA
df_original$SD5[width:NROW(df_original)] <- sdList

boxplot(SD5 ~ seizure, data=df_original )

width = 10
sdList <- c()
for (i in width:NROW(df_original)) {
  sdList <- c(sdList, sd(df_original$S[(i-width+1):i]))
}

df_original$SD10 <- NA
df_original$SD10[width:NROW(df_original)] <- sdList

boxplot(SD10 ~ seizure, data=df_original )


width = 15
sdList <- c()
for (i in width:NROW(df_original)) {
  sdList <- c(sdList, sd(df_original$S[(i-width+1):i]))
}

df_original$SD15 <- NA
df_original$SD15[width:NROW(df_original)] <- sdList

boxplot(SD15 ~ seizure, data=df_original )


## Lets model this time-series to classify it based on the features we have engineered

model <- glm(formula = seizure~., data = df_original[,-c(1,2,4,6)], family = "binomial")
summary(model)

predictions <- predict(model, df_original[,-c(1,2,4,6)])

plot(density(na.omit(exp(predictions))))  # plot of the "ODDS" of each row being classified for seizure = 1

# ODDS = p / (1 - p); where ODDS = exp(predictions)
# => (1 - p)*ODDS = p
# => p = ODDS/(1+ODDS)

# Probability of each row  corresponding to seizure = 1
plot(density(na.omit(exp(predictions))/(1+na.omit(exp(predictions)))))  # plot of the "ODDS" of each row being classified for the law being in effect
summary(na.omit(exp(predictions))/(1+na.omit(exp(predictions))))


# boxplot to find the optimal probability cutoff:
boxplot((exp(predictions))/(1+(exp(predictions))) ~ df_original$seizure)

table(df_original$seizure, (exp(predictions))/(1+(exp(predictions)))>0.5)



##  Optimizing cutoffs 
## Is 0.5 the optimal threhsold?
library(ROCR)
PredictedProbas <- (exp(predictions))/(1+(exp(predictions)))
PredictedProbas [is.na(PredictedProbas)] <- 0

pred <- prediction(PredictedProbas, relevel(as.factor(df_original$seizure), ref = "0"))
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))
# print(1-opt.cut(roc.perf, pred)[3]) 

optThreshold <- opt.cut(roc.perf, pred)[3]
df_original$OptimalPredictedClass <- ifelse(PredictedProbas>optThreshold, "True", "False")
table(df_original$seizure, df_original$OptimalPredictedClass)

