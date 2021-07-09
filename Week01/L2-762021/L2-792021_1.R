# Where to find datasets?
library(datasets)

seatbelts <- datasets::Seatbelts
class(seatbelts)

df <- as.data.frame(seatbelts)
# df$Date <- rownames(seatbelts)
View(df)

# install.packages("zoo")
# install.packages("xts")
library(zoo)
library(xts)
# df$Date <- as.yearmon(time(seatbelts))
df$Date <- as.Date(time(seatbelts))


df$law <- as.factor(df$law)
sapply(df, class)

boxplot(DriversKilled ~ law, data = df)
t.test(DriversKilled ~ law, data = df)

boxplot(VanKilled ~ law, data = df)
t.test(VanKilled ~ law, data = df)


## If the hypothesis that drivers killed are fewer when the seatbelt law is in effect, is TRUE, 
## then, can we PREDICT based on the measureable number of deaths on a given day, whether the 
## Law was in effect or not?  
    #  This is a classification problem

#  Y = law (response variable)
#  X = [DriversKilled, VanKilled]



# Logistic regression for classification of whether the Law is effect for any given date 
model <- glm (formula = law ~  DriversKilled + VanKilled, family = "binomial", data = df)
summary(model)

exp(model$coefficients)
# DriversKilled     VanKilled 
#     0.9490347     0.5935072 

boxplot(predictionsOfLawInEffect ~ df$law)  #Boxplot to find the optimal log-odds cutoff
predictionsOfLawInEffect <- predict(model, newdata = df)  # LOG ODDS of each row correspondign to when the law is in effect
predictionsOfLawInEffect_CATEGORICAL <- predictionsOfLawInEffect > -2

table(df$law, predictionsOfLawInEffect_CATEGORICAL)  # Confusion matrix


plot(density(exp(predictionsOfLawInEffect)))  # plot of the "ODDS" of each row being classified for the law being in effect

# ODDS = p / (1 - p); where ODDS = exp(predictionsOfLawInEffect)
# => (1 - p)*ODDS = p
# => p = ODDS/(1+ODDS)

# Probability of each row  corresponding to the law being in effect: 
plot(density(exp(predictionsOfLawInEffect)/(1+exp(predictionsOfLawInEffect))))  # plot of the "ODDS" of each row being classified for the law being in effect
summary(exp(predictionsOfLawInEffect)/(1+exp(predictionsOfLawInEffect)))
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000343 0.0030743 0.0338102 0.1197917 0.1595388 0.8213848 


# boxplot to find the optimal probability cutoff:
boxplot(exp(predictionsOfLawInEffect)/(1+exp(predictionsOfLawInEffect)) ~ df$law)


# Linear regression for REGRESSION of the number of people that died ON the X's 
# / regressors law-being-in-effect and total number of drivers  

# Y = number of people that died i.e.  DriversKilled variable
# X = [law, drivers, PetrolPrice]

model2 <- lm (DriversKilled ~ law  , data = df)
summary(model2)


## Export df to a file:
write.csv(df, row.names = F, file = "britishSeatBeltStudy.csv")
