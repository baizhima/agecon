# Statistical Software Application, SARD Fall 2013, RUC
# Final exam code, Shan Lu
# Chapter 3 
# Question 2 AER pp.100
rm(list=ls())
require("AER")
data("HousePrices")
summary(HousePrices)

# (a) Fit a multiple linear regression model to the logarithm of the price,
#     using all remaining variables as regressors. Experiment with models
#     containing lot size, number of bathrooms, number of bedrooms, and
#     stories in logarithms and in levels, respectively. Which model do you
#     prefer?

# regression 1 stories in logarithms (preferred)
hp_lm <- lm(log(price) ~ log(lotsize) + log(bedrooms) + log(bathrooms) + log(stories)+
            driveway+recreation+fullbase+gasheat+aircon+garage+prefer,data = HousePrices)
summary(hp_lm) # Adjusted R-squared:  0.6774 
# regression 2 stories in levels
hp_lm2 <- lm(log(price) ~ as.factor(lotsize) + as.factor(bedrooms) + as.factor(bathrooms)
             + as.factor(stories) + driveway+recreation+fullbase+gasheat+aircon+garage+prefer
             ,data = HousePrices)
summary(hp_lm2) # Adjusted R-squared:  0.687 
AIC(hp_lm,hp_lm2) # Akaike Info Criteria
BIC(hp_lm,hp_lm2) # Bayes Info Criteria

# (b) What is the expected price of a two-story house of 4,700 sq. ft. with
#     three bedrooms, two bathrooms, a driveway, no recreational room, a
#     full finished basement, without gas heating or air conditioning, and
#     two-car garage, that is not located in a preferred area? Report also a
#     prediction interval.
hp_predict <- predict(hp_lm, newdata = data.frame(lotsize = 4700, bedrooms = 3,
                                    bathrooms = 2, stories = 2, driveway = "yes",
                                    recreation = "no", fullbase = "yes",
                                    gasheat = "no", aircon = "no",
                                    garage = 2, prefer = "no"), 
                      interval = "predict")
# (c) In order to determine whether the logarithmic transformation of the
#     dependent variable is really appropriate, a Box-Cox transformation
#     might be helpful. Use the function boxcox() from the package MASS.
#     What do you conclude?
require(MASS)
boxcox(hp_lm) # 95% at lambda = 0.2 near 0, logarithmatic is a good transformation

# Problem 5 AER pp.101
rm(list=ls()) # clean workspace
data("PSID1982") # include data
# (a) Compute the J tests for M1 vs. M2 and M2 vs. M1, respectively.
m1 <- lm(log(wage) ~ education + experience + I(experience ^ 2) + weeks + married
         + gender + ethnicity + union, data = PSID1982)
m2 <- lm(log(wage) ~ education + experience + I(experience ^ 2) + weeks + occupation
         + south + smsa + industry, data = PSID1982)
# J test M1 vs. M2
jtest(m1,m2)

# (b) Both M1 and M2 can be artificially nested within a larger model. Use
#     an F test for M1 versus this augmented model. Repeat for M2 versus
#     the augmented model. What do you conclude?
aug_m <- lm(log(wage) ~ education + experience + I(experience ^ 2) + weeks + married
             + gender + ethnicity + union + occupation
             + south + smsa + industry, data = PSID1982)
anova(m1,aug_m)
anova(m1,aug_m)

# Problem 9 pp.101
rm(list=ls()) # clean workspace
data("USConsump1993") # include data
# (a) by OLS
consump_lm <- lm(expenditure ~ income, data = USConsump1993)


# (b) by IV. The only available instrument is investment, given by the
#     identity expenditure + investment = income.
consump_iv <- ivreg(expenditure ~ income | income - expenditure,data = USConsump1993)

# (c) Compare both sets of estimates using a Hausman test, thus replicating
#     Baltagi (2002, Section 11.7). What do you conclude?
install.packages(plm) # linear regression w/ panel data package
require(plm)
dfcomp <- data.frame(USConsump1993)

