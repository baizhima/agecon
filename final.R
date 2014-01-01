# Statistical Software Application, SARD Fall 2013, RUC
# Final exam code, Shan Lu

################################################################
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
data("USConsump1993") # include data, mts format
# (a) by OLS
consump_lm <- lm(expenditure ~ income, data = USConsump1993)


# (b) by IV. The only available instrument is investment, given by the
#     identity expenditure + investment = income.
dfUSConsump <- data.frame(USConsump1993)
dfUSConsump$investment <- dfUSConsump$income - dfUSConsump$expenditure
consump_iv2 <- ivreg(expenditure ~ income | investment, data = dfUSConsump)

# (c) Compare both sets of estimates using a Hausman test, thus replicating
#     Baltagi (2002, Section 11.7). What do you conclude?
summary(consump_lm)
summary(consump_iv2)

#####################################################
# Chapter 4
# Question 3, pp.128 
# Explore further growth regressions for the OECDGrowth data using the augmented
# and extended Solow models of Nonneman and Vanhoudt (1996),
# which consider the additional regressors log(school) (human capital)
# and log(randd) (technological know-how), respectively. First, replicate
# the OLS results from Nonneman and Vanhoudt (1996, Table IV), and
# subsequently compare them with the resistant LTS results by adopting
# the strategy of Zaman et al. (2001).

rm(list=ls()) # clean workspace
data("OECDGrowth")

# source: ?OECDGrowth OECD Macroeconomic Data Documentation
## augmented and extended Solow growth model
## Nonneman and Vanhoudt (1996), Table IV
aso_ols <- lm(log(gdp85/gdp60) ~ log(gdp60) + log(invest) +
                log(school) + log(popgrowth+.05), data = OECDGrowth)
eso_ols <- lm(log(gdp85/gdp60) ~ log(gdp60) + log(invest) +
                log(school) + log(randd) + log(popgrowth+.05), data = OECDGrowth)
## determine unusual observations using LTS
library("MASS")
so_lts <- lqs(log(gdp85/gdp60) ~ log(gdp60) +  log(invest) + log(popgrowth+.05),
              data = OECDGrowth, psamp = 13, nsamp = "exact")
## large residuals
nok1 <- abs(residuals(so_lts))/so_lts$scale[2] > 2.5
residuals(so_lts)[nok1]/so_lts$scale[2]
## high leverage
so_ols <- lm(log(gdp85/gdp60) ~ log(gdp60) + log(invest) + log(popgrowth+.05),
             data = OECDGrowth) ## Zaman, Rousseeuw and Orhan (2001), Table 2
X <- model.matrix(so_ols)[,-1]
cv <- cov.rob(X, nsamp = "exact")
mh <- sqrt(mahalanobis(X, cv$center, cv$cov))
nok2 <- mh > 2.5
mh[nok2]

## bad leverage
nok <- which(nok1 & nok2)
nok

## robust results without bad leverage points
so_rob <- update(so_ols, subset = -nok)
summary(so_rob)
# This is similar to Zaman, Rousseeuw and Orhan (2001), Table 2
# but uses exact computations (and not sub-optimal results
# for the robust functions lqs and cov.rob) 

##############################################################
# Chapter 5
# Question 2, pp.158 Probit model
rm(list=ls())

data("PSID1976")

## Example 21.4, p. 681, and Tab. 21.3, p. 682
# (a) Fit a probit model for labor force participation using the regressors
#     age, age squared, family income, education, and a factor indicating
#     the presence of children. (The factor needs to be constructed from the available information.)
PSID1976$kids <- with(PSID1976, factor((youngkids + oldkids) > 0,
                                       levels = c(FALSE, TRUE), labels = c("no", "yes")))
gr_probit1 <- glm(participation ~ age + I(age^2) + I(fincome/10000) + education + kids,
                  data = PSID1976, family = binomial(link = "probit") )  
# (b) Reestimate the model assuming that different equations apply to
#     women with and without children.
gr_probit2 <- glm(participation ~ age + I(age^2) + I(fincome/10000) + education,
                  data = PSID1976, family = binomial(link = "probit"))
gr_probit3 <- glm(participation ~ kids/(age + I(age^2) + I(fincome/10000) + education),
                  data = PSID1976, family = binomial(link = "probit"))
# (c) Perform a likelihood ratio test to check whether the more general
#     model is really needed.
lrtest(gr_probit1)

# 5.Using the PSID1976 data, run a tobit regression of hours worked on nonwife
#   income (to be constructed from the available information), age, experience,
#   experience squared, education, and the numbers of younger and
#   older children.


mc_tobit <- tobit(hours ~ nwincome + education + experience + I(experience^2) + age
                  +youngkids + oldkids, data = PSID1976)
coeftest(mc_tobit)