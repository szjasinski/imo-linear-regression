library(dplyr)
library(tidyr)
library(stringr)
library(corrplot)


# OUTCOME VARIABLE
imo = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/project_data.csv")

summary(imo)
str(imo)


# CREATING NEW VARIABLES
imo$logPop2023 = log(imo$pop2023)
imo$logDensity = log(imo$density)
imo$logArea = log(imo$area)
imo$logLandAreaKm = log(imo$landAreaKm)



# HANDLING MISSING VALUES -- 
# --- FINDING MISSING VALUES IN THE WEB?????
# --- REPLACING WITH MEDIAN/MEAN OR PREDICTING VALUES WITH REGRESSION
# --- DELETING ROWS WITH NA

sum(is.na(imo$happiness2021))
sum(is.na(imo$total_score))

imo$weeklyHours

imo$schoolAge

imo$total_score = as.numeric(as.character(imo$total_score)) 
imo$rain = as.numeric(as.character(imo$rain)) 
imo$labor = as.numeric(as.character(imo$labor)) 
imo$temp = as.numeric(as.character(imo$temp)) 
imo$area = as.numeric(imo$area)
imo$weeklyHours = as.numeric(imo$weeklyHours)
imo$pop2023 = as.numeric(imo$pop2023)
imo$googleall = as.numeric(imo$googleall)
imo$google5y = as.numeric(imo$google5y)
imo$google1y = as.numeric(imo$google1y)
imo$schoolAge = as.numeric(imo$schoolAge)

# REPLACING OR DROPPING NAs
imo_median = imo %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))
imo_mean = imo %>% mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm=TRUE))))
imo_drop = na.omit(imo)


# CREATING LIST WITH IMO SETS WITH NA DIFFERENTLY HANDLED
imo_na_replaced_sets = list(imo, imo_median, imo_mean, imo_drop)

# single brackets [.] extracts list
# double brackets [[.]] extracts element of the list
head(imo_na_replaced_sets[[1]])

head(imo)
str(imo)
# DELETING COUNTRY COLUMN TO CALCULATE CORRELATION MATRIX
predictors = subset(imo, select = -c(total_score, country))
predictors_total_score = subset(imo, select = -c(country, pop2023, area, density, landAreaKm))
summary(predictors)
round(cor(predictors, method=c("pearson")),2)

# VISUALIZATION OF CORRELATION MATRIX
M = cor(predictors)
M
corrplot(M, method = 'number', type = 'lower', order = 'FPC') # colorful number
range(predictors)
cor(predictors_total_score)
predictors
corrplot(cor(predictors_total_score), method = 'number', type = 'lower', order = 'FPC') # colorful number

# PLOTS
plot(happy$happiness2021)
plot(happy$pop2023)
plot(density(happy$pop2023, na.rm=TRUE))

# CORRELATIONS
# use = "complete.obs" works with NA in data
cor(imo$logPop2023, imo$total_score, use = "complete.obs")
cor(imo$happiness2021, imo$total_score, use = "complete.obs")
cor(imo$rain, imo$total_score, use = "complete.obs")
cor(imo$labor, imo$total_score, use = "complete.obs")
cor(imo$temp, imo$total_score, use = "complete.obs")
cor(imo$logDensity, imo$total_score, use = "complete.obs")
cor(imo$logArea, imo$total_score, use = "complete.obs")
cor(imo$logLandAreaKm, imo$total_score, use = "complete.obs")
cor(imo$weeklyHours, imo$total_score, use = "complete.obs")
cor(imo$spending, imo$total_score, use = "complete.obs")
cor(imo$iq, imo$total_score, use = "complete.obs")
cor(imo$googleall, imo$total_score, use = "complete.obs")
cor(imo$google5y, imo$total_score, use = "complete.obs")
cor(imo$google1y, imo$total_score, use = "complete.obs")

cor.test(imo$happiness2021, imo$total_score)
cor.test(imo$logPop2023, imo$total_score)

# ----------------------------
#### CHECKING MODEL ASSUMPTIONS ####

# 1. LINEARITY (BETWEEN EACH PREDICTOR AND OUTCOME)
# plots, if not transform by log or exp etc, x^n or drop variable
# 2. NO MULTICOLLINEARITY (BETWEEN PREDICTORS)
# VIF value < 5 or cor matrix, if not drop variable or change model to ridge, lasso etc
# 3. INDEPENDENCE OF OBSERVATIONS
# Durbin-Watson test of residuals
# 4. HOMOSCEDASTICITY (CONSTANT VARIANCE OF RESIDUALS)
# plot of residuals vs predicted value
# 5. NORMALITY OF RESIDUALS
# Q-Q plot, tests like: Shapiro-Wilk, Kolmogorov-Smirnov, Jarque-Barre, D'Agostino-Pearson


# ----------------------------
#### SIMPLE LINEAR MODELS ####

# SCHOOL AGE
lmod = lm(total_score ~ schoolAge, imo)
summary(lmod)
plot(total_score ~ schoolAge, imo)
abline(lmod)

# GOV EDU SPENDING
lmod = lm(total_score ~ GovEduSpendingMill, imo)
summary(lmod)
plot(total_score ~ GovEduSpendingMill, imo)
abline(lmod)

# LOG(GOV EDU SPENDING)
lmod = lm(total_score ~ log(GovEduSpendingMill), imo)
summary(lmod)
plot(total_score ~ log(GovEduSpendingMill), imo)
abline(lmod)

# TEACHERS SALARY
lmod = lm(total_score ~ teacherSalary, imo)
summary(lmod)
plot(total_score ~ teacherSalary, imo)
abline(lmod)

# TEACHERS COMPENSATION PERCCENTAGE
lmod(total_score ~ teachersCompPerc, imo)
summary(lmod)
plot(total_score ~ teachersCompPerc, imo)
abline(lmod)


# HAPPINESS
lmod = lm(total_score ~ happiness2021, data = imo)
lmod
summary(lmod)
plot(total_score ~ happiness2021, data = imo)
abline(lmod)
text(x = 5, y = 240, paste0("total_score = ", round(coefs[1], 2),  "+", round(coefs[2], 2), "*happiness2021"))
# na.omit(data)

# LOG POPULATION
lmod = lm(total_score ~ logPop2023, imo)
plot(total_score ~ logPop2023, imo)
abline(lmod)
summary(lmod)
coef(lmod)

# POP
lmod = lm(total_score ~ I(exp(logPop2023)), imo)
plot(total_score ~ I(exp(logPop2023)), imo)
abline(lmod)
summary(lmod)

# LOG DENSITY
lmod = lm(total_score ~ logDensity, imo)
plot(total_score ~ logDensity, imo)
abline(lmod)
summary(lmod)
cor.test(imo$total_score, imo$logDensity)

# LOG AREA
lmod = lm(total_score ~ logArea, imo)
plot(total_score ~ logArea, imo)
abline(lmod)
summary(lmod)

# LOG LAND AREA KM
lmod = lm(total_score ~ logLandAreaKm, imo)
plot(total_score ~ logLandAreaKm, imo)
abline(lmod)
summary(lmod)

# RAIN
lmod = lm(total_score ~ rain, imo)
plot(total_score ~ rain, imo)
abline(lmod)
summary(lmod)

# LABOR
lmod = lm(total_score ~ labor, imo)
plot(total_score ~ labor, imo)
abline(lmod)
summary(lmod)

# TEMP
lmod = lm(total_score ~ temp, imo)
plot(total_score ~ temp, imo)
abline(lmod)
summary(lmod)

# IQ
lmod = lm(total_score ~ iq, imo)
summary(lmod)
plot(total_score ~ iq, imo)
abline(lmod)

# MILITARY 
lmod = lm(total_score ~ log(spending), imo)
summary(lmod)
plot(total_score ~ log(spending), imo)
abline(lmod)

# GOOGLEALL
lmod = lm(total_score ~ log(googleall), imo)
summary(lmod)
plot(total_score ~ log(googleall), imo)
abline(lmod)
plot(lmod)

# GOOGLE5Y
lmod = lm(total_score ~ log(google5y), imo)
summary(lmod)
plot(total_score ~ log(google5y), imo)
abline(lmod)
plot(lmod)

# GOOGLE1Y
lmod = lm(total_score ~ log(google1y), imo)
summary(lmod)
plot(total_score ~ log(google1y), imo)
abline(lmod)
plot(lmod)

# GOOGLE ULTRA FEATURE
lmod = lm(total_score ~ I(log(google5y+googleall+google1y)), imo)
summary(lmod)
plot(total_score ~ I(log(google5y+googleall+google1y)), imo)
abline(lmod)
plot(lmod)


# -----------------------------
#### MULTIPLE LINEAR MODELS ####

# MODEL 1 ###
head(imo)
mlmod = lm(total_score ~  weeklyHours + spending + iq + growthRate + rain + temp + logLandAreaKm + logDensity + logArea + logPop2023 + happiness2021, imo)
summary(mlmod)
summary(mlmod)$r.squared
summary(mlmod)$adj.r.squared
plot(mlmod)

# MODEL 2 ###
mlmod = lm(total_score ~  iq + temp + logPop2023, imo)
summary(mlmod)
summary(mlmod)$r.squared
summary(mlmod)$adj.r.squared

# MODEL 3 ### THE BEST ONE
mlmod = lm(total_score ~ iq + rain + logPop2023, imo)
summary(mlmod)
summary(mlmod)$r.squared
summary(mlmod)$adj.r.squared
plot(mlmod)

# MODEL 4 ### 
mlmod = lm(total_score ~ iq + rain + log(GovEduSpendingMill), imo)
summary(mlmod)
summary(mlmod)$r.squared
summary(mlmod)$adj.r.squared
plot(mlmod)

# MODEL 5 ### 
mlmod = lm(total_score ~ schoolAge + iq + rain + logPop2023, imo)
summary(mlmod)
summary(mlmod)$r.squared
summary(mlmod)$adj.r.squared
plot(mlmod)

# MODEL 6 ### 
mlmod = lm(total_score ~  iq + rain + logPop2023 + log(spending), imo)
summary(mlmod)
summary(mlmod)$r.squared
summary(mlmod)$adj.r.squared
plot(mlmod)


# MODEL XD ### DELETED BELARUS ---- TOO LOW MILITARY SPENDING
sort(log(imo$spending))
xdd = imo[c("country", "total_score", "spending")]
require(dplyr)
xdd = filter(xdd, country != "Belarus")

lmod = lm(total_score ~ log(spending), xdd)
summary(lmod)
plot(xdd$total_score ~ log(xdd$spending))

imo$spending [imo$country == "Belarus"] = NA
plot(log(imo$spending))

mlmod = lm(total_score ~  iq + rain + logPop2023 + log(spending), imo)
summary(mlmod)
summary(mlmod)$r.squared
summary(mlmod)$adj.r.squared




plot(imo$total_score ~ log(imo$spending))
abline(lmod)




cor(xdd$total_score, xdd$spending, use = "complete.obs")


cor(imo$iq, imo$rain, use = "complete.obs")
cor(imo$iq, imo$logPop2023, use = "complete.obs")
cor(imo$rain, imo$logPop2023, use = "complete.obs")

# -----------------------------
# TRAINING MANY MODELS AT ONCE

# run n regressions
n = length(imo_na_replaced_sets)
my_lms = lapply(1:n, function(x) lm(total_score ~ iq + rain + logPop2023, imo_na_replaced_sets[[x]]))
sapply(my_lms, coef)
summaries = lapply(my_lms, summary)
summaries
lapply(summaries, function(x) x$coefficients[, c(1,4)]) # coefficents with p values:
sapply(summaries, function(x) c(r_sq = x$r.squared, 
                                adj_r_sq = x$adj.r.squared)) # r-squared values
# dictionary would be better than list to store datasets
print("1:imo,  2:imo_median,  3:imo_mean,  4:imo_drop")




# pv of model??
# statistical significance of variables pv < 0.05???
# too many variables -- overfitting???   eg 2 dots, 1 line??
# R^2 = 1 - RSS/TSS
# Residual Sum of Squares = sum((y-y_predicted)^2)
# Total Sum of Squares = sum((y-mean(y))^2)

# adjR^2 = R^n adjusted for number of predictors



