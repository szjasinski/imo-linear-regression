library(corrplot)
library(nortest)
library(lmtest)
library(faraway)
library(AICcmodavg)
library(MASS)


##### ----------------------------
##### 2 - CHARAKTERYSTYKI DANYCH #####
##### ----------------------------

# dane do projektu
imo = read.csv("data.csv")

# wstępna analiza danych
dim(imo)
head(imo)
str(imo)
summary(imo)


# wykresy zmiennych
plot(imo$total_score)
plot(imo$google_trends)
plot(imo$school_age)
plot(imo$hours_worked)
plot(imo$iq)
plot(imo$rain)
plot(imo$fertility)
plot(imo$gdp)
plot(imo$population)
plot(imo$age_dependency)
plot(imo$agriculture)


##### ----------------------------
##### 3 - BUDOWA MODELU #####
##### ----------------------------


model = lm(total_score ~  google_trends + hours_worked + school_age + 
             agriculture + iq + rain + fertility + gdp + population + 
             age_dependency, imo)


##### ----------------------------
##### 4 - WERYFIKACJA ZAŁOŻEŃ MODELU #####
##### ----------------------------


# 4.1 LINIOWA STRUKTURA (BETWEEN EACH PREDICTOR AND OUTCOME)
# plots, if not transform by log or exp etc, x^n or drop variable

# residuals vs fitted i krzywa dopasowania??
plot(model,1)

#  rainbow test
raintest(model)

# reset test
resettest(model)

# wykres reszt - brak wzorca
plot(model$residuals)


# 4.2 BRAK WSPÓŁLINIOWOŚCI

# macierz korelacji
M = cor(imo,use = "complete.obs")
corrplot(M) # by default, method = 'circle'

# podejrzane korelacje
cor(imo$fertility, imo$iq,use = "complete.obs")
cor(imo$agriculture, imo$iq,use = "complete.obs")
cor(imo$hours_worked, imo$iq,use = "complete.obs")
cor(imo$hours_worked, imo$fertility,use = "complete.obs")
cor(imo$population, imo$gdp,use = "complete.obs")

# vif
vif(model)   


# 4.3 NORMAlNOŚĆ RESIDUÓW

# rozklad residuuow tez
summary(model) 


# qqplot
qqnorm(model$residuals)
qqline(model$residuals)
plot(model,2)

# histogram
hist(model$residuals,breaks=15)

# gestosc
plot(density(model$residuals))

# test Anderson-Darling
ad.test(model$residuals)

# test Shapiro-Wilk
shapiro.test(model$residuals)

# test Kolmogorov-Smirnov
lillie.test(model$residuals)



# 4.4 STAŁA WARIANCJA RESIDUÓW 

# wykres residuów
plot(model$residuals)

# test Breusch-Pagan
bptest(model)


# 4.5 AUTOKORELACJA RESIDUÓW

# test Durbin-Watson
dwtest(model)

# test Box-Pierce
Box.test(model$residuals,type='Box-Pierce')

# test Box-Ljung
Box.test(model$residuals,type='Ljung-Box')

# autokorelacja pierwszgo rzędu
n = length(model$residuals)
plot(model$residuals[2:n] ~ model$residuals[1:(n-1)]) 



##### ----------------------------
##### 5 - INTERPRETACJA MODELU #####
##### ----------------------------

summary(model)
plot(model)



##### ----------------------------
##### 6 - OBSERWACJE ODSTAJĄCE #####
##### ----------------------------


# obserwacje z gwiazdka
influence.measures(model)



# obliczmy dzwignie dla kazdej obserwacji w modelu i sortujemy
sort(hatvalues(model))

# wykres dziegni dla kazdej obserwacji
plot(hatvalues(model), type = 'h')

cooks.distance(model)

sort(cooks.distance(model))


##### ----------------------------
##### 7 - DRUGI MODEL #####
##### ----------------------------


# usuwamy Stany Zjednoczone
imo2 = imo[-100,]

# usuwamy po kolei najbardziej nieistotną cechę z każdego kolejnego modelu, aż do uzyskania samych istotnych

modelA = lm(total_score ~  google_trends + hours_worked + school_age + 
              agriculture + iq + rain + fertility + gdp + population + 
              age_dependency, imo2)

# bez hours_worked
modelB = lm(total_score ~  google_trends + school_age + 
              agriculture + iq + rain + fertility + gdp + population + 
              age_dependency, imo2)

# bez agriculture
modelC = lm(total_score ~  google_trends + school_age + iq + rain + fertility + gdp + population + 
              age_dependency, imo2)

# bez population
modelD = lm(total_score ~  google_trends + school_age + iq + rain + fertility + gdp + 
              age_dependency, imo2)

# bez fertility
modelE = lm(total_score ~  google_trends + school_age + iq + rain + gdp + 
              age_dependency, imo2)

# bez age_dependency
modelF = lm(total_score ~  google_trends + school_age + iq + rain + gdp, imo2)

# bez rain
modelG = lm(total_score ~  google_trends + school_age + iq + gdp, imo2)




# definiujemy liste modeli
modele <- list(modelA, modelB, modelC, modelD, modelE, modelF, modelG)

# definiujemy nazwy
mod.nazwy <- c('A', 'B', 'C', 'D', 'E', 'F', 'G')

# obliczamy AIC dla kazdego modelu
aictab(cand.set = modele, modnames = mod.nazwy)


modelF = lm(total_score ~  google_trends + school_age + iq + rain + gdp, imo2)
summary(modelF)
plot(modelF)

# Chiny - obserwacja odstająca

# bez Chin
imo3 = imo2[-18,]
modelF1 = lm(total_score ~  google_trends + school_age + iq + rain + gdp, imo3)
summary(modelF1)
plot(modelF1)



##### MODEL 2
model = lm(total_score ~  google_trends + school_age + iq + rain + gdp, imo3)
summary(model)
plot(model)
##### WERYFIKACJA ZAŁOŻEŃ MODELU 2 #####



# 4.1 LINIOWA STRUKTURA (BETWEEN EACH PREDICTOR AND OUTCOME)
# plots, if not transform by log or exp etc, x^n or drop variable

# residuals vs fitted i krzywa dopasowania??
plot(model,1)

#  rainbow test
raintest(model)

# reset test
resettest(model)

# wykres reszt - brak wzorca
plot(model$residuals)


# 4.2 BRAK WSPÓŁLINIOWOŚCI

# macierz korelacji
imo3 = imo3[,-c(4,7,9,10,11)]
M = cor(imo3,use = "complete.obs")
M
corrplot(M) # by default, method = 'circle'


# vif
vif(model)   


# 4.3 NORMAlNOŚĆ RESIDUÓW

# rozklad residuuow tez
summary(model) 


# qqplot
qqnorm(model$residuals)
qqline(model$residuals)
plot(model,2)

# histogram
hist(model$residuals,breaks=15)

# gestosc
plot(density(model$residuals))

# test Anderson-Darling
ad.test(model$residuals)

# test Shapiro-Wilk
shapiro.test(model$residuals)

# test Kolmogorov-Smirnov
lillie.test(model$residuals)



# 4.4 STAŁA WARIANCJA RESIDUÓW 

# wykres residuów
plot(model$residuals)

# test Breusch-Pagan
bptest(model)

# BOX COX
y = imo3$total_score
x1 = imo3$google_trends
x2 = imo3$school_age
x3 = imo3$iq
x4 = imo3$rain
x5 = imo3$gdp
bc <- boxcox(y ~ x1 + x2 + x3 + x4 + x5)
(lambda <- bc$x[which.max(bc$y)])
[1] 1.030303

# nowy model po transformacji Boxa Coxa
# model <- lm(((y^lambda-1)/lambda) ~ x)

# 4.5 AUTOKORELACJA RESIDUÓW

# test Durbin-Watson
dwtest(model)

# test Box-Pierce
Box.test(model$residuals,type='Box-Pierce')

# test Box-Ljung
Box.test(model$residuals,type='Ljung-Box')

# autokorelacja pierwszgo rzędu
n = length(model$residuals)
plot(model$residuals[2:n] ~ model$residuals[1:(n-1)]) 






summary(model2)
plot(model2)

