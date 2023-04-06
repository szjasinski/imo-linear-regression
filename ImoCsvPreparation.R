library(stringr)


# OUTCOME VARIABLE
imo_score = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/imo_data.csv", sep=";")

# PREDICTING VARIABLES
happy = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/happiness_data.csv") # many predictors here
rain = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/annual_precipation_2017_mm.csv", sep=";")
labor = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/avg_annual_labor_hours_2017.csv", sep=";")
temp = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/avg_annual_temp_61-90_celc.csv", sep=";")
worked = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/weekly hours worked.csv")
military = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/military spending in 2023.csv")
iq = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/iq population data 2023.csv")
googleall = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/google trends mathematics all time.csv", header = FALSE)
google5y = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/google trends mathmatics 5 years.csv", header = FALSE)
google1y = read.csv("/Users/szymon/Desktop/ekonometria/ekon projekt/datasets/google trends mathematics past year.csv", header = FALSE)


head(imo_score)
head(happy)
head(rain)
head(labor)
head(temp)
head(worked)
head(military)
head(iq)
head(googleall)
head(google5y)
head(google1y)


# ------------------------
#### DATA PREPARATION ####

# REMOVING FIRST ROWS WITHOUT DATA
google1y = tail(google1y, -2)
google5y = tail(google5y, -2)
googleall = tail(googleall, -2)

# KEEPING RELEVANT COLUMNS ONLY
imo_score = imo_score[c("country", "total.points")]
happy = happy[c("country", "happiness2021", "growthRate", "pop2023", "area", "landAreaKm", "density")]
rain = rain[c("X.1", "X.2")]
labor = labor[c("X.1", "X.2")]
temp = temp[c("X.1", "X.2")]
worked = worked[c("country", "weeklyHours")]
military = military[c("country", "spending")]
iq = iq[c("country", "iq")]

# RENAMING COLUMNS
names(rain)[1] = "country"
names(labor)[1] = "country"
names(temp)[1] = "country"
names(googleall)[1] = "country"
names(google5y)[1] = "country"
names(google1y)[1] = "country"
names(imo_score)[2] = "total_score"
names(rain)[2] = "rain"
names(labor)[2] = "labor"
names(temp)[2] = "temp"
names(googleall)[2] = "googleall"
names(google5y)[2] = "google5y"
names(google1y)[2] = "google1y"


# REMOVING LEADING WHITESPACES
rain[,c(1)]=trimws(str_trim(rain[,c(1)]))
labor[,c(1)]=trimws(str_trim(labor[,c(1)]))
temp[,c(1)]=trimws(str_trim(temp[,c(1)]))

str(rain)

# STANDARDIZING COUNTRY NAMES 
imo_score$country [imo_score$country == "Islamic Republic of Iran"] = "Iran"
imo_score$country [imo_score$country == "People's Republic of China"] = "China"
imo_score$country [imo_score$country == "United States of America"] = "United States"
imo_score$country [imo_score$country == "Republic of Korea"] = "South Korea"
imo_score$country [imo_score$country == "Republic of Moldova"] = "Moldova"
rain$country [rain$country == "Palestine[b]"] = "Palestine"
googleall$country [googleall$country == "Bosnia & Herzegovina"] = "Bosnia and Herzegovina"
googleall$country [googleall$country == "Czechia"] = "Czech Republic"
googleall$country [googleall$country == "Türkiye"] = "Turkey"
googleall$country [googleall$country == "Macao"] = "Macau"
google5y$country [google5y$country == "Bosnia & Herzegovina"] = "Bosnia and Herzegovina"
google5y$country [google5y$country == "Czechia"] = "Czech Republic"
google5y$country [google5y$country == "Türkiye"] = "Turkey"
google5y$country [google5y$country == "Macao"] = "Macau"
google1y$country [google1y$country == "Bosnia & Herzegovina"] = "Bosnia and Herzegovina"
google1y$country [google1y$country == "Czechia"] = "Czech Republic"
google1y$country [google1y$country == "Türkiye"] = "Turkey"
google1y$country [google1y$country == "Macao"] = "Macau"




# MERGING DATAFRAMES INTO ONE
imo_1 = merge(x = imo_score, y = rain, by = "country", all.x = TRUE, all.y = FALSE)
imo_2 = merge(x = imo_1, y = labor, by = "country", all.x = TRUE, all.y = FALSE)
imo_3 = merge(x = imo_2, y = temp, by = "country", all.x = TRUE, all.y = FALSE)
imo_4 = merge(x = imo_3, y = worked, by = "country", all.x = TRUE, all.y = FALSE)
imo_5 = merge(x = imo_4, y = military, by = "country", all.x = TRUE, all.y = FALSE)
imo_6 = merge(x = imo_5, y = iq, by = "country", all.x = TRUE, all.y = FALSE)
imo_7 = merge(x = imo_6, y = googleall, by = "country", all.x = TRUE, all.y = FALSE)
imo_8 = merge(x = imo_7, y = google5y, by = "country", all.x = TRUE, all.y = FALSE)
imo_9 = merge(x = imo_8, y = google1y, by = "country", all.x = TRUE, all.y = FALSE)
imo = merge(x = imo_9, y = happy, by = "country", all.x = TRUE, all.y = FALSE)

imo_7[!complete.cases(imo_7), ]


# FIXING COLUMN FORMATTING SO ITS TYPE CAN BE CHANGED
imo$temp = gsub("−", "-", imo$temp)
imo$rain = gsub(",", "", imo$rain)
imo$labor = gsub(",", "", imo$labor)

# CHANGING COLUMN TYPE
imo$total_score = as.numeric(as.character(imo$total_score)) 
imo$rain = as.numeric(as.character(imo$rain)) 
imo$labor = as.numeric(as.character(imo$labor)) 
imo$temp = as.numeric(as.character(imo$temp)) 
imo$area = as.numeric(imo$area)
imo$weeklyHours = as.numeric(imo$weeklyHours)
imo$googleall = as.numeric(imo$googleall)
imo$google5y = as.numeric(imo$google5y)
imo$google1y = as.numeric(imo$google1y)

str(imo)

# EXPORTING CSV
write.csv(imo, "/Users/szymon/Desktop/ekonometria/ekon projekt/project_data.csv", row.names=FALSE)

summary(imo)
imo$google5y - imo$google1y
