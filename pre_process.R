library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

# reading files and adding 'year' column
df_2019 <- read.csv(file = './index2019_data.csv', stringsAsFactors = FALSE)
df_2019$Year = 2019
df_2018 <- read.csv('index2018_data.csv', stringsAsFactors = FALSE)
df_2018$Year = 2018
df_2017 <- read.csv('index2017_data.csv', stringsAsFactors = FALSE)
df_2017$Year = 2017
df_2016 <- read.csv('index2016_data.csv', stringsAsFactors = FALSE)
df_2016$Year = 2016
df_2015 <- read.csv('index2015_data.csv', stringsAsFactors = FALSE)
df_2015$Year = 2015
df_2014 <- read.csv('index2014_data.csv', stringsAsFactors = FALSE)
df_2014$Year = 2014

# Changing to common column 'X.Score' in all
colnames(df_2019)[colnames(df_2019) == 'X2019.Score'] <- 'X.Score'
colnames(df_2018)[colnames(df_2018) == 'X2018.Score'] <- 'X.Score'
colnames(df_2017)[colnames(df_2017) == 'X2017.Score'] <- 'X.Score'
colnames(df_2016)[colnames(df_2016) == 'X2016.Score'] <- 'X.Score'
colnames(df_2015)[colnames(df_2015) == 'X2015.Score'] <- 'X.Score'
colnames(df_2014)[colnames(df_2014) == 'X2014.Score'] <- 'X.Score'

# removing 'Country' as that is same as 'Country.Name'
df_2019$Country <- NULL

# adding columns which were missing in 2014, 2015 and 2016 with values NA
df_2016[c("Judical.Effectiveness", "Government.Integrity", "Tax.Burden", "Fiscal.Health")] <- NA
df_2015[c("Judical.Effectiveness", "Government.Integrity", "Tax.Burden", "Fiscal.Health")] <- NA
df_2014[c("Judical.Effectiveness", "Government.Integrity", "Tax.Burden", "Fiscal.Health")] <- NA

# keeping same columns as in 2019 in other years
df_2018 <- df_2018[, colnames(df_2019)]
df_2017 <- df_2017[, colnames(df_2019)]
df_2016 <- df_2016[, colnames(df_2019)]
df_2015 <- df_2015[, colnames(df_2019)]
df_2014 <- df_2014[, colnames(df_2019)]

# merging in a common dataframe
df <- rbind(df_2019, df_2018, df_2017, df_2016, df_2015, df_2014)

# removing rows with no CountryID (blank rows)
df <- df[!is.na(df$CountryID), ]


# Names of some countries have changed over the period, or are written in different 
# ways in the data. Finding such countries through CountryID which is unique:
ID_con <- df %>% group_by(., CountryID, Country.Name) %>% summarise(., n = n())
nam_chg <- ID_con[ID_con$n != 6, ]
nam_chg


# updating names of counntries using the names in latest year:
new_nam_30 <- df[(df$CountryID == 30) & (df$Year == 2019),'Country.Name']
df[(df$CountryID == 30),'Country.Name'] = new_nam_30

new_nam_71 <- df[(df$CountryID == 71) & (df$Year == 2019),'Country.Name']
df[(df$CountryID == 71),'Country.Name'] = new_nam_71

new_nam_91 <- df[(df$CountryID == 91) & (df$Year == 2019),'Country.Name']
df[(df$CountryID == 91),'Country.Name'] = new_nam_91

new_nam_138 <- df[(df$CountryID == 138) & (df$Year == 2019),'Country.Name']
df[(df$CountryID == 138),'Country.Name'] = new_nam_138

new_nam_139 <- df[(df$CountryID == 139) & (df$Year == 2019),'Country.Name']
df[(df$CountryID == 139),'Country.Name'] = new_nam_139

new_nam_148 <- df[(df$CountryID == 148) & (df$Year == 2019),'Country.Name']
df[(df$CountryID == 148),'Country.Name'] = new_nam_148

new_nam_156 <- df[(df$CountryID == 156) & (df$Year == 2019),'Country.Name']
df[(df$CountryID == 156),'Country.Name'] = new_nam_156


# removing a few columns
df$Tariff.Rate.... <- NULL
df$Income.Tax.Rate.... <- NULL
df$Corporate.Tax.Rate.... <- NULL
df$Tax.Burden...of.GDP <- NULL
df$Gov.t.Expenditure...of.GDP <- NULL
df$FDI.Inflow..Millions. <- NULL
df$X5.Year.GDP.Growth.Rate.... <- NULL

df[df == 'n/a',] <- NA
df[df == 'N/A',] <- NA

# converting data-types of different variables
df$Year<- as.factor(df$Year)

df$World.Rank <- as.integer(df$World.Rank)
df$Region.Rank <- as.integer(df$Region.Rank)

df$X.Score <- parse_number(df$X.Score)

df$Property.Rights <- parse_number(df$Property.Rights)
df$Judical.Effectiveness <- parse_number(df$Judical.Effectiveness)
df$Government.Integrity <- parse_number(df$Government.Integrity)

df$Tax.Burden <- parse_number(df$Tax.Burden)
df$Gov.t.Spending <- parse_number(df$Gov.t.Spending)
df$Fiscal.Health <- parse_number(df$Fiscal.Health)

df$Business.Freedom <- parse_number(df$Business.Freedom)
df$Labor.Freedom <- parse_number(df$Labor.Freedom)
df$Monetary.Freedom <- parse_number(df$Monetary.Freedom)

df$Trade.Freedom <- parse_number(df$Trade.Freedom)
df$Investment.Freedom <- parse_number(df$Investment.Freedom)
df$Financial.Freedom <- parse_number(df$Financial.Freedom)

df$Population..Millions. <- parse_number(df$Population..Millions.)
df$GDP..Billions..PPP. <- parse_number(df$GDP..Billions..PPP.)
df$GDP.Growth.Rate.... <- parse_number(df$GDP.Growth.Rate....)
df$X5.Year.GDP.Growth.Rate.... <- parse_number(df$X5.Year.GDP.Growth.Rate....)
df$GDP.per.Capita..PPP. <- parse_number(df$GDP.per.Capita..PPP.)
df$Unemployment.... <- parse_number(df$Unemployment....)
df$Inflation.... <- parse_number(df$Inflation....)
df$Public.Debt....of.GDP. <- parse_number(df$Public.Debt....of.GDP.)

colnames(df)[colnames(df) == 'Population..Millions.'] <- 'Population.Millions'
colnames(df)[colnames(df) == 'GDP..Billions..PPP.'] <- 'GDP.PPP.Billions'
colnames(df)[colnames(df) == 'GDP.Growth.Rate....'] <- 'GDP.Growth.Rate'
colnames(df)[colnames(df) == 'X5.Year.GDP.Growth.Rate....'] <- 'Five.Year.GDP.Growth.Rate'
colnames(df)[colnames(df) == 'GDP.per.Capita..PPP.'] <- 'GDP.per.Capita.PPP'
colnames(df)[colnames(df) == 'Unemployment....'] <- 'Unemployment.Rate'
colnames(df)[colnames(df) == 'Inflation....'] <- 'Inflation'
colnames(df)[colnames(df) == 'Public.Debt....of.GDP.'] <- 'Public.Debt.Perc.of.GDP'
colnames(df)[colnames(df) == 'Gov.t.Spending'] <- 'Govt.Spending'


# adding new columns representing four categories
df$rule_law <- rowMeans(df[,c('Property.Rights', 'Judical.Effectiveness', 'Government.Integrity')])
df$govt_size <- rowMeans(df[, c('Tax.Burden', 'Gov.t.Spending', 'Fiscal.Health')])
df$reg_eff <- rowMeans(df[, c('Business.Freedom', 'Labor.Freedom', 'Monetary.Freedom')])
df$mkt_open <- rowMeans(df[, c('Trade.Freedom', 'Investment.Freedom', 'Financial.Freedom')])   

#str(df)
colnames(df)

write.csv(df, file = 'efi.csv', row.names = FALSE)

