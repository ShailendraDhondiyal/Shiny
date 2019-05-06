library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

df <- read.csv(file = 'efi.csv', stringsAsFactors = FALSE)

df$Year <- as.factor(df$Year)

# drop-down choices for selectInput
resp_var <- c('GDP.per.Capita.PPP', 'Unemployment.Rate', 'Inflation')
pred_var <- c('X.Score', 'rule_law', 'govt_size', 'reg_eff', 'mkt_open')


# Creating quantiles for box-plots and mosaic-plots
df2019 <- df %>% filter(., Year == 2019) %>% filter(., !is.na(GDP.per.Capita.PPP))

df2019$income.quartile <- cut(df2019$GDP.per.Capita.PPP, 4, labels = c("lo_inc", "loav_inc", "hiav_inc", "hi_inc"))
df2019 <- df2019 %>% filter(., !is.na(income.quartile))

df2019$rule.law.quartile <- cut(df2019$rule_law, 4, labels = c("lo_rule_law", "loav_rule_law", "hiav_rule_law", "hi_rule_law"))
df2019$govt.size.quartile <- cut(df2019$govt_size, 4, labels = c("lo_govt_size", "loav_govt_size", "hiav_govt_size", "hi_govt_size"))
df2019$reg.eff.quartile <- cut(df2019$reg_eff, 4, labels = c("lo_reg_eff", "loav_reg_eff", "hiav_reg_eff", "hi_reg_eff"))
df2019$mkt.open.quartile <- cut(df2019$mkt_open, 4, labels = c("lo_mkt_open", "loav_mkt_open", "hiav_mkt_open", "hi_mkt_open"))


# tables for mosaic-plot
df2019_law <- df2019 %>% 
  filter(., !is.na(income.quartile), !is.na(rule.law.quartile)) %>%
  select(., income.quartile, rule.law.quartile)
table2019_law <- table(df2019_law)

df2019_govt <- df2019 %>% 
  filter(., !is.na(income.quartile), !is.na(govt.size.quartile)) %>%
  select(., income.quartile, govt.size.quartile)
table2019_govt <- table(df2019_govt)

df2019_reg <- df2019 %>% 
  filter(., !is.na(income.quartile), !is.na(reg.eff.quartile)) %>%
  select(., income.quartile, reg.eff.quartile)
table2019_reg <- table(df2019_reg)

df2019_mkt <- df2019 %>% 
  filter(., !is.na(income.quartile), !is.na(mkt.open.quartile)) %>%
  select(., income.quartile, mkt.open.quartile)
table2019_mkt <- table(df2019_mkt)


