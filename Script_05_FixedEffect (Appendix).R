# Replication Code
# Better Incentives, Better Marks: A Synthetic Control Evaluation of the New Educational Policies in Ceará, Brazil
# Script 05 - Fixed-effect models (Appendix)
# Bruno Ponne 

library(lmtest)
library(sandwich)
library(dplyr)
library(stargazer)

# Table C1 - Appendix



load("data/DATA_COMPLETE.RData")

# Primary School

DATA_PM <- DATA_COMPLETE %>% 
  filter(grade == 'P' & subject == 'math') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state == "CE" & year > 2008, 1, 0))

DATA_PP <- DATA_COMPLETE %>% 
  filter(grade=='P' & subject=='port') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state=="CE" & year > 2008, 1, 0))

# Primary School - mathematics
MODEL_PM03 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year) + homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_PM)
MODEL_PM03_PL <- coeftest(MODEL_PM03, vcov = vcovPL(MODEL_PM03, cluster = ~ abbr_state))
MODEL_PM03_BS <- coeftest(MODEL_PM03, vcov = vcovBS(MODEL_PM03, cluster = ~ abbr_state, type = "residual"))

# Primary School - Portuguese
MODEL_PP03 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year) + homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_PP)
MODEL_PP03_PL <- coeftest(MODEL_PP03, vcov = vcovPL(MODEL_PP03, cluster = ~ abbr_state))
MODEL_PP03_BS <- coeftest(MODEL_PP03, vcov = vcovBS(MODEL_PP03, cluster = ~ abbr_state, type = "residual"))


# Robust SE a la Newey-West (1987) and Driscoll and Kraay (1998) for panel data.

stargazer(MODEL_PM03_PL, 
          MODEL_PP03_PL, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))

# Bootstrap Robust SE

stargazer(MODEL_PM03_BS, 
          MODEL_PP03_BS, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))
# Regular SE

stargazer(MODEL_PM03, 
          MODEL_PP03, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))


# Lower Secondary School

DATA_LSM <- DATA_COMPLETE %>% 
  filter(grade=='LS' & subject=='math') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state=="CE" & year > 2008, 1, 0))


DATA_LSP <- DATA_COMPLETE %>% 
  filter(grade=='LS' & subject=='port') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state=="CE" & year > 2008, 1, 0))

# Lower Secondary School - mathematics

MODEL_LSM03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data=DATA_LSM)
MODEL_LSM03_PL <- coeftest(MODEL_LSM03, vcov = vcovPL(MODEL_LSM03, cluster = ~ abbr_state))
MODEL_LSM03_BS <- coeftest(MODEL_LSM03, vcov = vcovBS(MODEL_LSM03, cluster = ~ abbr_state, type = "residual"))

# Lower Secondary School - Portuguese

MODEL_LSP03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_LSP)
MODEL_LSP03_PL <- coeftest(MODEL_LSP03, vcov = vcovPL(MODEL_LSP03, cluster = ~ abbr_state))
MODEL_LSP03_BS <- coeftest(MODEL_LSP03, vcov = vcovBS(MODEL_LSP03, cluster = ~ abbr_state, type = "residual"))



# Robust SE a la Newey-West (1987) and Driscoll and Kraay (1998) for panel data.

stargazer(MODEL_LSM03_PL, 
          MODEL_LSP03_PL, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))

# Bootstrap Robust SE error

stargazer(MODEL_LSM03_BS, 
          MODEL_LSP03_BS, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))

# Regular SE

stargazer(MODEL_LSM03, 
          MODEL_LSP03, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))


# Upper Secondary School 

DATA_USM <- DATA_COMPLETE %>% 
  filter(grade=='US' & subject=='math') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state=="CE" & year > 2010, 1, 0))


DATA_USP <- DATA_COMPLETE %>% 
  filter(grade=='US' & subject=='port') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state=="CE" & year > 2010, 1, 0))

# Upper Secondary School - mathematics

MODEL_USM03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data=DATA_USM)
MODEL_USM03_PL <- coeftest(MODEL_USM03, vcov = vcovPL(MODEL_USM03, cluster = ~ abbr_state))
MODEL_USM03_BS <- coeftest(MODEL_USM03, vcov = vcovBS(MODEL_USM03, cluster = ~ abbr_state, type = "wild-webb"))

# Upper Secondary School - Portuguese

MODEL_USP03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_USP)
MODEL_USP03_PL <- coeftest(MODEL_USP03, vcov = vcovPL(MODEL_USP03, cluster = ~ abbr_state))
MODEL_USP03_BS <- coeftest(MODEL_USP03, vcov = vcovBS(MODEL_USP03, cluster = ~ abbr_state, type = "wild-webb"))

# Robust SE a la Newey-West (1987) and Driscoll and Kraay (1998) for panel data.

stargazer(MODEL_USM03_PL, 
          MODEL_USP03_PL, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))
# Bootstrap Robust SE

stargazer(MODEL_USM03_BS, 
          MODEL_USP03_BS, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))
# Regular SE

stargazer(MODEL_USM03, 
          MODEL_USP03, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))


# two-way clustering with Bootstrap Robust SE - Note 01 in Appendix:


# Primary Education
MODEL_PM03 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year) + homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_PM)
summary(MODEL_PM03)
MODEL_PM03_BS_TW <- coeftest(MODEL_PM03, vcov = vcovBS(MODEL_PM03, cluster = ~ abbr_state + year, type = "residual"))
MODEL_PM03_BS_TW

MODEL_PP03 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year) + homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_PP)
summary(MODEL_PP03)
MODEL_PP03_BS_TW <- coeftest(MODEL_PP03, vcov = vcovBS(MODEL_PP03, cluster = ~ abbr_state + year, type = "residual"))
MODEL_PP03_BS_TW 

# Lower Secondary Education

MODEL_LSM03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data=DATA_LSM)
summary(MODEL_LSM03)
MODEL_LSM03_BS_TW <- coeftest(MODEL_LSM03, vcov = vcovBS(MODEL_LSM03, cluster = ~ abbr_state+year, type = "residual"))
MODEL_LSM03_BS_TW

MODEL_LSP03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_LSP)
summary(MODEL_LSP03)
MODEL_LSP03_BS_TW <- coeftest(MODEL_LSP03, vcov = vcovBS(MODEL_LSP03, cluster = ~ abbr_state+year, type = "residual"))
MODEL_LSP03_BS_TW
