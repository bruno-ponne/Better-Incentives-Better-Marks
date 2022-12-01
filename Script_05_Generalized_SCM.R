# Replication Code
# Better Incentives, Better Marks: A Synthetic Control Evaluation of the New Educational Policies in Ceará, Brazil
# Script 05 - Generalized Synthetic Control Method
# Bruno Ponne 


library(gsynth)
library(dplyr)


# Loading data

load("data/DATA_COMPLETE.RData")

# Data by level of education and subject:

port_primary <- DATA_COMPLETE %>%
    filter(grade == "P", subject == "port")

math_primary <- DATA_COMPLETE %>%
    filter(grade == "P", subject == "math")

port_ls <- DATA_COMPLETE %>%
    filter(grade == "LS", subject == "port")

math_ls <- DATA_COMPLETE %>%
    filter(grade == "LS", subject == "math")

port_us <- DATA_COMPLETE %>%
    filter(grade == "US", subject == "port")

math_us <- DATA_COMPLETE %>%
    filter(grade == "US", subject == "math")

port_primary$treatment = as.numeric(port_primary$abbr_state == "CE"& port_primary$year>2008)
math_primary$treatment = as.numeric(math_primary$abbr_state == "CE"& math_primary$year>2008)
port_ls$treatment = as.numeric(port_ls$abbr_state == "CE"& port_ls$year>2008)
math_ls$treatment = as.numeric(math_ls$abbr_state == "CE"& math_ls$year>2008)

port_us$treatment = as.numeric(port_us$abbr_state == "CE"& port_us$year>2010)
math_us$treatment = as.numeric(math_us$abbr_state == "CE"& math_us$year>2010)

# Estimating the synthetic controls with gsynth

# Primary Education Portuguese

out_p_port <- gsynth(score ~ treatment  + homicides + unemployment + ln_pop + TWh + edu_invest_pc, data = port_primary, 
              index = c("abbr_state","year"), force = "two-way",
              CV = TRUE, r = c(0, 5), se = TRUE, 
              inference = "nonparametric", seed = 42, nboots = 2000)

print(out_p_port)
out_p_port$est.avg
plot(out_p_port, theme.bw = TRUE, xlab = "Years relative to intervention", ylab = "Effect", main = "Primary Education - Portuguese")








out_p_math <- gsynth(score ~ treatment  + homicides + unemployment + ln_pop + TWh + edu_invest_pc, data = math_primary, 
                     index = c("abbr_state","year"), force = "two-way",
                     CV = TRUE, r = c(0, 5), se = TRUE, 
                     inference = "parametric", seed = 42, nboots = 2000)

out_ls_port <- gsynth(score ~ treatment  + homicides + unemployment + ln_pop + TWh + edu_invest_pc, data = port_ls, 
                     index = c("abbr_state","year"), force = "two-way",
                     CV = TRUE, r = c(0, 5), se = TRUE, 
                     inference = "parametric", seed = 42, nboots = 2000)


out_ls_math <- gsynth(score ~ treatment  + homicides + unemployment + ln_pop + TWh + edu_invest_pc, data = math_ls, 
                     index = c("abbr_state","year"), force = "two-way",
                     CV = TRUE, r = c(0, 5), se = TRUE, 
                     inference = "parametric", seed = 42, nboots = 2000)

out_us_port <- gsynth(score ~ treatment  + homicides + unemployment + ln_pop + TWh + edu_invest_pc, data = port_us, 
                      index = c("abbr_state","year"), force = "two-way",
                      CV = TRUE, r = c(0, 5), se = TRUE, 
                      inference = "parametric", seed = 42, nboots = 2000)


out_us_math <- gsynth(score ~ treatment  + homicides + unemployment + ln_pop + TWh + edu_invest_pc, data = math_us, 
                      index = c("abbr_state","year"), force = "two-way",
                      CV = TRUE, r = c(0, 5), se = TRUE, 
                      inference = "parametric", seed = 42, nboots = 2000)


print(out_p_math)
out_p_math$est.avg
plot(out_p_math, theme.bw = TRUE, xlab = "Years relative to intervention", ylab = "Effect", main = "Primary Education - Mathematics")

print(out_ls_port)
out_ls_port$est.avg
plot(out_ls_port, theme.bw = TRUE, xlab = "Years relative to intervention", ylab = "Effect", main = "Lower Secondary Education - Portuguese")

print(out_ls_math)
out_ls_math$est.avg
plot(out_ls_math, theme.bw = TRUE, xlab = "Years relative to intervention", ylab = "Effect", main = "Lower Secondary Education - Mathematics")

print(out_us_port)
out_us_port$est.avg
plot(out_us_port, theme.bw = TRUE, xlab = "Years relative to intervention", ylab = "Effect", main = "Lower Secondary Education - Portuguese")

print(out_us_math)
out_us_math$est.avg
plot(out_us_math, theme.bw = TRUE, xlab = "Years relative to intervention", ylab = "Effect", main = "Lower Secondary Education - Mathematics")

pairs(DATA_COMPLETE[,c(7,8,9,11,14)])
cor(DATA_COMPLETE[,c(7,8,9,10,11,14)])