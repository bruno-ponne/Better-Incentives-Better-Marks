# Replication Code
# Better Incentives, Better Marks: A Synthetic Control Evaluation of the New Educational Policies in Ceará, Brazil
# Script 01 - Analytical Sample Statistics
# Bruno Ponne                       


#Libraries needed for this script:
  
library(tidyverse)
library(table1)

# Loading data:

load("data/DATA_COMPLETE.RData")

# Table 2: Primary Education

PRIMARY <- filter(DATA_COMPLETE, grade == "P")

PRIMARY$TIME[PRIMARY$year <= 2007] <- "Pre-Reform"
PRIMARY$TIME[PRIMARY$year > 2007] <- "Post-Reform"
PRIMARY$TIME <- factor(PRIMARY$TIME, levels = c("Pre-Reform", "Post-Reform"))


PRIMARY$STATE[PRIMARY$abbr_state == "CE"] <- "Ceará"
PRIMARY$STATE[PRIMARY$abbr_state != "CE"] <- "Other States"

PRIMARY <- spread(PRIMARY, subject, score)


table1::label(PRIMARY$math) <- "Score in Mathematics"
table1::label(PRIMARY$port) <- "Score in Portuguese" 
table1::label(PRIMARY$edu_invest_pc) <- "Investment in education and culture per capita" 
table1::label(PRIMARY$homicides) <- "Homicides per 100,000 inhabitants"
table1::label(PRIMARY$ln_pop) <- "Natural logarithm of the population"
table1::label(PRIMARY$unemployment) <- "Unemployment (in %)"
table1::label(PRIMARY$TWh) <- "Industrial Electricity Consumption in TWh"

table1(~ math + port + edu_invest_pc + homicides + ln_pop + unemployment + TWh | TIME*STATE, data=PRIMARY)

# Table 2: Lower Secondary Education

LOWER_S <- filter(DATA_COMPLETE, grade == "LS")

LOWER_S$TIME[LOWER_S$year <= 2007] <- "Pre-Reform"
LOWER_S$TIME[LOWER_S$year > 2007] <- "Post-Reform"
LOWER_S$TIME <- factor(LOWER_S$TIME, levels = c("Pre-Reform", "Post-Reform"));

LOWER_S$STATE[LOWER_S$abbr_state == "CE"] <- "Ceará"
LOWER_S$STATE[LOWER_S$abbr_state != "CE"] <- "Other States"

LOWER_S <- pivot_wider(LOWER_S, names_from = subject, values_from = score)

table1::label(LOWER_S$math) <- "Score in Mathematics"
table1::label(LOWER_S$port) <- "Score in Portuguese" 
table1::label(LOWER_S$edu_invest_pc) <- "Investment in education and culture per capita" 
table1::label(LOWER_S$homicides) <- "Homicides per 100,000 inhabitants"
table1::label(LOWER_S$ln_pop) <- "Natural logarithm of the population"
table1::label(LOWER_S$unemployment) <- "Unemployment (in %)"
table1::label(LOWER_S$TWh) <- "Industrial Electricity Consumption in TWh"

table1(~ math + port + edu_invest_pc + homicides + ln_pop + unemployment + TWh | TIME*STATE, data=LOWER_S)

# Table 2: Upper Secondary Education

UPPER_S <- filter(DATA_COMPLETE, grade == "US")
UPPER_S$TIME <- -1
UPPER_S$TIME[UPPER_S$year <= 2010] <- "Pre-Reform"            # DIFERENT PRE AND POST PERIODS!
UPPER_S$TIME[UPPER_S$year > 2010] <- "Post-Reform"
UPPER_S$TIME <- factor(UPPER_S$TIME, levels = c("Pre-Reform", "Post-Reform"));


UPPER_S$STATE[UPPER_S$abbr_state == "CE"] <- "Ceará"
UPPER_S$STATE[UPPER_S$abbr_state != "CE"] <- "Other States"

UPPER_S <- pivot_wider(UPPER_S, names_from = subject, values_from = score)

table1::label(UPPER_S$math) <- "Score in Mathematics"
table1::label(UPPER_S$port) <- "Score in Portuguese" 
table1::label(UPPER_S$edu_invest_pc) <- "Investment in education and culture per capita" 
table1::label(UPPER_S$homicides) <- "Homicides per 100,000 inhabitants"
table1::label(UPPER_S$ln_pop) <- "Natural logarithm of the population"
table1::label(UPPER_S$unemployment) <- "Unemployment (in %)"
table1::label(UPPER_S$TWh) <- "Industrial Electricity Consumption in TWh"

table1(~ math + port + edu_invest_pc + homicides + ln_pop + unemployment + TWh | TIME*STATE, data=UPPER_S)

# Last Column

# Complete Analytical Sample - Primary

table1::label(PRIMARY$math) <- "Score in Mathematics"
table1::label(PRIMARY$port) <- "Score in Portuguese" 
table1::label(PRIMARY$edu_invest_pc) <- "Investment in education and culture per capita" 
table1::label(PRIMARY$homicides) <- "Homicides per 100,000 inhabitants"
table1::label(PRIMARY$ln_pop) <- "Natural logarithm of the population"
table1::label(PRIMARY$unemployment) <- "Unemployment (in %)"
table1::label(PRIMARY$TWh) <- "Industrial Electricity Consumption in TWh"

table1(~ math + port + edu_invest_pc + homicides + ln_pop + unemployment + TWh , data=PRIMARY)

# Complete Analytical Sample - Lower Secondary

table1::label(LOWER_S$math) <- "Score in Mathematics"
table1::label(LOWER_S$port) <- "Score in Portuguese" 
table1::label(LOWER_S$edu_invest_pc) <- "Investment in education and culture per capita" 
table1::label(LOWER_S$homicides) <- "Homicides per 100,000 inhabitants"
table1::label(LOWER_S$ln_pop) <- "Natural logarithm of the population"
table1::label(LOWER_S$unemployment) <- "Unemployment (in %)"
table1::label(LOWER_S$TWh) <- "Industrial Electricity Consumption in TWh"

table1(~ math + port + edu_invest_pc + homicides + ln_pop + unemployment + TWh, data=LOWER_S)

# Complete Analytical Sample - Upper Secondary

table1::label(UPPER_S$math) <- "Score in Mathematics"
table1::label(UPPER_S$port) <- "Score in Portuguese" 
table1::label(UPPER_S$edu_invest_pc) <- "Investment in education and culture per capita" 
table1::label(UPPER_S$homicides) <- "Homicides per 100,000 inhabitants"
table1::label(UPPER_S$ln_pop) <- "Natural logarithm of the population"
table1::label(UPPER_S$unemployment) <- "Unemployment (in %)"
table1::label(UPPER_S$TWh) <- "Industrial Electricity Consumption in TWh"

table1(~ math + port + edu_invest_pc + homicides + ln_pop + unemployment + TWh, data=UPPER_S)
