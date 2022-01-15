# Replication Code
# Better Incentives, Better Marks: A Synthetic Control Evaluation of the New Educational Policies in Ceará, Brazil
# Script 04 - Size of effect in months of schooling
# Bruno Ponne 


library(dplyr)

load("data/DATA_COMPLETE.RData")

# Cohort 1: Students in the 5th grade in 2015 and in the 9th grade in 2019:

students_2015_primary <- DATA_COMPLETE %>% 
  filter(year==2015, grade == "P", subject == "port", abbr_state!="CE")

mean(students_2015_primary$score)

students_2019_lower_secondary <- DATA_COMPLETE %>% 
  filter(year==2019, grade == "LS", subject == "port", abbr_state!="CE")

diff_score_1 = mean(students_2019_lower_secondary$score)-mean(students_2015_primary$score)

# Cohort 2: Students in the 5th grade in 2011 and in the 9th grade in 2015:

students_2011_primary <- DATA_COMPLETE %>% 
  filter(year==2011, grade == "P", subject == "port", abbr_state!="CE")

students_2015_lower_secondary <- DATA_COMPLETE %>% 
  filter(year==2015, grade == "LS", subject == "port", abbr_state!="CE")

diff_score_2 = mean(students_2015_lower_secondary$score)-mean(students_2011_primary$score)

# Cohort 3: Students in the 5th grade in 2007 and in the 9th grade in 2011:

students_2007_primary <- DATA_COMPLETE %>% 
  filter(year==2007, grade == "P", subject == "port", abbr_state!="CE")

students_2011_lower_secondary <- DATA_COMPLETE %>% 
  filter(year==2011, grade == "LS", subject == "port", abbr_state!="CE")

diff_score_3 = mean(students_2011_lower_secondary$score)-mean(students_2007_primary$score)

mean(c(diff_score_1,diff_score_2,diff_score_3))/4

# Note 10: 14.9 points per year.


