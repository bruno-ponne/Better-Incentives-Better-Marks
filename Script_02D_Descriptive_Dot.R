# Replication Code
# Better Incentives, Better Marks: A Synthetic Control Evaluation of the New Educational Policies in Ceará, Brazil
# Script 02D - Descriptive Statistics - Dot Plots
# Bruno Ponne 


library(ggplot2)
library(dplyr)

load("data/DATA_COMPLETE.RData")

# Figure 04

EFFICIENCY <- DATA_COMPLETE %>% 
  filter(year>=2007) 

AVERAGE_INVEST <- EFFICIENCY %>% 
  group_by(abbr_state) %>% 
  summarise(avg_inv = mean(edu_invest_pc))

SCORE_2007 <- EFFICIENCY %>% 
  filter(year==2007) %>% 
  mutate(score_2007 = score)

SCORE_2019 <- EFFICIENCY %>% 
  filter(year==2019) %>% 
  mutate(score_2019 = score)

SCORE_CHANGE <- left_join(SCORE_2007, 
                          SCORE_2019, 
                          by = c("abbr_state", "grade", "subject")) %>%
  mutate(score_change = score_2019 - score_2007)

SCORE_CHANGE <- select(SCORE_CHANGE, abbr_state, grade, subject, score_change)


PLOT_DATA <- left_join(SCORE_CHANGE, AVERAGE_INVEST, by = "abbr_state")
PLOT_DATA$grade[PLOT_DATA$grade=="P"] <- "Primary Education"
PLOT_DATA$grade[PLOT_DATA$grade=="LS"] <- "Lower Secondary Education"
PLOT_DATA$grade[PLOT_DATA$grade=="US"] <- "Upper Secondary Education"

PLOT_DATA$subject[PLOT_DATA$subject=="math"] <- "Mathematics"
PLOT_DATA$subject[PLOT_DATA$subject=="port"] <- "Portuguese"

PLOT_DATA$FACET <- -1
PLOT_DATA$FACET <- paste(PLOT_DATA$grade, "-", PLOT_DATA$subject)

PLOT_DATA$grade <- factor(PLOT_DATA$grade, 
                          levels = c("Primary Education", 
                                     "Lower Secondary Education",
                                     "Upper Secondary Education"))

ggplot(data = PLOT_DATA, aes(x = avg_inv, 
                             y = score_change, 
                             color = abbr_state == "CE",
                             shape = abbr_state == "CE"))+
  geom_point(size=3, alpha = 0.8)+
  scale_shape_manual(values=c(16,17), 
                     labels=c("Other States", "Ceará"), 
                     name = "")+
  scale_color_manual(values= c("#d8b365","#01665e"), 
                     labels= c("Other States", "Ceará"), 
                     name = "")+
  ylab("Score Change between 2007 and 2019")+
  xlab("Average Spending in education and culture per capita between 2007 and 2019")+
  theme_bw()+
  theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_text(colour = "#636363"),
        axis.line = element_line(colour = "gray"),
        panel.border = element_rect(colour = "gray"),
        legend.position = "bottom",
        strip.background = element_rect(fill="white", linetype = "blank"),
        text = element_text(family="Helvetica", color ="#636363"))+
  facet_grid(vars(subject),vars(grade))

ggsave(filename = "figure04.png", path = "plots", width = 18, height = 14, , units = "cm")


