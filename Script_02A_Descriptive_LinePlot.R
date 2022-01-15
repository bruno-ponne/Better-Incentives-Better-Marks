# Replication Code
# Better Incentives, Better Marks: A Synthetic Control Evaluation of the New Educational Policies in Ceará, Brazil
# Script 02A - Descriptive Statistics - Line Plots
# Bruno Ponne 


library(ggplot2)
library(dplyr)


load("data/DATA_COMPLETE.RData")

# Time Series Plots (Figures 01)

DATA_COMPLETE$group <- -1
DATA_COMPLETE$group[DATA_COMPLETE$abbr_state == "CE"] <- "CE"
DATA_COMPLETE$group[DATA_COMPLETE$abbr_state != "CE"] <- "Other States"

DATA_GRAPH <- DATA_COMPLETE %>% 
  group_by(year, group, subject, grade) %>% 
  summarise(score = mean(score))

DATA_GRAPH$grade[DATA_GRAPH$grade=="P"] <- "Primary Education"
DATA_GRAPH$grade[DATA_GRAPH$grade=="LS"] <- "Lower Secondary Education"
DATA_GRAPH$grade[DATA_GRAPH$grade=="US"] <- "Upper Secondary Education"
DATA_GRAPH$grade <- factor(DATA_GRAPH$grade, levels = c("Primary Education", "Lower Secondary Education", "Upper Secondary Education"))

DATA_GRAPH$subject[DATA_GRAPH$subject=="math"] <- "Mathematics"
DATA_GRAPH$subject[DATA_GRAPH$subject=="port"] <- "Portuguese"


ggplot(data = filter(DATA_GRAPH, grade != "Upper Secondary Education" ), aes(x=year, y= score, color = group))+
  geom_vline(xintercept = 2008, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_line(size = 0.9)+
  scale_color_manual(values= c("#01665e","#d8b365"), 
                     labels= c( "Ceará", "Average of Other States"), 
                     name = "")+
  ylab("Score")+
  xlab("Year")+
  scale_x_continuous(labels = c(1995,2000,2005,2010,2015,2020))+
  annotate("text", x = 2013, y = 159, label = "Policy Change", color = "#636363", size = 4)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_text(colour = "#636363"),
        axis.line = element_line(colour = "gray"),
        panel.border = element_rect(colour = "gray"),
        legend.position = "bottom",
        panel.spacing = unit(1.1, "lines"),
        strip.background = element_rect(fill="white", linetype = "blank"),
        text = element_text(family="Helvetica", color ="#636363"))+
  facet_grid(vars(grade), vars(subject))

ggsave(filename = "figure01.png", path = "plots", width = 19, height = 15, , units = "cm")



