# Replication Code
# Better Incentives, Better Marks: A Synthetic Control Evaluation of the New Educational Policies in Ceará, Brazil
# Script 02B - Descriptive Statistics - Density Plots
# Bruno Ponne 

library(ggplot2)
library(dplyr)

# Loading data:

load("data/DATA_COMPLETE.RData")

# Figure 03

DATA_DIST <- DATA_COMPLETE %>% 
  select(abbr_state, grade, year, score)

DATA_DIST$LEGEND <- -1
DATA_DIST$LEGEND[DATA_DIST$abbr_state == "CE"] <- "Ceará"
DATA_DIST$LEGEND[DATA_DIST$abbr_state != "CE"] <- "Other States"

DATA_DIST$PERIOD <- -1
DATA_DIST$PERIOD[DATA_DIST$year <= 2007] <- "Pre-Intervention"
DATA_DIST$PERIOD[DATA_DIST$year >= 2011] <- "Post-Intervention"
DATA_DIST$PERIOD <- factor(DATA_DIST$PERIOD, levels = c("Pre-Intervention", "Post-Intervention"))

DATA_DIST$grade[DATA_DIST$grade == "P"] <- "Primary Education"
DATA_DIST$grade[DATA_DIST$grade == "LS"] <- "Lower Secondary Education"
DATA_DIST$grade[DATA_DIST$grade == "US"] <- "Upper Secondary Education"
DATA_DIST$grade <- factor(DATA_DIST$grade, levels = c("Primary Education", "Lower Secondary Education", "Upper Secondary Education"))
DATA_DIST <- na.omit(DATA_DIST)

ggplot(data = DATA_DIST, aes(x = score, color = LEGEND))+
  geom_density(size = 0.8)+
  scale_color_manual(values= c("#01665e","#d8b365"), 
                     labels= c( "Ceará", "Average of Other States"), 
                     name = "")+
  scale_linetype_manual(values= c("solid","dashed"), 
                        labels= c("Post", "Pre"), 
                        name = "Time:")+
  ylab("Density")+
  xlab("Scores in mathematics and Portuguese")+
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
  guides(color = guide_legend(override.aes = list(collor = c("#01665e","#d8b365"), fill = c("#01665e","#d8b365"))))+
  xlim(130,300)+
  facet_grid(vars(PERIOD),vars(grade))

ggsave(filename = "figure03.png", path = "plots", width = 18, height = 14, , units = "cm")

