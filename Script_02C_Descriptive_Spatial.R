# Replication Code
# Better Incentives, Better Marks: A Synthetic Control Evaluation of the New Educational Policies in Ceará, Brazil
# Script 02C - Descriptive Statistics - Spatial Plots
# Bruno Ponne 

library(sf)
library(ggplot2)
library(dplyr)


# Loading map:
states_map <- read_sf("map/UFEBRASIL.shp")
states_map <- rename(states_map, code_state = CD_GEOCODU)


# Loading abbreviations of the states:
load("data/abbr_code.RData")


# Loading SAEB data:
load("data/DATA_COMPLETE.RData")

# Primary education (Figure 5)
DATA_2007_19 <- DATA_COMPLETE %>% 
  filter(year == 2007 | year == 2019, grade == "P") %>% 
  mutate(code_state = as.character(code_state))

IDEB_MAP_M_P <- left_join(states_map, DATA_2007_19, by = "code_state")
IDEB_MAP_M_P$year[IDEB_MAP_M_P$year==2007] <- "Pre-Intervention"
IDEB_MAP_M_P$year[IDEB_MAP_M_P$year==2019] <- "Post-Intervention"
IDEB_MAP_M_P$year <- factor(IDEB_MAP_M_P$year, levels = c("Pre-Intervention", "Post-Intervention"))

IDEB_MAP_M_P$subject[IDEB_MAP_M_P$subject=="math"] <- "Mathematics"
IDEB_MAP_M_P$subject[IDEB_MAP_M_P$subject=="port"] <- "Portuguese"

IDEB_MAP_M_P_CE <- IDEB_MAP_M_P %>% filter(code_state==23)

max_score <- round(max(IDEB_MAP_M_P$score))
min_score <- round(min(IDEB_MAP_M_P$score))

ggplot()+ 
  geom_sf(data = IDEB_MAP_M_P, aes(fill = score), size= 0)+
  scale_fill_gradient(name = "Score", breaks = c(min_score, 200, max_score), labels = c(min_score, 200, max_score), low = "#d8b365", high ="#01665e")+
  geom_sf(data = IDEB_MAP_M_P_CE, aes(fill = score), size= 0.2, color = "white" )+
  facet_grid(vars(year), vars(subject))+
  theme_bw()+
  theme(axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      strip.text = element_text(colour = "#636363"),
      axis.line = element_line(colour = "gray"),
      panel.border = element_rect(colour = "gray"),
      legend.position = "bottom",
      strip.background = element_rect(fill="white", linetype = "blank"),
      text = element_text(family="Helvetica", color ="#636363"))

ggsave(filename = "figure05.png", path = "plots", width = 12, height = 12, units = "cm")

# Figure B1

DATA_2007_19 <- DATA_COMPLETE %>% 
  filter(year == 2007 | year == 2019, grade == "LS") %>% 
  mutate(code_state = as.character(code_state))

IDEB_MAP_M_P <- left_join(states_map, DATA_2007_19, by = "code_state")
IDEB_MAP_M_P$year[IDEB_MAP_M_P$year==2007] <- "Pre-Intervention"
IDEB_MAP_M_P$year[IDEB_MAP_M_P$year==2019] <- "Post-Intervention"
IDEB_MAP_M_P$year <- factor(IDEB_MAP_M_P$year, levels = c("Pre-Intervention", "Post-Intervention"))

IDEB_MAP_M_P$subject[IDEB_MAP_M_P$subject=="math"] <- "Mathematics"
IDEB_MAP_M_P$subject[IDEB_MAP_M_P$subject=="port"] <- "Portuguese"

IDEB_MAP_M_P_CE <- IDEB_MAP_M_P %>% filter(code_state==23)

max_score <- round(max(IDEB_MAP_M_P$score))
min_score <- round(min(IDEB_MAP_M_P$score))

ggplot()+ 
  geom_sf(data = IDEB_MAP_M_P, aes(fill = score), size= 0)+
  scale_fill_gradient(name = "Score", breaks = c(min_score, 240, max_score), labels = c(min_score, 240, max_score), low = "#d8b365", high ="#01665e")+
  geom_sf(data = IDEB_MAP_M_P_CE, aes(fill = score), size= 0.2, color = "white" )+
  facet_grid(vars(year), vars(subject))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_text(colour = "#636363"),
        axis.line = element_line(colour = "gray"),
        panel.border = element_rect(colour = "gray"),
        legend.position = "bottom",
        strip.background = element_rect(fill="white", linetype = "blank"),
        text = element_text(family="Helvetica", color ="#636363"))


ggsave(filename = "figureB1.png", path = "plots", width = 12, height = 12, units = "cm")

# Figure B2

DATA_US <- DATA_COMPLETE %>% 
  filter(year == 2007 | year == 2019, grade == "US") %>% 
  mutate(code_state = as.character(code_state))

DATA_B2 <- left_join(states_map, DATA_US, by = "code_state")
DATA_B2$year[DATA_B2$year==2007] <- "Pre-Intervention"
DATA_B2$year[DATA_B2$year==2019] <- "Post-Intervention"
DATA_B2$year <- factor(DATA_B2$year, levels = c("Pre-Intervention", "Post-Intervention"))

DATA_B2$subject[DATA_B2$subject=="math"] <- "Mathematics"
DATA_B2$subject[DATA_B2$subject=="port"] <- "Portuguese"

DATA_B2_CE <- DATA_B2 %>% filter(code_state==23)

B2_max_score <- round(max(DATA_B2$score))
B2_min_score <- round(min(DATA_B2$score))

ggplot()+ 
  geom_sf(data = DATA_B2, aes(fill = score), size= 0)+
  scale_fill_gradient(name = "Score", breaks = c(B2_min_score, 260, B2_max_score), labels = c(B2_min_score, 260, B2_max_score), low = "#d8b365", high ="#01665e")+
  geom_sf(data = DATA_B2_CE, aes(fill = score), size= 0.2, color = "white" )+
  facet_grid(vars(year), vars(subject))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_text(colour = "#636363"),
        axis.line = element_line(colour = "gray"),
        panel.border = element_rect(colour = "gray"),
        legend.position = "bottom",
        strip.background = element_rect(fill="white", linetype = "blank"),
        text = element_text(family="Helvetica", color ="#636363"))


ggsave(filename = "figureB2.png", path = "plots", width = 12, height = 12, units = "cm")
