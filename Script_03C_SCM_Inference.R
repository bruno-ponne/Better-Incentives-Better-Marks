# Replication Code
# Better Incentives, Better Marks: A Synthetic Control Evaluation of the New Educational Policies in Ceará, Brazil
# Script 03C - Synthetic Control Inference (Permutation Test)  
# Bruno Ponne 


library(Synth)
library(dplyr)
library(ggpubr)

source("functions/permutate_p_ls.R")
source("functions/permutate_us.R")


load("data/DATA_COMPLETE.RData")

PRIMARY_M <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "math"))
PRIMARY_P <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "port"))
LOWERS_M <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "math"))
LOWERS_P <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "port"))
UPPERS_M <- as.data.frame(filter(DATA_COMPLETE, grade == "US", subject == "math"))
UPPERS_P <- as.data.frame(filter(DATA_COMPLETE, grade == "US", subject == "port"))

############################################################
## Permutation test for Primary and Lower Secondary School #
############################################################

# Executing the synthetic control for each state in each subgroup:
GAP_PRIMARY_M <- permutate_p_ls(PRIMARY_M)
GAP_PRIMARY_M$subject <- "Math"
GAP_PRIMARY_M$grade <- "Primary School"

GAP_PRIMARY_P <- permutate_p_ls(PRIMARY_P)
GAP_PRIMARY_P$subject <- "Portuguese"
GAP_PRIMARY_P$grade <- "Primary School"

GAP_LOWERS_M <- permutate_p_ls(LOWERS_M)
GAP_LOWERS_M$subject <- "Math"
GAP_LOWERS_M$grade <- "Lower Secondary School"

GAP_LOWERS_P <- permutate_p_ls(LOWERS_P)
GAP_LOWERS_P$subject <- "Portuguese"
GAP_LOWERS_P$grade <- "Lower Secondary School"

# Joining all the data for the plots
GAP_P_LS <- rbind(GAP_PRIMARY_M, GAP_PRIMARY_P, GAP_LOWERS_M, GAP_LOWERS_P)

GAP_P_LS$grade <- factor(GAP_P_LS$grade, levels = c("Primary School", "Lower Secondary School"))

GAP_P_LS$isCE[GAP_P_LS$code_state == "X23"] <- "Ceará"
GAP_P_LS$isCE[GAP_P_LS$code_state != "X23"] <- "Control States"

GAP_P_LS$isCE <- factor(GAP_P_LS$isCE, levels = c("Control States", "Ceará"))

# Figure 12:

# Permutation Graphs with all 27 states except one (the one treated with placebo)

GAP_P_LS$subject[GAP_P_LS$subject=="Math"] <- "Mathematics"

GAP_P_LS$grade <- if_else(GAP_P_LS$grade == "Primary School", "Primary Education", "Lower Secondary Education" )

GAP_P_LS$grade <- factor(GAP_P_LS$grade, levels = c("Primary Education", "Lower Secondary Education"))

a_12 <- ggplot()+
  geom_vline(xintercept = 2008, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_vline(xintercept = 2011, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_hline(yintercept = 0, size = 0.7, color = "#636363")+
  geom_line(data = filter(GAP_P_LS, code_state != "X23", grade == "Primary Education"), aes(x=year, y= gap, group = code_state, color = "Control States"),size=0.5)+
  geom_line(data = filter(GAP_P_LS, code_state == "X23", grade == "Primary Education"), aes(x=year, y= gap, color = "Ceará"), size=1.0)+
  scale_color_manual(name='',
                     breaks=c('Control States', 'Ceará'),
                     values=c('Control States'='gray', "Ceará" = "#01665e"))+
  ylab("Effect")+
  xlab("Year")+
  annotate("text", x = 2007, y = 35, label = "TI", color = "#636363", size = 4)+
  annotate("text", x = 2013, y = -35, label = "TI + TA", color = "#636363", size = 4)+
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
  ylim(-40,40)+
  facet_grid(vars(grade), vars(subject))

b_12 <- ggplot()+
  geom_vline(xintercept = 2008, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_vline(xintercept = 2015, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_hline(yintercept = 0, size = 0.7, color = "#636363")+
  geom_line(data = filter(GAP_P_LS, code_state != "X23", grade == "Lower Secondary Education"), aes(x=year, y= gap, group = code_state, color = "Control States"),size=0.5)+
  geom_line(data = filter(GAP_P_LS, code_state == "X23", grade == "Lower Secondary Education"), aes(x=year, y= gap, color = "Ceará"), size=1.0)+
  scale_color_manual(name='',
                     breaks=c('Control States', 'Ceará'),
                     values=c('Control States'='gray', "Ceará" = "#01665e"))+
  ylab("Effect")+
  xlab("Year")+
  annotate("text", x = 2007, y = 35, label = "TI", color = "#636363", size = 4)+
  annotate("text", x = 2017, y = -35, label = "TI + TA", color = "#636363", size = 4)+
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
  ylim(-40,40)+
  facet_grid(vars(grade), vars(subject))

ggarrange(a_12, b_12, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")


ggsave(filename = "figure12.png", path = "plots", width = 21, height = 15, , units = "cm")


# Preintervention mean squared prediction error (MSPE)

MSPE <- GAP_P_LS %>%
  filter(year < 2008) %>% 
  group_by(code_state, grade, subject) %>% 
  summarize(MSPE = mean(gap*gap))

PLOT_MSPE <- left_join(GAP_P_LS, MSPE, by = c("code_state", "grade", "subject"))

# MSPE for Ceará (Primary - Math): 19.75
# 2x19.75 = 39.5
MSPE_PRIMARY_M <- PLOT_MSPE %>% 
  filter(grade == "Primary Education", subject == "Mathematics", MSPE < 39.5)

# MSPE for Ceará (Primary - Portuguese): 8.8
# 2x8.8 = 17.6
MSPE_PRIMARY_P <- PLOT_MSPE %>% 
  filter(grade == "Primary Education", subject == "Portuguese", MSPE < 17.6)

# MSPE for Ceará (Lower Secondary - Math): 12.15
# 2x12.15 = 24.3
MSPE_LS_M <- PLOT_MSPE %>% 
  filter(grade=="Lower Secondary Education", subject == "Mathematics", MSPE<24.3)

# MSPE for Ceará (Lower Secondary - Math): 6.44
# 2x6.44 = 12.88
MSPE_LS_P <- PLOT_MSPE %>% 
  filter(grade=="Lower Secondary Education", subject == "Portuguese", MSPE<12.88)

GAP_P_LS_2x <- rbind(MSPE_PRIMARY_M, MSPE_PRIMARY_P, MSPE_LS_M, MSPE_LS_P)

# Figure 13:
# Permutation Graphs ONLY with states whose pre-intervention MSPE was lower than 2x pre-intervention MSPE Ceará


a_13 <- ggplot()+
  geom_vline(xintercept = 2008, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_vline(xintercept = 2011, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_hline(yintercept = 0, size = 0.7, color = "#636363")+
  geom_line(data = filter(GAP_P_LS_2x, code_state != "X23", grade == "Primary Education"), aes(x=year, y= gap, group = code_state, color = "Control States"),size=0.5)+
  geom_line(data = filter(GAP_P_LS_2x, code_state == "X23", grade == "Primary Education"), aes(x=year, y= gap, color = "Ceará"), size=1.0)+
  scale_color_manual(name='',
                     breaks=c('Control States', 'Ceará'),
                     values=c('Control States'='gray', "Ceará" = "#01665e"))+
  ylab("Effect")+
  xlab("Year")+
  annotate("text", x = 2007, y = 35, label = "TI", color = "#636363", size = 4)+
  annotate("text", x = 2013, y = -35, label = "TI + TA", color = "#636363", size = 4)+
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
  ylim(-40,40)+
  facet_grid(vars(grade), vars(subject))

b_13 <- ggplot()+
  geom_vline(xintercept = 2008, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_vline(xintercept = 2015, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_hline(yintercept = 0, size = 0.7, color = "#636363")+
  geom_line(data = filter(GAP_P_LS_2x, code_state != "X23", grade == "Lower Secondary Education"), aes(x=year, y= gap, group = code_state, color = "Control States"),size=0.5)+
  geom_line(data = filter(GAP_P_LS_2x, code_state == "X23", grade == "Lower Secondary Education"), aes(x=year, y= gap, color = "Ceará"), size=1.0)+
  scale_color_manual(name='',
                     breaks=c('Control States', 'Ceará'),
                     values=c('Control States'='gray', "Ceará" = "#01665e"))+
  ylab("Effect")+
  xlab("Year")+
  annotate("text", x = 2007, y = 35, label = "TI", color = "#636363", size = 4)+
  annotate("text", x = 2017, y = -35, label = "TI + TA", color = "#636363", size = 4)+
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
  ylim(-40,40)+
  facet_grid(vars(grade), vars(subject))

ggarrange(a_13, b_13, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")


ggsave(filename = "figure13.png", path = "plots", width = 21, height = 15, , units = "cm")






############################################################
## Post/Pre Reform MSPE                                    #
############################################################

# Preintervention mean squared prediction error (MSPE)

MSPE_POST <- GAP_P_LS %>%
  filter(year > 2008) %>% 
  group_by(code_state, grade, subject) %>% 
  summarize(MSPE_POST = mean(gap*gap))

POST_PRE <- left_join(MSPE, MSPE_POST, by = c("code_state", "grade","subject"))

POST_PRE$RATIO <- POST_PRE$MSPE_POST/POST_PRE$MSPE

POST_PRE$isCE[POST_PRE$code_state == "X23"] <- "Ceará"
POST_PRE$isCE[POST_PRE$code_state != "X23"] <- "Control States"

# Figure 14:

POST_PRE$isCE <- factor(POST_PRE$isCE, levels = c("Control States", "Ceará"))


ggplot(data = POST_PRE, aes(x= RATIO, color = isCE, fill = isCE))+
  geom_histogram(alpha = 0.7)+
  scale_color_manual(values= c("#d8b365","#01665e"), 
                     labels= c( "Control States", "Ceará"), 
                     name = "")+
  scale_fill_manual(values= c("#d8b365","#01665e"), 
                     labels= c( "Control States", "Ceará"), 
                     name = "")+
  ylab("Frequency")+
  xlab("Post/Pre-intervention Mean Squared Prediction Error")+
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

ggsave(filename = "figure14.png", path = "plots", width = 21, height = 15, , units = "cm")







