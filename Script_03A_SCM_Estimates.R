# Replication Code
# Better Incentives, Better Marks: A Synthetic Control Evaluation of the New Educational Policies in Ceará, Brazil
# Script 03A - Synthetic Control Estimates and in-time placebo test
# Bruno Ponne 

library(dplyr)
library(Synth)
library(kableExtra)
library(xtable)
library(gridExtra)


source("functions/plot_scm.R")
source("functions/prepare_p_ls.R")
source("functions/prepare_us.R")
source("functions/prepare_time_placebo.R")


load("data/DATA_COMPLETE.RData")
load("data/abbr_code.RData")

# Subsetting data:

PRIMARY_M <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "math"))
PRIMARY_P <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "port"))
LOWERS_M <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "math"))
LOWERS_P <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "port"))
UPPERS_M <- as.data.frame(filter(DATA_COMPLETE, grade == "US", subject == "math"))
UPPERS_P <- as.data.frame(filter(DATA_COMPLETE, grade == "US", subject == "port"))


# Preparing data for Synth:
DATA_PM <- prepare_p_ls(PRIMARY_M)
DATA_PP <- prepare_p_ls(PRIMARY_P)

DATA_LSM <- prepare_p_ls(LOWERS_M)
DATA_LSP <- prepare_p_ls(LOWERS_P)

DATA_USM <- prepare_us(UPPERS_M)
DATA_USP <- prepare_us(UPPERS_P)


##############################################
# SCM for primary and lower secondary school #
##############################################

# Run synth
SCM_PM    <- synth(DATA_PM)
TABLES_PM <- synth.tab(dataprep.res = DATA_PM, synth.res = SCM_PM)

SCM_PP    <- synth(DATA_PP)
TABLES_PP <- synth.tab(dataprep.res = DATA_PP, synth.res = SCM_PP)

SCM_LSM    <- synth(DATA_LSM)
TABLES_LSM <- synth.tab(dataprep.res = DATA_LSM, synth.res = SCM_LSM)

SCM_LSP    <- synth(DATA_LSP)
TABLES_LSP <- synth.tab(dataprep.res = DATA_LSP, synth.res = SCM_LSP)


# Graphs in ggplot
PM <- plot_scm(PRIMARY_M, TABLES_PM)
PM_SC <- PM[[1]]
PM_GAP <- PM[[2]]

PP <- plot_scm(PRIMARY_P, TABLES_PP)
PP_SC <- PP[[1]]
PP_GAP <- PP[[2]]

LSM <- plot_scm(LOWERS_M, TABLES_LSM)
LSM_SC <- LSM[[1]]
LSM_GAP <- LSM[[2]]

LSP <- plot_scm(LOWERS_P, TABLES_LSP)
LSP_SC <- LSP[[1]]
LSP_GAP <- LSP[[2]]

DATA_GRAPH <- rbind(PM_SC, PP_SC, LSM_SC, LSP_SC)

DATA_GRAPH$grade[DATA_GRAPH$grade=="P"] <- "Primary Education"
DATA_GRAPH$grade[DATA_GRAPH$grade=="LS"] <- "Lower Secondary Education"
DATA_GRAPH$grade <- factor(DATA_GRAPH$grade, levels = c("Primary Education", "Lower Secondary Education"))

DATA_GRAPH$subject[DATA_GRAPH$subject=="math"] <- "Mathematics"
DATA_GRAPH$subject[DATA_GRAPH$subject=="port"] <- "Portuguese"

# Figure 06:

ggplot(data = DATA_GRAPH, aes(x=year, y= score, color = unit))+
  geom_vline(xintercept = 2008, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_line(size=0.9)+
  scale_color_manual(values= c("#01665e","#d8b365"), 
                     labels= c( "Ceará", "Synthetic Ceará"), 
                     name = "")+
  ylab("Score")+
  xlab("Year")+
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

ggsave(filename = "figure06.png", path = "plots",   width = 19, height = 15, , units = "cm")

DATA_GAP <- rbind(PM_GAP, PP_GAP, LSM_GAP, LSP_GAP)

DATA_GAP$grade[DATA_GAP$grade=="P"] <- "Primary Education"
DATA_GAP$grade[DATA_GAP$grade=="LS"] <- "Lower Secondary Education"
DATA_GAP$grade <- factor(DATA_GAP$grade, levels = c("Primary Education", "Lower Secondary Education"))

DATA_GAP$subject[DATA_GAP$subject=="math"] <- "Mathematics"
DATA_GAP$subject[DATA_GAP$subject=="port"] <- "Portuguese"

# Figure 07:

ggplot(data = DATA_GAP, aes(x=year, y=gap))+
  geom_vline(xintercept = 2008, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_hline(yintercept = 0, color = "lightgray", size = 0.5)+
  geom_line(size=0.9, color = "#01665e")+
  ylab("Effect")+
  xlab("Year")+
  annotate("text", x = 2013, y = 35, label = "Policy Change", color = "#636363", size = 4)+
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

ggsave(filename = "figure07.png", path = "plots", width = 19, height = 15, , units = "cm")

# Average Effects:

DATA_GAP %>% 
  filter(year>2008) %>% 
  group_by(subject, grade) %>%
  summarise(mean(gap))
  
  

##############################################
# SCM for upper secondary school             #
##############################################

# Run synth
SCM_USM    <- synth(DATA_USM)
TABLES_USM <- synth.tab(dataprep.res = DATA_USM, synth.res = SCM_USM)

SCM_USP    <- synth(DATA_USP)
TABLES_USP <- synth.tab(dataprep.res = DATA_USP, synth.res = SCM_USP)


# Graphs
USM <- plot_scm(UPPERS_M, TABLES_USM)
USM_SC <- USM[[1]]
USM_GAP <- USM[[2]]

USP <- plot_scm(UPPERS_P, TABLES_USP)
USP_SC <- USP[[1]]
USP_GAP <- USP[[2]]

GRAPH_US <- rbind(USM_SC, USP_SC)
GRAPH_US$subject[GRAPH_US$subject=="math"] <- "Mathematics"
GRAPH_US$subject[GRAPH_US$subject=="port"] <- "Portuguese"
GRAPH_US$grade <- "Upper Secondary Education"

# Figure 08

ggplot(data = GRAPH_US, aes(x=year, y= score, color = unit))+
  geom_vline(xintercept = 2011, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_line(size=0.9)+
  scale_color_manual(values= c("#01665e","#d8b365"), 
                     labels= c( "Ceará", "Synthetic Ceará"), 
                     name = "")+
  ylab("Score")+
  xlab("Year")+
  ylim(220,310)+
  annotate("text", x = 2015.5, y = 230, label = "Policy Change", color = "#636363", size = 4)+
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

ggsave(filename = "figure08.png", path = "plots", width = 19, height = 8, , units = "cm")

DATA_GAP_US <- rbind(USM_GAP,USP_GAP)

DATA_GAP_US$subject[DATA_GAP_US$subject=="math"] <- "Mathematics"
DATA_GAP_US$subject[DATA_GAP_US$subject=="port"] <- "Portuguese"
DATA_GAP_US$grade <- "Upper Secondary Education"

# Figure 09

ggplot(data = DATA_GAP_US, aes(x=year, y=gap))+
  geom_vline(xintercept = 2011, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_hline(yintercept = 0, color = "lightgray", size = 0.5)+
  geom_line(size=0.9, color = "#01665e")+
  ylab("Effect")+
  xlab("Year")+
  annotate("text", x = 2015.5, y = 35, label = "Policy Change", color = "#636363", size = 4)+
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

ggsave(filename = "figure09.png", path = "plots", width = 19, height = 8, units = "cm")



# Table with W vectors (Table 03)

W_PM <- as.data.frame(TABLES_PM$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(PM = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

W_PP <- as.data.frame(TABLES_PP$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(PP = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

W_LSM <- as.data.frame(TABLES_LSM$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(LSM = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

W_LSP <- as.data.frame(TABLES_LSP$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(LSP = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

W_USM <- as.data.frame(TABLES_USM$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(USM = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

W_USP <- as.data.frame(TABLES_USP$tab.w) %>% 
  filter(w.weights>0.01) %>% 
  rename(USP = w.weights, abbreviation = unit.names) %>% 
  dplyr::select(-unit.numbers)

TABLE_W <- left_join(abbr_state, W_PM, by="abbreviation")
TABLE_W <- left_join(TABLE_W, W_PP, by="abbreviation")
TABLE_W <- left_join(TABLE_W, W_LSM, by="abbreviation")
TABLE_W <- left_join(TABLE_W, W_LSP, by="abbreviation")
TABLE_W <- left_join(TABLE_W, W_USM, by="abbreviation")
TABLE_W <- left_join(TABLE_W, W_USP, by="abbreviation")

TABLE_W[is.na(TABLE_W)] <- 0.00
write.csv(TABLE_W, file="table03.csv")

# Table with V vectors (Table 04)

V_PM <- as.data.frame(TABLES_PM$tab.v) %>% 
  rename(PM = v.weights)
V_PP <- as.data.frame(TABLES_PP$tab.v) %>% 
  rename(PP = v.weights)
V_LSM <- as.data.frame(TABLES_LSM$tab.v) %>% 
  rename(LSM = v.weights)
V_LSP <- as.data.frame(TABLES_LSP$tab.v) %>% 
  rename(LSP = v.weights)
V_USM <- as.data.frame(TABLES_USM$tab.v) %>% 
  rename(USM = v.weights)
V_USP <- as.data.frame(TABLES_USP$tab.v) %>% 
  rename(USP = v.weights)
TABLE_V <- cbind(V_PM, V_PP, V_LSM, V_LSP, V_USM, V_USP)


print(xtable(TABLE_V, include.rownames=TRUE), type="html", file="table04.html")

# Table of predictions and comparison (Table 05)

PRED_PM <- as.data.frame(TABLES_PM$tab.pred) %>% 
  rename(Ceará_M = Treated, SyntheticPM = Synthetic, Mean_M = 'Sample Mean')

PRED_PP <- as.data.frame(TABLES_PP$tab.pred) %>% 
  rename(SyntheticPP = Synthetic) %>% 
  select(SyntheticPP)

PRED_LSM <- as.data.frame(TABLES_LSM$tab.pred) %>% 
  rename(SyntheticLSM = Synthetic) %>% 
  select(SyntheticLSM)

PRED_LSP <- as.data.frame(TABLES_LSP$tab.pred) %>% 
  rename(SyntheticLSP = Synthetic) %>% 
  select(SyntheticLSP)

PRED_USM <- as.data.frame(TABLES_USM$tab.pred) %>% 
  rename(SyntheticUSM = Synthetic) %>% 
  select(SyntheticUSM)

PRED_USP <- as.data.frame(TABLES_USP$tab.pred) %>% 
  rename(SyntheticUSP = Synthetic) %>% 
  select(SyntheticUSP)

PRED_PRI <- cbind(PRED_PM, PRED_PP, PRED_LSM, PRED_LSP, PRED_USM, PRED_USP)

print(xtable(PRED_PRI, include.rownames=TRUE), type="html", file="table05.html")

##########################
## in-time placebo test ##
##########################

# Figure 12 (atificial intervention in 2003)

# Preparing data with Synth:
DATA_PM_P <- prepare_time_placebo(PRIMARY_M)
DATA_PP_P <- prepare_time_placebo(PRIMARY_P)

DATA_LSM_P <- prepare_time_placebo(LOWERS_M)
DATA_LSP_P <- prepare_time_placebo(LOWERS_P)


# Run synth
SCM_PM_P    <- synth(DATA_PM_P)
TABLES_PM_P <- synth.tab(dataprep.res = DATA_PM_P, synth.res = SCM_PM_P)

SCM_PP_P   <- synth(DATA_PP_P)
TABLES_PP_P <- synth.tab(dataprep.res = DATA_PP_P, synth.res = SCM_PP_P)

SCM_LSM_P    <- synth(DATA_LSM_P)
TABLES_LSM_P <- synth.tab(dataprep.res = DATA_LSM_P, synth.res = SCM_LSM_P)

SCM_LSP_P    <- synth(DATA_LSP_P)
TABLES_LSP_P <- synth.tab(dataprep.res = DATA_LSP_P, synth.res = SCM_LSP_P)

# Graphs in ggplot
PM <- plot_scm(PRIMARY_M, TABLES_PM_P)
PM_SC <- PM[[1]]
PM_GAP <- PM[[2]]

PP <- plot_scm(PRIMARY_P, TABLES_PP_P)
PP_SC <- PP[[1]]
PP_GAP <- PP[[2]]

LSM <- plot_scm(LOWERS_M, TABLES_LSM_P)
LSM_SC <- LSM[[1]]
LSM_GAP <- LSM[[2]]

LSP <- plot_scm(LOWERS_P, TABLES_LSP_P)
LSP_SC <- LSP[[1]]
LSP_GAP <- LSP[[2]]

DATA_GRAPH <- rbind(PM_SC, PP_SC, LSM_SC, LSP_SC)

DATA_GRAPH$grade[DATA_GRAPH$grade=="P"] <- "Primary Education"
DATA_GRAPH$grade[DATA_GRAPH$grade=="LS"] <- "Lower Secondary Education"
DATA_GRAPH$grade <- factor(DATA_GRAPH$grade, levels = c("Primary Education", "Lower Secondary Education"))

DATA_GRAPH$subject[DATA_GRAPH$subject=="math"] <- "Mathematics"
DATA_GRAPH$subject[DATA_GRAPH$subject=="port"] <- "Portuguese"

ggplot(data = DATA_GRAPH, aes(x=year, y= score, color = unit))+
  geom_vline(xintercept = 2003, color = "#636363", linetype = "dashed", size = 0.9)+
  geom_line(size=0.9)+
  scale_color_manual(values= c("#01665e","#d8b365"), 
                     labels= c( "Ceará", "Synthetic Ceará"), 
                     name = "")+
  ylab("Score")+
  xlab("Year")+
  annotate("text", x = 2009, y = 260, label = "Placebo Policy Change", color = "#636363", size = 3)+
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

ggsave(filename = "figure10.png", path = "plots", width = 19, height = 15, , units = "cm")



