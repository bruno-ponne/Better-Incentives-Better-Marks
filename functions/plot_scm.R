#-----------------------------------------------------------------------------------------#
# Function: plot_scm()                                                                    #
# Description: prepares data from the synthetic control output to be plotted with ggplot  #
#-----------------------------------------------------------------------------------------#



plot_scm <- function(original_data, synth.tables){
  library(tidyverse)
  W <- as.data.frame(synth.tables[["tab.w"]])
  W <- W %>% 
    filter(w.weights>0.01) %>% 
    mutate(w.weights = round(w.weights, digits = 3)) %>% 
    rename(abbr_state = unit.names)
  
  SC <- left_join(original_data, select(W, -unit.numbers), by = "abbr_state") %>% 
    na.omit() %>% 
    group_by(year) %>% 
    summarise(sc = weighted.mean(score, w.weights))
  
  CE <- original_data %>% 
    filter(abbr_state == "CE") %>% 
    select(year, score)
  
  GAP <- left_join(CE, SC, by = "year") %>% 
    mutate(gap = score-sc)
  GAP$grade <- unique(original_data$grade)
  GAP$subject <- unique(original_data$subject)
  
  GG_DATA <- left_join(CE, SC, by = "year") %>% 
    pivot_longer(!year, names_to = "unit", values_to = "score")
  
  GG_DATA$unit[GG_DATA$unit == "score"] <- "Ceará"
  GG_DATA$unit[GG_DATA$unit == "sc"] <- "Synthetic Control"
  GG_DATA$grade <- unique(original_data$grade)
  GG_DATA$subject <- unique(original_data$subject)
  
  return(list(GG_DATA, GAP))
}