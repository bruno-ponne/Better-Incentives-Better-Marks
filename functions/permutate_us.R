#-----------------------------------------------------------------------------------------#
# Function: permutate_us()                                                                #
# Description: executes permutation test for upper secondary school                       #
#-----------------------------------------------------------------------------------------#

permutate_us <- function(data){
  
  library(Synth)
  library(tidyverse)

  states <- c(11:17, 21, 23, 28:29, 31:33, 35, 41:43, 50:53)
  predictors <- c("homicides", "TWh", "unemployment", "ln_pop", "edu_invest_pc")
  
  results <- list()
  results_synth <- list()
  gaps <- list()
  
  for (i in states) {
    dataprep.out <- dataprep(foo = data,
                             predictors = predictors,
                             dependent     = "score",
                             unit.variable = "code_state",
                             time.variable = "year",
                             unit.names.variable = "abbr_state",
                             treatment.identifier  = i,
                             controls.identifier   = states[which(states!=i)],
                             time.predictors.prior = seq(1995, 2009, 2),
                             time.optimize.ssr     = seq(1995, 2009, 2),
                             time.plot             = seq(1995, 2019, 2))
    
    results[[as.character(i)]] <- dataprep.out
    results_synth[[as.character(i)]] <- synth(results[[as.character(i)]])
    gaps[[as.character(i)]] <- 
    results[[as.character(i)]]$Y1plot - (results[[as.character(i)]]$Y0plot %*% results_synth[[as.character(i)]]$solution.w)
  }
  
  GAP <- as.data.frame(gaps)
  GAP$year <- as.numeric(row.names(GAP))
  GAP <- GAP %>%  pivot_longer(!year,  names_to = "code_state", values_to = "gap")
  
  return(GAP)
}

