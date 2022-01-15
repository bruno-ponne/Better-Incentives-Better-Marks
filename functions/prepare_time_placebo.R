#-----------------------------------------------------------------------------------------#
# Function: prepare_time_placebo()                                                        #
# Description: prepares data for the synth function - placebo in-time test                #
#-----------------------------------------------------------------------------------------#

prepare_time_placebo <- function(data){
  library(Synth)
  
  predictors <- c("homicides", "TWh", "ln_pop", "unemployment", "edu_invest_pc")
  
  DATA_PM <- dataprep(foo = data,
                      predictors = predictors,
                      dependent     = "score",
                      unit.variable = "code_state",
                      time.variable = "year",
                      unit.names.variable = "abbr_state",
                      treatment.identifier  = 23,
                      controls.identifier   = c(11:17, 21:22, 24:29, 31:33, 35, 41:43, 50:53),
                      time.predictors.prior = seq(1995, 2003, 2),
                      time.optimize.ssr     = seq(1995, 2003, 2),
                      time.plot             = seq(1995, 2019, 2))
 
  return(DATA_PM)
}