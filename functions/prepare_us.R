#-----------------------------------------------------------------------------------------#
# Function: prepare_us()                                                                  #
# Description: prepares data for the synth function - upper secondary school              #
#-----------------------------------------------------------------------------------------#

prepare_us <- function(data){
  library(Synth)
  
  predictors <- c("homicides", "TWh", "unemployment", "ln_pop", "edu_invest_pc")
  
  DATA_PM <- dataprep(foo = data,
                      predictors = predictors,
                      dependent     = "score",
                      unit.variable = "code_state",
                      time.variable = "year",
                      unit.names.variable = "abbr_state",
                      treatment.identifier  = 23,
                      controls.identifier   = c(11:17, 21:22, 24:26, 28:29, 31:33, 35, 41:43, 50:53),
                      time.predictors.prior = seq(1995, 2009, 2),
                      time.optimize.ssr     = seq(1995, 2009, 2),
                      time.plot             = seq(1995, 2019, 2))
 
  return(DATA_PM)
}