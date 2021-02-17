##########################################
### Function to compute maternal diet 
### index given input data
###########################################

#### Function use requirements ####
# - Provide the required 8 arguments:
#   `1-7. intake frequencies of (as numeric object or numeric vector or numeric data frame column):
#     ~ vegetables (not incl. salad, potatoes, or beans), 
#     ~ yogurt (not incl. frozen yogurt), 
#     ~ fried potatoes (french fries, tater tots, etc.),
#     ~ rice/cooked grains (not incl. pasta or bread), 
#     ~ 100% pure fruit juice (not incl. aritificial fruit drinks like Hi-C, lemonade, etc.), 
#     ~ red meats, 
#     ~ cold cereals (not incl. oatmeal or cooked cereals)
#    8.The units for frequency of intake variables/objects must be the same for all of the 7 index component variables
#      and must be in units of either: 
#       ~ number of times consumed per day
#       ~ number of times consumed per month
#       ~ number of times consumed per year
#      Argument to the Units input should be given in quotations as either: "Days", "Weeks", or "Months" (case-sensitive)

##############################################################################################################

ComputeDietIndex <- function(VegLab, YogLab, FriesLab, RiceLab, JuiceLab, RedMeatLab, CerealLab, Units) {
  # Create temporary data.frame object with all input variables (except Units) as columns
  temp <- data.frame(VegLab, YogLab, FriesLab, RiceLab, JuiceLab, RedMeatLab, CerealLab)
  
  # Convert units to number of times per day if they are not already
  if (Units=="Days") {
    temp <- temp
  } else if (Units=="Weeks") {
    temp <- data.frame(apply(temp, 2, function(x) x*(52/365)))
  } else if (Units=="Months"){
    temp <- data.frame(apply(temp, 2, function(x) x*(12/365)))
  } else {
    stop("Invalid input for argument Units")
  }
  
  # Cap maximum daily consumption at 13 times per day
  temp$VegLab <- pmin(temp$VegLab, 13)
  temp$YogLab <- pmin(temp$YogLab, 13)
  temp$FriesLab <- pmin(temp$FriesLab, 13)
  temp$RiceLab <- pmin(temp$RiceLab, 13)
  temp$JuiceLab <- pmin(temp$JuiceLab, 13)
  temp$RedMeatLab <- pmin(temp$RedMeatLab, 13)
  temp$CerealLab <- pmin(temp$CerealLab, 13)
  
  # Compute raw index score
  RawScores <- 33.0108 + (-0.4106*temp$VegLab) + (-0.5590*temp$YogLab) + (-0.8566*(13-temp$FriesLab)) + (-0.5752*(13-temp$RiceLab)) + (-0.1870*(13-temp$JuiceLab)) + (-0.5793*(13-temp$RedMeatLab)) + (-0.4406*(13-temp$CerealLab))
  
  # Compute min raw index score
  MinRaw <- 33.0108 + (-0.4106*13) + (-0.5590*13) + (-0.8566*(13-0)) + (-0.5752*(13-0)) + (-0.1870*(13-0)) + (-0.5793*(13-0)) + (-0.4406*(13-0))
  
  # Compute max raw index score
  MaxRaw <- 33.0108 + (-0.4106*0) + (-0.5590*0) + (-0.8566*(13-13)) + (-0.5752*(13-13)) + (-0.1870*(13-13)) + (-0.5793*(13-13)) + (-0.4406*(13-13))
  
  # Commpute scaled index score
  IndexScore <- (100*(RawScores - MaxRaw))/(MinRaw - MaxRaw)
  
  # Return the index score
  return(IndexScore)
  
}
