# initial market reserach
PayS <- 100
PayF <- -70
PrS <- 0.7
PrF <- 0.3

ExValue <- PayS * PrS + PayF * PrF

# purchase insurance at PrF * PayF. This is the VALUE OF PERFECT INFORMATION
# does my expected value go higher if i do a test group?

# Pilot test, T
PrTpos_S <- 0.8
PrTneg_S <- 0.2
PrTpos_F <- 0.3
PrTneg_F <- 0.7

# calculate PrTpos
PrTpos <- PrTpos_S*PrS + PrTpos_F*PrF
PrTneg <- 1-PrTpos

# update ExValue with more information