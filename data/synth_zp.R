# library(tidyverse)
# library(synthpop)
# 
# zp <- read_csv("zp_read_all.csv")[,-c(1:2)]
# names(zp)
# 
# synzp <- syn(zp, minnumlevels = 4, seed = nchar("we all play synth"))
# 
# # compare the synthetic and original data frames
# compare(synzp, zp, nrow = 3, ncol = 4)
# 
# write_csv(synzp$syn, file="zp_synth.csv")
