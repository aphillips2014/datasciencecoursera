#################################
# corrTest file
#
#################################
setwd("c:/specdata")
source("corr.R")
cr <- corr("specdata", 150)
head(cr)

summary(cr)
