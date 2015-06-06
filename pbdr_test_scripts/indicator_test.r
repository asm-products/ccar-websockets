#Script to test the normality of data

library(rjson)

indicatorURL <- "http://www.smart-earn.com/gj/5/16"
indicatorData <- readLines(indicatorURL)
v <- fromJSON(indicatorData)
d <- sapply(v, function(y) y[[2]])
shapiro.test(as.numeric(as.character(d[-1])))