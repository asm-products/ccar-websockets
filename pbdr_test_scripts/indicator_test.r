#Script to test the normality of data


# Given a url compute if the data
# in the format time, value 
# passes normality test
library(rjson)
library ( pbdMPI , quietly = TRUE )

computeShapiroTest<- function(indicatorURL, index) {
	indicatorData <- readLines(indicatorURL)
	v <- fromJSON(indicatorData)
	if(length(v) > 1) {
		d <- sapply(v, function(y) y[[2]])
		return(c("index" = index, "profile"=shapiro.test(as.numeric((d[-1])))))
		#return(shapiro.test(d))
	}else {
		return((sprintf("Ignoring %s",  indicatorURL)))
	}
}

init ()

indicators <- c(1,5,6, 7,8, 9)
indicators <- append(indicators, c(10:173))

#indicators <- c(1)
baseURL <- "http://www.smart-earn.com/gj/5/"
indicatorURLs <- sapply(indicators, function(y) {
		url <- sprintf("%s%d", baseURL, y)
		index <- sprintf("%d", y)
		computeShapiroTest(url, index)
	}
	)
cat(toJSON(indicatorURLs))
finalize()