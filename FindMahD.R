#------------------------------------------------------------------------
#Function effect size using Hotellings T2 test
# Calculating Effect size using Hotellings T2 test
#	- Effect sizes for
#     -Sample population
#     -Group comparisons
#----------------------------------------------------

#Requires DescTools package
library(DescTools)

#Function multiplies T2 by the inverse of the weighting to get 
#Mahalanobis distance for T2 test or paired T2 tests

MahEff <- function(data = NULL, g1 = NULL, g2 = NULL){

	if(is.null(g1) == TRUE & is.null(g2) == TRUE){
	q <- ncol(data); n <- nrow(data)
	T2 <- HotellingsT2Test(data, test = "f")$statistic
  EffectSize <- ((q*(n-1))/(n*(n-q)))*T2
	return(EffectSize)
	}
	else{ 
	q <- ncol(g1)
	n <- nrow(g1)+nrow(g2) - 1
	T2 <- HotellingsT2Test(x = g1, y = g2 , test = "f")$statistic
	EffectSize <-((q*(n-1))/(n-q))* T2
	}
	return(EffectSize)
}
 
  
  
