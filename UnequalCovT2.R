#--------------------------------------------------------
#EDP 646A
#James Smith
#University of Arizona
#---------------------------------
#September 27, 2018
#	- Hotellings T2 Test for Groups with Unequal Cov
#   -T2
#   -p-value
#   -Effect size (Mahalanobis Distance)
#----------------------------------------------------

#Required packages
library(psych);library(DescTools)


#Function

UnequalHT2 <- function(g1, g2){
	n <- nrow(g1)+nrow(g2)-1; q <- ncol(g1)
	
	#Mean vectors for each group
	mg1 <- describe(g1)$mean; mg2 <- describe(g2)$mean

	#Covariance matrices
	Sg1 <- cov(g1); Sg2 <- cov(g2)

	# Sum of covariance matrices in slides:
	sumcov <- ((Sg1/nrow(g1)) + (Sg2/nrow(g2)))
	
	part2 <- t(mg1-mg2) %*% solve(sumcov) %*% (mg1 - mg2)
	part1 <- ((n-q)/(q*(n-1)))
	
	T2 <- part1*part2

	#Calculate p-value
	df1 = q

	m1 <- (1/(nrow(g1)-1))*((t(mg1 - mg2) %*% solve(sumcov)
		%*% (Sg1/nrow(g1)) %*% solve(sumcov) %*% (mg1 - mg2))/T2)^2

	m2 <- (1/(nrow(g2)-1))*((t(mg1 - mg2) %*% solve(sumcov) 
		%*% (Sg2/nrow(g2)) %*% solve(sumcov) %*% (mg1 - mg2))/T2)^2

	m.inv <- m1+m2; m <- 1/m.inv; 

	p <- pf(q=T2, lower.tail=FALSE, df1=q, df2=m)
	
	#Rounding values
	p <- round(p,3)
	T2 <- round(T2, 3)
	part2 <- round(part2,3)
	
	#Print results
	print(paste("T2 =", T2,  "  |  p =", p ,"  |  Effect Size =",part2))
}
