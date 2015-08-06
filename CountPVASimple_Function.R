## 	CountPVA Simple function


# all variables with <<- assignment will be avaliable outside of the function
# 	Extract variable:
# Each lambda for the duration of the study as "Sitemu"	
# r squared	as "R.sq"
# mu with lower and upper 95% CI as "mu.s" with three columns of "mu.s$fit", "mu.s$lwr", and "mu.s$upr" 
# the set of year intervals of the start year of study and each consecutive addition of years as "years"
#		"years$RangeStart" and "years$RangeEnd"
# Lambda and the lower and upper 95% CIs as "growth.exp" with "growth.exp$fit", "growth.exp$lwr", and "growth.exp$upr"  

#	To use the function, enter "CountPVA(x,y)
#	x The column with the years of the study census
#	y The column with the annual count (years can be missed, the counts are for the matching years in the x year column) 


# To create the function in your R Console, run the following block of code:
#############################################################################################
############################### Count PVA Function ##########################################
##################################### Start #################################################
#############################################################################################


CountPVAsimple <- function(x,y){

	Yrs <- length(unique(x))
	rm(ySpecies); rm(mu_sp); rm(PVA.lm); rm(year.int); rm(growth); rm(rsq)
	rsq <- c()
	xvar <- c()
	ySpecies <- c()
	PVA.lm <- list()	
	mu_sp <- c()
	year.int <- c()

		count1 <- x
		yrs <- length(count1)
			for(i in 1:(yrs-1)){
				yr <- sqrt((x[i+1]) - (x[i]))
				xvar <- rbind(xvar, yr)
				y1 <- ( log10(y[i+1]/y[i]) )/yr
				ySpecies <- rbind(ySpecies, y1)
				PVA.table <<- data.frame(cbind(xvar = as.numeric(xvar),
					ySpecies = as.numeric(ySpecies)))
				year.int1 <- data.frame(cbind(RangeStart = min(x), 
					RangeEnd = x[Yrs]))
				PVA.lm <- lm(PVA.table[,2] ~ -1 + PVA.table[,1])

				mu_sp1 <- predict(PVA.lm, level= 0.95, 
					interval = "confidence",
					se.fit = T)
			}
	rsq <- data.frame(rbind(rsq, summary(PVA.lm)$adj.r.squared))	# pull R squared values from
												# each year interval 1995-1999...
	Sitemu <<- ySpecies
		rm(ySpecies); rm(xvar); rm(PVA.table)
		ySpecies <- c(); xvar <- c(); PVA.table <- list()
	
		mu_sp <- data.frame(rbind(mu_sp, mu_sp1$fit[1,]))
		year.int <- data.frame(rbind(year.int, year.int1))
	

	R.sq <<- rsq
	colnames(R.sq) <<- "R.squared"
	mu.s <<- mu_sp
	colnames(mu.s) <<- c("fit", "lwr", "upr")
	growth <<- mu_sp
	years <<- year.int
	growth.exp <<- exp(growth)
	lambda <<- data.frame(cbind((min(years[,2])-3):(max(years[,2])-1),Sitemu))
	
}

save(CountPVAsimple, file = "CountPVASimpleFunction.R")

#############################################################################################
############################### Count PVA Function ##########################################
####################################### End #################################################
#############################################################################################







###############################EXAMPLE##########################################

# to see how the function works, test with this example:

#Q:\Research\All_Projects_by_Species\Astragalus_microcymbus\R_Analysis\R_tables\
# PlotSummary_2012


countAsMi <- read.csv(file.choose(""), header = TRUE, as.is = TRUE)

# Count based on individuals that were above ground = "Total.Alive", excluding dormant
NumperYr <- aggregate(countAsMi$Total.Alive, 
	list(Site = countAsMi$Site, Year = countAsMi$Year),
	sum)	

# Format the data so each site is a column and number of individauls per year (row)
num <- reshape(NumperYr, idvar = "Year", timevar = "Site", direction = "wide")

num	# an example of a table with a year column and count columns
x <- num$Year
y <- num$x.5

## Testing example
CountPVA(num[,1],num[,4])

par(mar = c(5,4,1,1))
plot(years[,2], growth.exp[,1], xlab = "Length of study (years)",
	ylim = c(min(growth.exp[,2]), max(growth.exp[,3])),
	ylab = "Growth rate",
	xaxt = "n",
	main = "")
 lines(years[,2], growth.exp[,2], col="red")
 lines(years[,2], growth.exp[,3], col="red")
abline(h=1, col = "grey")
axis(1, at = min(years[,2]):max(years[,2]), labels = 4:(length(num[,1])))


growth

mu.s

R.sq

growth.exp 










