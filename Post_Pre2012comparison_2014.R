######## Choose: 2012on_scgl_130913.csv  ###########

#sg1213 <- read.csv(file.choose(),
#	header = TRUE, as.is=TRUE)


library(sciplot)
library(plyr)
library(ggplot2)


setwd(path.expand("Q:/Research/All_Projects_by_Species/Sclerocactus SPECIES/Sclerocactus_glaucus/R_code_ScGl/R_tables/"))

sg <- read.csv("2014_scgl_rawdata.csv", header = T, as.is = T)
head(sg)

# Check on Pond, something seems off
head(sg)
unique(sg$Site)
Pond.check <- subset(sg, Site == "Pond" | Site == "Pond (old)")
table(Pond.check$Transect, Pond.check$Year)

expand.grid(unique(Pond.check$Site),unique(Pond.check$Transect))

pond.fix <- read.csv("FixPondCheck.csv")
names(pond.fix)
table(pond.fix$Site)

table(pond.fix$Transect, pond.fix$Year)

# for only living individuals

Pond.check.alive <- subset(Pond.check, Height.cm. > 0)

tapply(Pond.check.alive$Tag, 
       list(Pond.check.alive$Transect, Pond.check.alive$Site, Pond.check.alive$Year),
       FUN = function(x) length(x))

pond.summary <- tapply(Pond.check.alive$Tag, 
       list(Pond.check.alive$Transect, Pond.check.alive$Year),
       FUN = function(x) length(x))

library(reshape2)
pond.sum.melt <- melt(pond.summary)
names(pond.sum.melt)

ggplot(pond.sum.melt, aes(Var2,value,colour = as.character(Var1))) +
  geom_line()

#Chose All2008
ScGl08<-read.delim("All2008.txt", as.is=FALSE, sep=",",
	na.strings="NA", header=TRUE)
ScGl08_2 <- cbind(ScGl08[,c(1:3)], AverageHeight = NA, AverageWidth = NA,
			ScGl08[,c(4:8)],
			Minis = ScGl08[,9], ScGl08[,c(10:12)],
			AprRain = ScGl08[,c(13)], ScGl08[,c(14:21)],
			PrYrRain = ScGl08[,c(22)], ScGl08[,c(23:34)])
head(ScGl08_2)
ct.2008 <- ddply(ScGl08_2, .(SiteName, Year), summarise,
	X = sum(CountofHeight),
	Rep = sum(SumFlowered),
	Brs = sum(SumBrowsed),
	avgH = NA,
	sdH = NA,
	avgW = NA,
	sdW = NA,
	avgvol = NA,
	sdvol = NA)

#sg$Year[sg$Site == "Pond (old)"];sg$Year[sg$Site == "Oil Pad (old)"];sg$Year[sg$Site == "Road T-West (old)"]
#unique(sg$Site[sg$Year == 2014]);unique(sg$Site[sg$Year == 2013])

# Change Fl and Br to 1s and 0s, add a volume column
sg$Fl2 <- 0
sg$Fl2[sg$Fl == "y"] <- 1
sg$Br2 <- 0
sg$Br2[sg$Br == "y"] <- 1
sg$vol <- pi*(sg$Width.cm.^2)*sg$Height.cm.

# From raw data
# Calculate average and SD for the summary table
sg$Site[sg$Site == "Atwell Gulch_108" | sg$Site == "Atwell Gulch_165"] <- "Atwell Gulch"
ct.raw <- ddply(subset(sg, Height.cm. > 0 & Site != "Atwell Gulch"), .(Site, Year), summarise,
	X = length(Height.cm.),
	Rep = sum(Fl2),
	Brs = sum(Br2),
	avgH = mean(Height.cm., na.rm = T),
	sdH = sd(Height.cm., na.rm = T),
	avgW = mean(Width.cm., na.rm = T),
	sdW = sd(Width.cm., na.rm = T),
	avgvol = mean(vol, na.rm = T),
	sdvol = sd(vol, na.rm = T))

## Old Atwell is everything less than 30m for Y.coord.m.
ct.atwellold <- ddply(subset(sg, Y.coord.m. < 30 & Site == "Atwell Gulch"),
	.(Site, Year), summarise,
	X = length(Height.cm.),
	Rep = sum(Fl2),
	Brs = sum(Br2),
	avgH = mean(Height.cm., na.rm = T),
	sdH = sd(Height.cm., na.rm = T),
	avgW = mean(Width.cm., na.rm = T),
	sdW = sd(Width.cm., na.rm = T),
	avgvol = mean(vol, na.rm = T),
	sdvol = sd(vol, na.rm = T))
ct.atwellold$Site <- "Atwell Gulch (old)"

ct.atwellnew <- ddply(subset(sg, Year > 2011 & Site == "Atwell Gulch"), .(Site, Year), summarise,
	X = length(Height.cm.),
	Rep = sum(Fl2),
	Brs = sum(Br2),
	avgH = mean(Height.cm., na.rm = T),
	sdH = sd(Height.cm., na.rm = T),
	avgW = mean(Width.cm., na.rm = T),
	sdW = sd(Width.cm., na.rm = T),
	avgvol = mean(vol, na.rm = T),
	sdvol = sd(vol, na.rm = T))
colnames(ct.2008)[1] <- "Site"
ct.all <- rbind(ct.2008, ct.raw, ct.atwellold, ct.atwellnew)
subset(ct.all, Year == 2014)

# are there duplicate Pond records?
dups <- data.frame(table(sg$Tag[sg$Site == "Pond" & sg$Year == 2014],
		sg$Transect[sg$Site == "Pond" & sg$Year == 2014]))
dups$Freq[dups$Freq > 1]	# No duplicates

ggplot(ct.all, aes(Year, X, colour = Site)) +
	geom_line() +
	geom_point() +
	ylab("Total Individuals") +
	theme_bw() +
	theme(plot.background = element_blank(),
		panel.grid.major = element_blank()) +
	geom_text(data = ct.all[ct.all$Year == 2014,],
			label = ct.all$Site[ct.all$Year == 2014])


# groupings
ct.all$Genetic <- "South"
ct.all$Genetic[ct.all$Site == "Atwell Gulch" |
			ct.all$Site == "Atwell Gulch (old)" |
			ct.all$Site == "Oil Pad" |
			ct.all$Site == "Oil Pad (old)" |
			ct.all$Site == "Pond" |
			ct.all$Site == "Pond (old)" |
			ct.all$Site == "Road T-West (old)" |
			ct.all$Site == "Road T-East (old)" |
			ct.all$Site == "T-Junction" |
			ct.all$Site == "Pyramid Rock"] <- "North"

table(ct.all$Site, ct.all$Year)

# Same and changing sites
ct.all$SiteSame <- ct.all$Site
ct.all$SiteSame[ct.all$Site == "Atwell Gulch" |ct.all$Site == "Atwell Gulch (old)"] <- "Atwell Gulch"
ct.all$SiteSame[ct.all$Site == "Pond" |ct.all$Site == "Pond (old)"] <- "Pond"
ct.all$SiteSame[ct.all$Site == "Road T-West (old)" |ct.all$Site == "T-Junction"] <- "T-Junction"
ct.all$SiteSame[ct.all$Site == "Oil Pad (old)" |ct.all$Site == "Oil Pad"] <- "Oil Pad"


ggplot(subset(ct.all, Site != "Road T-East (old)"),
		aes(Year, X, label = Site, colour = Site )) +
	geom_line() +
	ylab("Total Individuals") +
	theme_bw() +
	theme(plot.background = element_blank(),
		panel.grid.major = element_blank()) +
	facet_grid(~Genetic) +
	geom_text(size = 2)



_______________________________________________________________________________________________

#Figure 3b
# total individuals per site
## 2015 note: should pond new be smaller, fewer individuals???

ggplot(subset(ct.all, Site != "Road T-East (old)"),
		aes(Year, X, colour = Site, label = Site)) +
	geom_line() +
	geom_point() +
	ylab(expression(paste("Total ", italic("Sclerocactus glaucus"), " individuals"))) +
	theme_bw() +
	theme(plot.background = element_blank(),
		panel.grid.major = element_blank()) +
	facet_wrap(~SiteSame, ncol = 5) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) #

# Why does Pond seem wrong?
table(ct.all$Site, ct.all$SiteSame)

pond <- subset(ct.all, Site == "Pond" | Site == "Pond (old)")
head(pond)

## Run the CountPVASimple_Funcation from
#	"Q:\Research\Stats & Software\R CODE\Functions\CountPVASimple_Function.r"


CountPVAsimple

##	Pre-2012 versions
## Need to combine the growth rates for Atwell, Pond, Oil Pad, and T-Junction
sts <- unique(ct.all$Site)

gr <- data.frame()
for(i in sts){
	CountPVAsimple(ct.all$Year[ct.all$Site == i],
			ct.all$X[ct.all$Site == i])
gr <- rbind(gr,
data.frame(Site =
	predict(lm(PVA.table[,2] ~ -1 + PVA.table[,1]), level = 0.95,
	interval = "confidence",
	se.fit = T)$fit[1,],
	i))
}

gr2 <- data.frame(gr, Var = substr(rownames(gr), 1,3))
gr3 <- ddply(gr2, .(i), summarize,
	fit = exp(Site[Var == 'fit']),
	lwr = exp(Site[Var == "lwr"]),
	upr = exp(Site[Var == "upr"]))

## Delta area annual growth rates
# Bridgeport

CountPVAsimple(ct.all$Year[ct.all$Site == sts[5]],	# x is the column for years
               ct.all$X[ct.all$Site == sts[5]])		# y is the column for counts

exp(PVA.table)

# Escalante
CountPVAsimple(ct.all$Year[ct.all$Site == sts[1]],  # x is the column for years
               ct.all$X[ct.all$Site == sts[1]])		# y is the column for counts

exp(PVA.table)

# Picnic
CountPVAsimple(ct.all$Year[ct.all$Site == sts[2]],  # x is the column for years
               ct.all$X[ct.all$Site == sts[2]])  	# y is the column for counts

exp(PVA.table)

# Powerline
CountPVAsimple(ct.all$Year[ct.all$Site == sts[3]],  # x is the column for years
               ct.all$X[ct.all$Site == sts[3]])    # y is the column for counts

exp(PVA.table)


#Atwell
	CountPVAsimple(ct.all$Year[ct.all$Site == sts[15]],	# x is the column for years
			   ct.all$X[ct.all$Site == sts[15]])		# y is the column for counts

At <- PVA.table
#Atwell (old)
	CountPVAsimple(ct.all$Year[ct.all$Site == sts[14]],	# x is the column for years
			   ct.all$X[ct.all$Site == sts[14]])		# y is the column for counts
Atold <- PVA.table

#Oil Pad
	CountPVAsimple(ct.all$Year[ct.all$Site == sts[7]],	# x is the column for years
			   ct.all$X[ct.all$Site == sts[7]])		# y is the column for counts
Op <- PVA.table
#Oil Pad (old)
	CountPVAsimple(ct.all$Year[ct.all$Site == sts[8]],	# x is the column for years
			   ct.all$X[ct.all$Site == sts[8]])		# y is the column for counts
Opold <- PVA.table


#Pond
	CountPVAsimple(ct.all$Year[ct.all$Site == sts[9]],	# x is the column for years
			   ct.all$X[ct.all$Site == sts[9]])		# y is the column for counts
p <- PVA.table
#Pond (old)
	CountPVAsimple(ct.all$Year[ct.all$Site == sts[10]],	# x is the column for years
			   ct.all$X[ct.all$Site == sts[10]])		# y is the column for counts
pold <- PVA.table


#T-Junction
	CountPVAsimple(ct.all$Year[ct.all$Site == sts[13]],	# x is the column for years
			   ct.all$X[ct.all$Site == sts[13]])		# y is the column for counts
rt <- PVA.table
#Road T-West (old)
	CountPVAsimple(ct.all$Year[ct.all$Site == sts[12]],	# x is the column for years
			   ct.all$X[ct.all$Site == sts[12]])		# y is the column for counts
rtold <- PVA.table


# After combine the tables from ct and ct1213 then run the linear model
##  lm(PVA.table[,2] ~ -1 + PVA.table[,1])

#Atwell
atwell <- rbind(At, Atold)

growthrates <-
(data.frame(Atwell =
	predict(lm(atwell[,2] ~ -1 + atwell[,1]), level = 0.95,
	interval = "confidence",
	se.fit = T)$fit[1,]))

#Oil Pad
oilpad <- rbind(Op, Opold)

growthrates <- cbind(growthrates,
(data.frame(OilPad =
	predict(lm(oilpad [,2] ~ -1 + oilpad [,1]), level = 0.95,
	interval = "confidence",
	se.fit = T)$fit[1,])))

#Pond
pond <- rbind(p, pold)

growthrates <- cbind(growthrates,
(data.frame(Pond =
	predict(lm(pond [,2] ~ -1 + pond [,1]), level = 0.95,
	interval = "confidence",
	se.fit = T)$fit[1,])))

#Road T-West
road <- rbind(rt, rtold)

growthrates <- cbind(growthrates,
(data.frame(RoadTWest =
	predict(lm(road [,2] ~ -1 + road [,1]), level = 0.95,
	interval = "confidence",
	se.fit = T)$fit[1,])))



t(growthrates)
grat<-data.frame(Site = row.names(t(growthrates)), exp(t(growthrates)))


ggplot(grat, aes(Site)) +
	geom_point(aes(y = fit, color = "fit", shape = "fit")) +
	geom_errorbar(aes(ymin = lwr, ymax = upr)) +
	ylab("Growth Rate") +
	xlab("Site") +
	geom_hline(aes(yintercept=1))


ggplot(gr3, aes(i)) +
	geom_point(aes(y = fit, color = i)) +
	geom_errorbar(aes(ymin = lwr, ymax = upr)) +
	ylab("Growth Rate") +
	xlab("Site") +
	geom_hline(aes(yintercept=0))

grat
colnames(gr3)[1] <- "Site"
gr4 <- rbind(grat, subset(gr3, Site != "Atwell Gulch" &
				Site != "Atwell Gulch (old)" &
				Site != "Pond" &
				Site != "Pond (old)" &
				Site != "T-Junction" &
				Site != "Road T-West (old)" &
				Site != "Oil Pad" &
				Site != "Oil Pad (old)" &
				Site != "Road T-East (old)"))

levels(gr4$Site) <- c(levels(gr4$Site)[as.numeric(gr4$Site)], "T-Junction", "Atwell Gulch", "Oil Pad")
gr4$Site[gr4$Site == "RoadTWest"] <- "T-Junction"
gr4$Site[gr4$Site == "Atwell"] <- "Atwell Gulch"

## Factor by north and south
gr5 <- transform(gr4, Site = factor(Site,
		levels = c('Bridgeport', 'Fram', 'Escalante Canyon', 'Picnic Site', 'Powerline',
				'Atwell Gulch', 'Pyramid Rock', 'Pond','T-Junction','OilPad')))

ggplot(gr5, aes(Site)) +
	geom_point(aes(y = fit, color = i)) +
	geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25) +
	ylab("Growth Rate") +
	xlab("Site") +
	geom_hline(aes(yintercept=1), colour = 'grey') +
	geom_vline(aes(xintercept=5.5), linetype = 4, colour = 'grey') +
	theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0),
		legend.position='none',
		panel.background = element_blank()) +
	geom_text(aes(3,0.8, label = "South", size = 2, color = 'grey')) +
	geom_text(aes(8,0.8, label = "North", size = 2, color = 'grey'))


# Demography: on average, lambdas
colMeans(gr5[,2:4], na.rm = T)

___________________________________________________________________

_______________________________________________________________________________________________

#Figure 3a
# individuals per square meter
# total area from table 2

unique(ct.all$Site)
ct.all$area[ct.all$Site == "Escalante Canyon"] <- 180
ct.all$area[ct.all$Site == "Bridgeport"] <- 400
ct.all$area[ct.all$Site == "Atwell Gulch"] <- 390
ct.all$area[ct.all$Site == "Atwell Gulch (old)"] <- 300
ct.all$area[ct.all$Site == "Fram"] <- 1300
ct.all$area[ct.all$Site == "Oil Pad (old)"] <- 672
ct.all$area[ct.all$Site == "Oil Pad"] <- 120
ct.all$area[ct.all$Site == "Picnic Site"] <- 160
ct.all$area[ct.all$Site == "Pond"] <- 36
ct.all$area[ct.all$Site == "Pond (old)"] <- 235.6
ct.all$area[ct.all$Site == "Powerline"] <- 40
ct.all$area[ct.all$Site == "Pyramid Rock"] <- 100
ct.all$area[ct.all$Site == "Road T-West (old)"] <- 2212
ct.all$area[ct.all$Site == "T-Junction"] <- 130

require(scales)
ggplot(subset(ct.all, Site != 'Road T-East (old)'),
		aes(Year, X/area, colour = Site)) +
	geom_line() +
	geom_point() +
	facet_wrap(~Genetic + SiteSame, ncol = 5) +
	ylab("Individuals per square meter") +
	theme(axis.text.x = element_text(angle = -90, vjust = .5, hjust = 1),
		panel.background = element_blank()) +
	scale_x_continuous(breaks=pretty_breaks(n=7))



# Only those that changed
names(ct.all)
ct.all$Psq <- ct.all$X/ct.all$area

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}


summarySE(ct.all, measurevar = 'Psq', groupvars = c('Site','Year'))

ggplot(subset(ct.all, Site == 'Oil Pad (old)' |
			    Site == 'Oil Pad' |
			    Site == 'T-Junction' |
			    Site == 'Road T-West (old)' |
			    Site == 'Atwell Gulch' |
			    Site == 'Atwell Gulch (old)' |
			    Site == 'Pond' |
			    Site == 'Pond (old)' ),
		aes(Year, X/area, colour = Site)) +
	geom_line() +
	geom_point() +
	facet_wrap(~ SiteSame, ncol = 2) +
	ylab("Individuals per square meter") +
	theme(axis.text.x = element_text(angle = -90, vjust = .5, hjust = 1),
		panel.background = element_blank()) +
	scale_x_continuous(breaks=pretty_breaks(n=7))
____________________________________________________________________________________________________


#Figure 2 b: average size, volume of individuals (black) and percent reproductive (red)

ct.all$Prepro <- ct.all$Rep/ct.all$X

# Bad plotting with two scales...
#ggplot(subset(ct.all, Site != 'Road T-East (old)' & Year > 2008),
#		aes(Year)) +
#	geom_line(aes(y = avgvol/10, colour = Site), linetype = 1, color = "black") +
#	geom_line(aes(y = Prepro*100, colour = Site), linetype = 1, color = "red") +
#	facet_wrap(~Site, ncol = 5)  +
#	scale_y_continuous() +
#	theme(axis.text.x = element_text(angle = -90, vjust = .5, hjust = 1),
#		panel.background = element_blank()) +
#	ylab("Percent reproductive (0-100), Average volume") +
#	geom_line(aes(y= 100*(X/area), colour = Site), linetype = 1, colour = "green") +
############


ggplot(subset(ct.all, Site != 'Road T-East (old)' & Year > 2008),
		aes(Year, avgvol, colour = Site)) +
	geom_line() +
	theme(axis.text.x = element_text(angle = -90, vjust = .5, hjust = 1),
		panel.background = element_blank()) +
	ylab("Average volume") +
	facet_wrap(~SiteSame, ncol = 5)      +
	geom_smooth(method = 'lm',
		formula = y ~ poly(x,2), colour = 'black')





#########
# PCA ScGl

climate <- read.csv("Climate_2014.csv", header = T, as.is = T,
	na.strings = "-9999")


str(climate)
names(climate)

## Need January through April of the current year
#		and May through Dec of the previous

pclim <- subset(merge(climate, climate, by = "Site"),
		Year.x == Year.y +1)
head(pclim)
names(pclim)
pclim[,c("Year.x","Year.y")]	# .y is the previous year
may.y <- grep("5.y", names(pclim))
jun.y <- grep("6.y", names(pclim))
jul.y <- grep("7.y", names(pclim))
aug.y <- grep("8.y", names(pclim))
sep.y <- grep("9.y", names(pclim))
oct.y <- grep("10.y", names(pclim))
nov.y <- grep("11.y", names(pclim))
dec.y <- grep("12.y", names(pclim))
jan.x <- grep("p1.x", names(pclim))
jan.x2 <- grep("I1.x", names(pclim))
jan.x3 <- grep("Z1.x", names(pclim))
feb.x <- grep("p2.x", names(pclim))
feb.x2 <- grep("I2.x", names(pclim))
feb.x3 <- grep("Z2.x", names(pclim))
mar.x <- grep("3.x", names(pclim))
apr.x <- grep("4.x", names(pclim))

names(pclim[,c()])

climate <- pclim[,c(1:2,may.y, jun.y, jul.y, aug.y, sep.y, oct.y, nov.y, dec.y,
			jan.x, jan.x2, jan.x3, feb.x, feb.x2, feb.x3, mar.x, apr.x)]


# average by season
# Winter: Nov, Dec, Jan; Spring: Feb, Mar, Apr;
# Summer: May, Jun, Jul; Fall: Aug, Sep, Oct
temp <- grep("Temp", names(climate))
names(climate[,temp[1:3]]);names(climate[,temp[4:6]])

climate$SummerTemp <- rowMeans(climate[,temp[1:3]])
climate$FallTemp <- rowMeans(climate[,temp[4:6]])
climate$WinterTemp <- rowMeans(climate[,temp[7:9]])
climate$SpringTemp <- rowMeans(climate[,temp[10:12]])


Precip <- grep("Precip", names(climate))
names(climate[,Precip[10:12]]);names(climate[,Precip[7:9]])

climate$SummerPrecip <- rowMeans(climate[,Precip[1:3]])
climate$FallPrecip <- rowMeans(climate[,Precip[4:6]])
climate$WinterPrecip <- rowMeans(climate[,Precip[7:9]])
climate$SpringPrecip <- rowMeans(climate[,Precip[10:12]])


PZ <- grep("PZ", names(climate))
names(climate[,PZ[10:12]]);names(climate[,PZ[7:9]])

climate$SummerPZ <- rowMeans(climate[,PZ[1:3]])
climate$FallPZ <- rowMeans(climate[,PZ[4:6]])
climate$WinterPZ <- rowMeans(climate[,PZ[7:9]])
climate$SpringPZ <- rowMeans(climate[,PZ[10:12]])


PDSI <- grep("PDSI", names(climate))
names(climate[,PDSI[10:12]])
names(climate[,PDSI[7:9]])

climate$SummerPDSI <- rowMeans(climate[,PDSI[1:3]])
climate$FallPDSI <- rowMeans(climate[,PDSI[4:6]])
climate$WinterPDSI <- rowMeans(climate[,PDSI[7:9]])
climate$SpringPDSI <- rowMeans(climate[,PDSI[10:12]])

names(climate)
str(climate[,51:66])
corfoo <- data.frame(cor(climate[,51:66], use = "na.or.complete"))
namcorfoo <- names(corfoo)
cors <- reshape(corfoo, direction="long", varying=list(names(corfoo)), v.names="cor",
	times=names(corfoo))
for(i in 1:16){
	cors$var[cors$id == i] <- namcorfoo[i]
}
subset(cors, abs(cors$cor) > .5 & abs(cors$cor) < 1)

cor.test.p <- function(x){
    FUN <- function(x, y) cor.test(x, y, na.action = "na.omit")[[3]]
    z <- outer(
      colnames(x),
      colnames(x),
      Vectorize(function(i,j) FUN(x[,i], x[,j]))
    )
    dimnames(z) <- list(colnames(x), colnames(x))
    z
}

cor.test.p(climate[,51:66])
cor.test(climate[,51:66])



# Keep		# Remove
SummerTemp		FallTemp, SummerPrecip, WinterTemp, SummerPrecip
FallPrecip		FallPZ, WinterPDSI,
WinterPrecip	WinterPZ, SpringPDSI,
SummerPZ		SummerPDSI, FallPDSI,
SpringTemp
SpringPrecip

names(climate[,c(51,56,57,59,54,58)])
scgl.pca <- princomp(climate[,c(51,56,57,59,54,58)])
summary(scgl.pca)

scgl.load <- loadings(scgl.pca)
scgl.pc <- predict(scgl.pca)


library(lattice)
library(latticeExtra)
library(devtools)
library(digest)
#source_url("http://cran.r-project.org/bin/windows/Rtools/") #and then run find_rtools()
source_url("https://raw.github.com/JoFrhwld/FAAV/master/r/stat-ellipse.R")

scgl.pc <- data.frame(PC1 = data.frame(scgl.pc)$Comp.1,
		PC2 = data.frame(scgl.pc)$Comp.2,
		Site = climate$Site,
		Year = climate$Year)
ggplot(scgl.pc, aes(PC1, PC2, colour = Site)) +
	geom_point() +
	stat_ellipse() +
	theme(plot.background = element_blank())


# ?TukeyHSD  Tukey Honest Significant Differences
# Sites are differentiated by Component 1
# Needs to not have additional '-' like in T-Junction

levels(scgl.pc$Site)[levels(scgl.pc$Site) == 'T-Junction'] <- 'TJunction'
scgl.THSD <- TukeyHSD(aov(PC1 ~ Site, data = scgl.pc))
str(scgl.THSD)
scgl.THSD$Site

library(multcompView)
lasign <- multcompLetters(extract_p(scgl.THSD$Site))
str(lasign)

SiteLetters <- data.frame(Letter = lasign$Letters,
		Site = rownames(data.frame(lasign$Letters)))

SiteLetters$Letter[order(SiteLetters$Site)]

ggplot(scgl.pc, aes(Site, PC1)) +
	geom_boxplot() +
	annotate("text", x = 1:10, y = 10,
		label = SiteLetters$Letter[order(SiteLetters$Site)])


# Can skip or redo PCoA with only study years
#_______________________________________________________
## Only study years
clim2 <- climate[,c(1,2,51,56,57,59,54,58)]
clim3 <- subset(climate, Year.x > 2007)
clim4 <- clim3[,-c(1:2)]
scgl.pca <- princomp(clim4)
summary(scgl.pca)

scgl.load <- loadings(scgl.pca)
scgl.pc <- predict(scgl.pca)
#_______________________________________________________


climate$Site[climate$Site == "Atwell"] <- "Atwell Gulch"
climate$Site[climate$Site == "Picnic"] <- "Picnic Site"

head(ct.all);head(scgl.pc)
unique(climate$Site);unique(ct.all$SiteSame);unique(ct.all$Site);unique(scgl.pc$Site)

#_______________________________________________________
#  For only study years
scgl.pc2 <- data.frame(PC1 = data.frame(scgl.pc)$Comp.1,
		PC2 = data.frame(scgl.pc)$Comp.2,
		Site = clim3$Site,
		Year = clim3$Year)
#  For only study years
#_______________________________________________________

scgl.pc <- data.frame(PC1 = data.frame(scgl.pc)$Comp.1,
		PC2 = data.frame(scgl.pc)$Comp.2,
		Site = climate$Site,
		Year = climate$Year)
#_______________________________________________________

sg.merge <- merge(scgl.pc, ct.all,
	by.x = c('Site','Year'), by.y = c('SiteSame', 'Year'))

t.test(PC1 ~ Genetic, data = sg.merge)
summary(aov(PC1 ~ Site, data = sg.merge))

names(sg.merge) <- c(names(sg.merge)[1:4],'OldSite',names(sg.merge)[6:17])

ggplot(scgl.pc, aes(PC1, PC2, colour = Site)) +
	geom_point() +
	stat_ellipse()

# Percent reproductive
avgPR <- mean(sg.merge$Prepro)
highPR <- mean(sg.merge$Prepro)+sd(sg.merge$Prepro)
lowPR <- mean(sg.merge$Prepro)-sd(sg.merge$Prepro)

sg.merge$PreproHL <- 'mid'
sg.merge$PreproHL[sg.merge$Prepro > highPR] <- 'high'
sg.merge$PreproHL[sg.merge$Prepro < lowPR] <- 'low'

ggplot(sg.merge, aes(PC1, PC2, colour = PreproHL)) +
	geom_point() +
	stat_ellipse()
# On PC1, more reproductive so older individuals

merge.all <- merge(ct.all, climate, by.x = c('Site', 'Year'),
		by.y = c('Site', 'Year.x'))
names(merge.all)

summary(lm(Prepro ~ PC1, data = sg.merge))

lmrep <- lm(Prepro ~ (SummerTemp + SummerPZ + SpringTemp)^2,
		data = merge.all)
step(lmrep, ~1, direction = 'both')

summary(lm(Prepro ~ SummerTemp, data = merge.all))


## Individuals per square meter
sg.merge$InpM <- sg.merge$X/sg.merge$area
avgInpM <- mean(sg.merge$InpM)
highInpM <- mean(sg.merge$InpM)+sd(sg.merge$InpM)
lowInpM <- mean(sg.merge$InpM)-sd(sg.merge$InpM)

sg.merge$InpMHL[sg.merge$InpM > avgInpM] <- 'high'
sg.merge$InpMHL[sg.merge$InpM <= avgInpM] <- 'low'

ggplot(sg.merge, aes(PC1, PC2, colour = InpMHL)) +
	geom_point() +
	stat_ellipse()




lmrep <- lm(avgvol ~ (SummerTemp + SummerPZ + SpringTemp)^2,
		data = merge.all)
step(lmrep, ~1, direction = 'both')



# Size
avgVol <- mean(sg.merge$avgvol, na.rm = T)
highVol <- mean(sg.merge$avgvol, na.rm = T)+sd(sg.merge$avgvol, na.rm = T)
lowVol <- mean(sg.merge$avgvol, na.rm = T)-sd(sg.merge$avgvol, na.rm = T)

sg.merge$VolHL <- 'mid'
sg.merge$VolHL[sg.merge$avgvol > highVol] <- 'high'
sg.merge$VolHL[sg.merge$avgvol < lowVol] <- 'low'


ggplot(sg.merge, aes(PC1, PC2, colour = VolHL)) +
	geom_point() +
	stat_ellipse()


ggplot(sg.merge, aes(PC1, PC2, colour = Genetic)) +
	geom_point() +
	stat_ellipse()



#### Size year to year
head(sg)
sg.xy <- subset(merge(sg, sg, by = c('Site','Transect','Tag'),
	sort = F), Year.x = Year.y +1)

head(sg.xy)
ggplot(sg.xy, aes(vol.x, vol.y)) +
	geom_point()

ggplot(sg.xy, aes(colour = Site)) +
	stat_smooth(method = lm, aes(y=Height.cm..x, x=Height.cm..y)) +
	ylab(expression("Height year"[y])) +
	xlab(expression("Height year"[y-1]))


ggplot(sg.xy, aes(colour = Site)) +
	stat_smooth(method = lm, aes(y=Width.cm..x, x=Width.cm..y)) +
	ylab(expression("Width year"[y])) +
	xlab(expression("Width year"[y-1]))

summary(lm(Height.cm..x ~ Height.cm..y, data = sg.xy))
summary(lm(Width.cm..x ~ Width.cm..y, data = sg.xy))

summary(lm(Height.cm..x ~ Height.cm..y*Site, data = sg.xy))
summary(lm(Width.cm..x ~ Width.cm..y*Site, data = sg.xy))

library(plotrix)

sg.xy$diffH <- sg.xy$Height.cm..x-sg.xy$Height.cm..y
mean(sg.xy$diffH[sg.xy$Height.cm..x != 0], na.rm = T)
std.error(sg.xy$diffH[sg.xy$Height.cm..x != 0], na.rm = T)

sg.xy$diffW <- sg.xy$Width.cm..x-sg.xy$Width.cm..y
mean(sg.xy$diffW[sg.xy$Width.cm..x != 0], na.rm = T)
std.error(sg.xy$diffW[sg.xy$Width.cm..x != 0], na.rm = T)


#### Stepwise regressions
names(sg)
names(climate)

sg.merge2 <- subset(merge(sg.merge,sg.merge,
	by=c('Site','Year'), sort = F), Year.x == Year.y +1)

# step on the volume of individuals
lm.vol <- lm(


lm.scgl <- lm(



#_________________________________________________
#Minis

head(sg)

ddply(subset(sg, Site == 'Pond'), .(Year), summarize,
	Minis = sum(Minis, na.rm = T))
ddply(subset(sg, Site == 'Pond (old)'), .(Year), summarize,
	Minis = sum(Minis, na.rm = T))

MoreMinis <- ddply(subset(sg, Year > 2008 & Height.cm. < 0.5 & Height.cm. > 0 & Site != 'Atwell Gulch'),
		.(Site, Year), summarize,
		Minis = length(Tag))

AtwellMinis <- ddply(subset(sg, Year > 2008 & Height.cm. < 0.5 &
			Height.cm. > 0 & Site == 'Atwell Gulch' &
			Y.coord.m. < 30),
		.(Site, Year), summarize,
		Minis = length(Tag))
AtwellMinis$Site <- 'Atwell Gulch (old)'

AtwellMinisNew <- ddply(subset(sg, Year > 2008 & Height.cm. < 0.5 &
			Height.cm. > 0 & Site == 'Atwell Gulch'),
		.(Site, Year), summarize,
		Minis = length(Tag))
AtwellMinisNew



sg$Minis[sg$Minis > 100] <- NA	# photo number in wrong column
sg$Minis[sg$


MarkedMinis <- ddply(subset(sg, Site != 'Atwell Gulch'), .(Site, Year), summarize,
	Minis = sum(Minis, na.rm = T))
MAtwellMinis <- ddply(subset(sg, Year > 2008 & Height.cm. < 0.5 &
			Height.cm. > 0 & Site == 'Atwell Gulch' &
			Y.coord.m. < 30),
		.(Site, Year), summarize,
		Minis = sum(Minis, na.rm = T))
MAtwellMinis$Site <- 'Atwell Gulch (old)'

MAtwellMinisNew <- ddply(subset(sg, Year > 2008 & Height.cm. < 0.5 &
			Height.cm. > 0 & Site == 'Atwell Gulch'),
		.(Site, Year), summarize,
		Minis = sum(Minis, na.rm = T))
MAtwellMinisNew





allminis <- ddply(rbind(MoreMinis, MarkedMinis, AtwellMinis, AtwellMinisNew,
			MAtwellMinisNew, MAtwellMinis),
	 .(Site, Year), summarize,
	Minis = sum(Minis, na.rm = T))



# groupings
sg$Genetic <- "South"
sg$Genetic[sg$Site == "Atwell Gulch" |
			sg$Site == "Atwell Gulch (old)" |
			sg$Site == "Oil Pad" |
			sg$Site == "Oil Pad (old)" |
			sg$Site == "Pond" |
			sg$Site == "Pond (old)" |
			sg$Site == "Road T-West (old)" |
			sg$Site == "Road T-East (old)" |
			sg$Site == "T-Junction" |
			sg$Site == "Pyramid Rock"] <- "North"


# Same and changing sites
allminis$SiteSame <- allminis$Site
allminis$SiteSame[allminis$Site == "Atwell Gulch" |allminis$Site == "Atwell Gulch (old)"] <- "Atwell Gulch"
allminis$SiteSame[allminis$Site == "Pond" |allminis$Site == "Pond (old)"] <- "Pond"
allminis$SiteSame[allminis$Site == "Road T-West (old)" |allminis$Site == "T-Junction"] <- "T-Junction"
allminis$SiteSame[allminis$Site == "Oil Pad (old)" |allminis$Site == "Oil Pad"] <- "Oil Pad"

unique(allminis$SiteSame)

ggplot(subset(allminis, Site != 'Road T-East (old)'), aes(Year, Minis, colour = Site)) +
	geom_line() +
	facet_wrap(~ SiteSame) +
	theme_bw()








##############################################################################
##############################################################################
##############################################################################
##############################################################################
#______ Just need Rawdata 2014
sg1213 <- read.csv("2012on_scgl_140922.csv",
	header = TRUE, as.is=TRUE)
str(sg1213)
unique(sg1213$Site)
table(sg1213$Site,sg1213$Transect)
lineplot.CI(sg1213$Year, sg1213$Num.Height, sg1213$Site)

##____________________________________________________________________
install.packages("xkcd", repos="http://R-Forge.R-project.org")
library(xkcd)
library(sysfonts)
?xkcd

#library(sysfonts)
 if( "xkcd.ttf" %in% font.files()) {
 font.add("xkcd", regular = "xkcd.ttf")
 p <- ggplot() + geom_point(aes(x=mpg, y=wt), data=mtcars) +
 theme(text = element_text(size = 16, family = "xkcd"))
 } else {
 warning("Not xkcd fonts installed!")
 p <- ggplot() + geom_point(aes(x=mpg, y=wt), data=mtcars)
 }
 p


library(sysfonts)
 download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf", mode="wb")
 font.paths()
 system("mkdir ~/.fonts")
 system("cp xkcd.tff -t ~/.fonts")
 font.files()
 font.add("xkcd", regular = "xkcd.ttf")
 font.families()

install.packages("extrafont");
library(extrafont)
font_import(pattern="[C/c]omic") # wait and hit y
font_import(pattern="[A/a]rial")# wait and hit y


help(package="xkcd")
 vignette("xkcd-intro") # it opens the pdf
 browseVignettes(package = "xkcd") # To browse the pdf, R and Rnw

download.file("http://simonsoftware.se/other/xkcd.ttf",     dest="xkcd.ttf")
     system("mkdir ~/.fonts")
     system("cp xkcd.tff -t ~/.fonts")
     library(extrafont)
     font_import()

font_import(pattern="[H/h]umor")

for(dirs in font.paths()) {
file.copy(file.path(dirs,"xkcd.ttf"), "/Users/shambho/library/Fonts/")
}

#### http://stackoverflow.com/questions/12675147/how-can-we-make-xkcd-style-graphs-in-r

xkcd_line <- function(x, y, color) {
  len <- length(x);
  rg <- par("usr");
  yjitter <- (rg[4] - rg[3]) / 1000;
  xjitter <- (rg[2] - rg[1]) / 1000;
  x_mod <- x + rnorm(len) * xjitter;
  y_mod <- y + rnorm(len) * yjitter;
  lines(x_mod, y_mod, col='white', lwd=10);
  lines(x_mod, y_mod, col=color, lwd=5);
}

 xkcdFontURL <- "http://simonsoftware.se/other/xkcd.ttf"
   download.file(xkcdFontURL,dest="xkcd.ttf")
font_import(".")

head(sg1213)

foo <- ddply(sg1213, .(Year, Site), summarize,
	ScGl = sum(Num.Height))

ggplot(foo, aes(Year, ScGl)) +
	xkcdaxis(range(foo$Year), range(foo$ScGl)) +
	geom_boxplot(family = 'xkcd')

ggplot(sg1213, aes(Year, ScGl)) +
	xkcdaxis(range(foo$Year), range(foo$ScGl))

##############################################################################
##############################################################################
##############################################################################
##############################################################################
#______ Just need Rawdata 2014 and 2008 data



#################################################################################
##____________________________ xkcd
# http://fibosworld.wordpress.com/2013/02/17/change-fonts-in-ggplot2-and-create-xkcd-style-graphs/

ggplot(ct, aes(Year, x)) +
	xkcdaxis(range(ct$Year), range(ct$x)) +
	geom_line(family = 'xkcd') +
	facet_wrap(~Site) +
	timelabel("Wet year", 2009, 150)

ggplot(ct, aes(Year, x)) +
	geom_line() +
	ggtitle("Sclerocactus glaucus trends") +
	theme(text=element_text(size =16, family="Comic Sans MS'))
##____________________________ xkcd






