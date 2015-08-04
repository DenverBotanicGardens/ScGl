
setwd(path.expand("Q:/Research/All_Projects_by_Species/Sclerocactus SPECIES/Sclerocactus_glaucus/R_code_ScGl/R_tables/"))



######## Choose: SummaryTable2012_R.txt  ###########

ScGl<-read.delim("SummaryTable2012_R.txt", as.is=FALSE, sep=",",
	na.strings="NA", header=TRUE)
colnames(ScGl)<-c("Year","SiteName","Transect","Height","Width",
	"Flowered","Browsed","SiteName12")
names(ScGl)
head(ScGl)
str(ScGl)

######## Choose: SummaryTable2008_R.txt  ###########

ScGl08<-read.delim(file.choose("Q://Research//PROJECTS//All_Projects_by_Species//
	Sclerocactus_glaucus//R_code_ScGl//R_tables"), as.is=FALSE, sep=",",
	na.strings="NA", header=TRUE)
colnames(ScGl08)<-c("Year","SiteName","Transect","Height","Width",
	"Flowered","Browsed","SiteName12")
names(ScGl08)
head(ScGl08)
str(ScGl08)

 
########### Choose: SummaryTable2012pond_R.txt

pond<-read.delim(file.choose("Q://Research//PROJECTS//All_Projects_by_Species//
	Sclerocactus_glaucus//R_code_ScGl//R_tables"), as.is=FALSE, sep=",",
	na.strings="NA", header=TRUE)					
colnames(pond)<-c("Year","SiteName12","Transect","Height","Width",
	"Flowered","Browsed")
pond<-transform(pond, SiteName12 = as.factor("Pond"),
	Transect = as.factor(pond$Transect))
names(pond)
head(pond)
str(pond)

head(ScGl[,c(1,8,3:7)])
head(ScGl08[,c(1,8,3:7)])
head(pond[,c(1:7)])

ScGlSum <- rbind(ScGl[,c(1,8,3:7)],ScGl08[,c(1,8,3:7)],pond[,c(1:7)])
head(ScGlSum)
str(ScGlSum)
ScGlSum <- transform(ScGlSum, Flowered = abs(Flowered), Browsed = abs(B)

SG08<-subset(ScGlSum, ScGlSum$Year ==  2008)
colMeans(SG08[,c(4:5)])


SG09<-subset(ScGlSum, ScGlSum$Year ==  2009)
unique(SG09$SiteName12)
colMeans(SG09[,c(4:5)], na.rm = TRUE)
sapply(SG09[,c(4,5)], sd, na.rm = TRUE)



SG10<-subset(ScGlSum, ScGlSum$Year ==  2010)
unique(SG10$SiteName12)
colMeans(SG10[,c(4:5)], na.rm = TRUE)
sapply(SG10[,c(4,5)], sd, na.rm = TRUE)


SG11<-subset(ScGlSum, ScGlSum$Year ==  2011)
unique(SG11$SiteName12)
colMeans(SG11[,c(4:5)], na.rm = TRUE)
sapply(SG11[,c(4,5)], sd, na.rm = TRUE)

SG12<-subset(ScGlSum, ScGlSum$Year ==  2012)
unique(SG12$SiteName12)
colMeans(SG12[,c(4:5)], na.rm = TRUE)
sapply(SG12[,c(4,5)], sd, na.rm = TRUE)


########### Choose: SummaryTablePost2012_R.txt

post<-read.delim(file.choose("Q://Research//PROJECTS//All_Projects_by_Species//
	Sclerocactus_glaucus//R_code_ScGl//R_tables"), as.is=FALSE, sep=",",
	na.strings="NA", header=TRUE)					
colnames(post)<-c("Year","SiteName12","Transect","Height","Width",
	"Flowered","Browsed")
pond<-transform(post, SiteName12 = as.factor("Pond"),
	Transect = as.factor(pond$Transect))
names(post)
head(post)
str(post)


SG12p<-subset(post, post$Year ==  2012)
unique(SG12p$SiteName12)
colMeans(SG12p[,c(4:5)], na.rm = TRUE)
sapply(SG12p[,c(4,5)], sd, na.rm = TRUE)













