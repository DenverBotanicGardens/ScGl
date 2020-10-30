### Packages
library(ggplot2)
library(popbio)
library(MASS)
library(dplyr)
library(reshape2)
library(ggplot2)

# read in data
currentYr <- as.numeric(format(as.Date(Sys.Date(),format="%Y-%m-%d"), "%Y"))-1
sg <- read.csv(paste("Q:/Research/All_Projects_by_Species/Sclerocactus SPECIES/Sclerocactus_glaucus/R_code_ScGl/R_tables/RawData_scgl_",
                     currentYr, ".csv",
                     sep=""))  

# sort 
sg <- sg[order(sg$Site, sg$Transect, sg$Tag, sg$Year),]
# Make all Heights and Widths that aren't measured NA
sg$Width.cm.[sg$Width.cm. == 0] <- NA

## data clean
sg <- sg[which(complete.cases(sg[,c("Tag")])),]
sg <- sg[grep("T-East", sg$Site, invert = TRUE),]
sg$Site <- as.character(sg$Site)
sg$Site <- as.factor(sg$Site)
sg$MainTag <- floor(sg$Tag)
# ones where height and width swapped or typos
sg$Width.cm.[sg$Year == 2017 & sg$Tag==363.00] <- 4.4
sg$Height.cm.[sg$Year == 2017 & sg$Tag==363.00] <- 1
sg$Width.cm.[which(sg$Year == 2013 & sg$Tag==1242)] <- 11.8 
sg$Width.cm.[which(sg$Year == 2009 & sg$Tag==96)] <- 9.8 
sg$Fl[which(sg$Width.cm. < 2.5 & sg$Fl == "y")] <- "n"
# sg$Width.cm.[is.na(sg$Width.cm.)] <- 0
# sg$Height.cm.[is.na(sg$Height.cm.)] <- 0
# for when the width is missing, make it the same as the height
sg$Width.cm.[sg$Height.cm.>0 & sg$Width.cm. == 0] <- sg$Height.cm.[sg$Height.cm.>0 & sg$Width.cm. == 0]
sg$Site <- as.character(sg$Site)

# Atwell gulch got extended transects (3 of them from 30 meters to 50 meters) in 2012
sg$Site[grep("Atwell", sg$Site)] <- "Atwell Gulch (old)"
sg$Site[sg$Site == "Atwell Gulch (old)" & sg$Y.coord.m. >= 30] <- "Atwell Gulch"
table(sg$Site)
sg$Population[sg$Site == "Fram"] <- "Whitewater" # Fram was missing data in 2015, need to impute
sg$Population[sg$Site %in% c("Pond","T-Junction","Oil Pad","Oil Pad (old)","Road T-West (old)", "Pond (old)",
                             "Pyramid Rock")] <- "Roan Creek"
sg$Population[sg$Site %in% c("Escalante Canyon","Picnic Site")] <- "Dominguez-Escalante"
sg$Population[sg$Site == "Bridgeport"] <- "Gunnison River East"
sg$Population[sg$Site == "Powerline"] <- "Cactus Park"
sg$Population[grep("Atwell", sg$Site)] <- "Plateau Creek"
sum(table(sg$Population))
sum(table(sg$Site))

sg$Site <- as.factor(sg$Site) # back and forth to remove the unused levels
table(sg$Site, sg$Year)
sg.temp <- sg[!(sg$Site == "Pond (old)" & sg$Year > 2014),]
table(sg.temp$Site, sg.temp$Year)
sg <- sg.temp

# Make NAs for missing data instead of zeros
# Like what Dan did in "erbr_ReformatData_tagClusterV2 doak m..." 
sg$UniqueTag <- unlist(lapply(1:nrow(sg), function(x) paste(sg$Site[x], sg$Transect[x], sg$Tag[x], sep="_")))
tags <- unique(sg$UniqueTag)

# must be a typo, yup, photo number moved over to minis
sg[sg$Site == "Atwell Gulch (old)" & sg$Year == 2013 & !is.na(sg$Minis),]
sg$Minis[is.na(sg$Minis)==FALSE & sg$Minis > 100] <- NA

## Minis for recruitment -----------------------------------------------------
# need to get rid of rows where the first occurrence is dead, get minis before remove rows
minis <- aggregate(sg$Minis, by = list(Transect = sg$Transect,
                                       Site = sg$Site,
                                       Year = sg$Year), sum, na.rm=TRUE)
minis <- minis[order(minis$Site, minis$Transect, minis$Year),]
## ---------------------------------------------------------------------------

# oops, there are some rows where a mini is noted in the comments but the individual isn't alive and there may or may not be any minis in the Minis column
sg[sg$Tag == 831.07 & sg$Site == "Pond",] # in 2015 someone said, that's a mini, I won't measure, just mark as a mini. 
sg[grepl("mini", sg$Comments) & is.na(sg$Minis),]
aggregate(minis$x, by = list(Year = minis$Year), sum) # this has duplicates in 2012 and what's happening in 2013?? 


# only keep rows after first appearance and not after death (remove any NA rows that are not in the middle of data)
sg$save <- 0
# tt <- tags[3]
for(tt in tags){
  szrows <- which(sg$UniqueTag == tt) # pick only rows with this tag
  szs <- sg$Width.cm.[szrows]
  # find only ones with some measure of size, rest are dead, need to make in the middles ones NA
  goodszsrows <- c(which(sg$Year == 2008 & sg$UniqueTag == tt), szrows[is.na(szs)==FALSE]) 
  
  if(length(goodszsrows)>0){
    firstgoodrow <- min(goodszsrows)
    lastgoodrow <- max(goodszsrows)
    sg$save[firstgoodrow:lastgoodrow] <- 1
  }
}

table(sg$save)
head(sg[sg$save == 0,],100)
sg[sg$Year == 2008,]
sg <- sg[sg$save == 1,]

# add stages to match BLM 
# Make all the sizes for 2008 be imputed from the following year, that's different from the previous year but should be doable
sg$Stage <- as.character("Reproductive")
sg$Stage[sg$Fl == "y"] <- "Reproductive" 
sg$Stage[sg$Fl == "n"] <- "Vegetative"
sg$Stage[sg$Year != 2008 & sg$Width.cm. < 0.5] <- "Seedling"  # need to add in minis when talking about recruitment - these are seedlings
# sg$Stage[sg$Year != 2008 & sg$Width.cm. == 0] <- "dead" 
table(sg$Stage, sg$Year) # 2008 we don't have measures of size 
table(sg$Stage, sg$Year, sg$Site)

# no one should be dead first time seen, that should have been removed above
# sg <- sg[!duplicated(sg[c("Tag","Transect","Site")]) & sg$Stage == "dead",]

# Only the first time a plant was added, no matter the size, call it a seedling, the rest are vegetative
sg$Stage[!duplicated(sg[c("Tag","Transect","Site")]) & sg$Width.cm. < 0.5 & sg$Width.cm. >0 & 
     is.na(sg$Width.cm.) == FALSE] # These should be seedlings

# These should not be seedlings because tag is duplicated so it's not the first time it was seen
sg[duplicated(sg[c("Tag","Transect","Site")]) & sg$Width.cm. < 0.5 & sg$Width.cm. >0 & is.na(sg$Width.cm.) ==  FALSE,] 
sg$Stage[duplicated(sg[c("Tag","Transect","Site")]) & sg$Width.cm. < 0.5 & sg$Width.cm.>0 & is.na(sg$Width.cm.) ==  FALSE] <- "Vegetative"

# When not the first year of the transect and a tag is new, it is a recruit but clearly not a seedling, how to account for that? Probably another column like mini to add
sg$Recruitment <- 0
firstyear <- do.call(rbind,lapply(split(sg, sg$Site), function(site){
  data.frame(FirstYear = min(site$Year), Site = unique(site$Site))
}))
sg$Recruitment[sg$Stage == "Seedling"] <- 1 # These are recruits

table(sg$Recruitment, sg$Stage)

# ss <- unique(sg$Site)[1]
# rm(ss)
for(ss in unique(sg$Site)){
  fstyr <- firstyear$FirstYear[firstyear$Site == ss]
  sg$Recruitment[!duplicated(sg[c("Tag","Transect","Site")]) & 
       sg$Site == ss &
       sg$Year != fstyr] <- 1 # !duplicated()==first time the tag was introduced, likely missed before
  
}

table(sg$Recruitment, sg$Stage)
table(sg$Stage)

# $lagsrtsz for the number of years back you need to go to find a measured size (will mostly just be the one year missed) ==0 for first year and for missing years

MPM.scgl1 <- sg[,c("Site","Population","Year","Tag","Transect","Stage","Height.cm.","Width.cm.","Fl","Br","Minis","MainTag","Recruitment",
                   "UniqueTag")]
sg.stagefate <- subset(merge(MPM.scgl1, MPM.scgl1, by = c("Site","Population","Tag","Transect","MainTag","UniqueTag")), sort = FALSE, Year.x == Year.y - 1)
# sg.stagefate$Stage.y[sg.stagefate$Stage.x == "Seedling" & sg.stagefate$Stage.y == "Seedling"] <- "Vegetative" 

# get rid of ones that start dead
sg.stagefate <- sg.stagefate[sg.stagefate$Stage.x != "dead",]

sg.stagefate$survived <- 1
sg.stagefate$survived[sg.stagefate$Width.cm..y==0] <- 0
sg.stagefate

model.surv <- glm(survived ~ Width.cm..x*Site + Population , family = binomial, data = sg.stagefate)
model.surv2 <-glm(survived ~ Width.cm..x*Population , family = binomial, data = sg.stagefate)
model.surv3 <- glm(survived ~ Stage.x*Site, family = binomial, data = sg.stagefate )
summary(model.surv)
summary(model.surv2)
summary(model.surv3)

# Survival
ggplot(sg.stagefate, aes(Width.cm..x, survived, colour =  Site))+
  geom_point()+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  # geom_smooth(method="glm.nb", colour = "darkgreen")+  # for growth
  facet_wrap(~Population)+
  xlab("Width (cm)")+
  ylab("survival")+
  theme_bw()

ggplot(sg.stagefate, aes(Width.cm..x, survived, colour = Population))+
  geom_point()+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  # geom_smooth(method="glm.nb", colour = "darkgreen")+  # for growth
  theme_bw()

ggplot(sg.stagefate, aes(Width.cm..x, survived, colour = Population))+
  geom_point()+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  # geom_smooth(method="glm.nb", colour = "darkgreen")+  # for growth
  theme_bw()+
  facet_wrap(~Year.x)


x <- split(sg.stagefate, list(sg.stagefate$Site,sg.stagefate$Stage.x))
sum(x[[3]]$survived)/(nrow(x[[3]]))
survival <- do.call(rbind,lapply(split(sg.stagefate, list(sg.stagefate$Site,sg.stagefate$Stage.x)), function(x){
  PercSurv <- sum(x$survived)/nrow(x)
  data.frame(Site=(x$Site[1]), Pop=(x$Population[1]),Stage = x$Stage.x[1],PercSurv)
}))

write.table(survival, "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus glaucus_SSA_2020/survival.csv",
            sep=",", row.names = FALSE)

df <- split(sg.stagefate, sg.stagefate$Site)
(nrow(df[[1]][df[[1]]$Stage.y == "Seedling",])+sum(df[[1]]$Minis.y, na.rm=TRUE))

## Recruitment
recruitment <- do.call(rbind,lapply(split(sg.stagefate, sg.stagefate$Site), function(df){
  recruit <- (nrow(df[df$Stage.y == "Seedling",])+sum(df$Minis.y, na.rm = TRUE))/nrow(df[df$Stage.x == "Reproductive",])
  data.frame(Site=(df$Site[1]), Pop=(df$Population[1]),recruit)
}))

write.table(recruitment, "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus glaucus_SSA_2020/recruitment.csv",
            sep=",", row.names = FALSE)

df2 <-split(sg.stagefate, sg.stagefate$Site)
nrow(df2[['Atwell Gulch (old)']][df2[['Atwell Gulch (old)']]$Stage.x == "Seedling" & 
                                   df2[['Atwell Gulch (old)']]$Year.x == 2013,])/
  nrow(df2[['Atwell Gulch (old)']][df2[['Atwell Gulch (old)']]$Stage.x == "Reproductive"
                                   & df2[['Atwell Gulch (old)']]$Year.x == 2012,])
df3 <- df2[['Atwell Gulch (old)']]
sg.stagefate[sg.stagefate$Stage.y == "Seedling",]
unique(df3$Year.x)[-length(unique(df3$Year.x))]

recruitmentXyear <- do.call(rbind,lapply(split(sg.stagefate, sg.stagefate$Site, function(df){
  recruit.out <- NA 
  for(yr in unique(df$Year.x)){
    recruit.out <- rbind(recruit.out,(nrow(df[df$Stage.x == "Seedling" & df$Year.x == yr+1,])+
                                        sum(df$Minis.x[df$Year.x == yr+1], na.rm = TRUE))/nrow(df[df$Stage.x == "Reproductive" &
                                                                                                    df$Year.x == yr,]))
    if(is.infinite(recruit)) recruit <- 0
    if(!is.na(recruit)){
      data.frame(Site=(df$Site[1]), Pop=(df$Population[1]),Year = df$Year.x[1], recruit[-1])
    }
  }
}))

write.table(recruitmentXyear, "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus glaucus_SSA_2020/recruitmentXyear.csv",
            sep=",", row.names = FALSE)


ggplot(recruitmentXyear, aes( Pop,recruit))+
  geom_boxplot()
