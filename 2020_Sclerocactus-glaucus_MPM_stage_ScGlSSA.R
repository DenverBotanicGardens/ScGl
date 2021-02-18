### Packages
library(ggplot2)
library(popbio)
library(MASS)
# library(dplyr)
library(reshape2)


rm(list=ls())
# read in data
currentYr <- 2019 # as.numeric(format(as.Date(Sys.Date(),format="%Y-%m-%d"), "%Y"))
sg.raw <- read.csv(paste("C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus-glaucus/Sclerocactus-glaucus_AnnualReports/2008-currentyr_Sclerocactus_glaucus_R_tables/RawData_scgl_",
                     currentYr, ".csv",
                     sep=""))  

sg <- sg.raw
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
sg$Width.cm.[sg$Height.cm.>0 & is.na(sg$Width.cm.) & !is.na(sg$Height.cm.)] <- 
  sg$Height.cm.[sg$Height.cm.>0 & is.na(sg$Width.cm.)& !is.na(sg$Height.cm.)]
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





## Stage Based MPM ------------------------------------------------------------------------------
# add stages to match BLM 
# Make all the sizes for 2008 be imputed from the following year, that's different from the previous year but should be doable
sg$Stage <- as.character("Reproductive")
sg$Stage[sg$Fl == "y"] <- "Reproductive" 
sg$Stage[sg$Fl == "n"] <- "Vegetative"

# Testing
# sgsite <- split(sg, sg$Site)[[11]]
# years <- min(sgsite$Year):max(sgsite$Year) 
# yr <- 5
rm(years)
# Make matrix models not in popbio, make wide and same as BLM data
# Recruitment will be new tags each year and sum of all 'minis'
# remove Pond and Fram 2015
TMx_sg <- lapply(split(sg, sg$Site), function(sgsite){
  print(unique(sgsite$Site))
  if(unique(sgsite$Site) %in% c("Fram","Pond","Pond (old)")){
    years <- c(min(sgsite$Year):2014,2016:max(sgsite$Year))
  } else {
    years <- min(sgsite$Year):max(sgsite$Year)
  }
  if(unique(sgsite$Site) %in% c("Escalante Canyon", "Picnic Site", "Powerline","Pyramid Rock")){
    years <- c(2009:max(sgsite$Year))
  }
  if(unique(sgsite$Site) %in% "Pond (old)"){
    years <- min(sgsite$Year):2013
  }
  sgMinis <- sgsite
  sgsite <- sgsite[!is.na(sgsite$Width.cm.),]
  Mx <- list() # vegetative and reproductive
  for(yr in 2:length(years)){
    print(years[yr])
    repro <- nrow(sgsite[sgsite$Fl == "y" & sgsite$Year == years[yr-1],]) # Reproductive last year
    newtags <- setdiff(sgsite$Tag[sgsite$Year == years[yr]],sgsite$Tag[sgsite$Year == years[yr-1]]) # tags in yr that weren't in yr-1
    if(length(newtags)==0){
      newtags <- NA
      recruit <- 0
      recruitStage <- c(0,0)
    } else {
      recruit <- length(newtags) # how many in t1 that weren't in t0
      recruitStage <- table(sgsite$Stage[sgsite$Tag %in% newtags & sgsite$Year == years[yr]]) # new this year since last year
    }
    minis <- sum(sgMinis$Minis[sgMinis$Year == years[yr]], na.rm = TRUE)  
    fecund2Veg <- (recruitStage[2] + minis)/repro # how many vegetative individuals 
    fecund2Rep <- recruitStage[1]/repro # how many newly discovered individuals are reproductive - this should be assigned to on fertility transition
    R <- (recruit + minis)/repro
    # How many tags were found in year-1 and year, survived, and what was their stage in year-1
    # survStage <- data.frame(Reproductive = 0, Vegetative = 0)
    survStage <- table(sgsite$Stage[sgsite$Tag %in% intersect(sgsite$Tag[sgsite$Year == years[yr]],sgsite$Tag[sgsite$Year == years[yr-1]]) &
                                      sgsite$Year == years[yr-1]])
    if(length(survStage) < 2) {
      survStage <- c(survStage, 0)
      names(survStage) <- c(names(survStage)[1], setdiff(c("Reproductive","Vegetative"), names(survStage)))
      }
    # How many tags grew from veg in year-1 to reproductive in year, 
    growthVegRep <- length(intersect(sgsite$Tag[sgsite$Year == years[yr] & sgsite$Stage == "Reproductive"],
                                     sgsite$Tag[sgsite$Year == years[yr-1]& sgsite$Stage == "Vegetative"]))
    G <- growthVegRep/survStage["Vegetative"]
    Staget0 <- table(sgsite$Stage[sgsite$Year == years[yr-1]]) # this is how many there were in year-1 but not all survived
    Mx[[yr-1]] <- matrix(c(survStage["Vegetative"]/Staget0["Vegetative"], # stasis of vegetative
                           growthVegRep/Staget0["Vegetative"], # growth from veg to reproductive
                           R, # fecundity (collapse recruitment to veg and repro)
                           survStage["Reproductive"]/Staget0["Reproductive"]), nrow = 2) # stasis of reproductive
  }
  names(Mx) <- paste(unique(sgsite$Site), unique(sgsite$Population), years[-length(years)], sep="_")
  Mx
})

# sgsite[sgsite$Tag %in% intersect(sgsite$Tag[sgsite$Year == yr],sgsite$Tag[sgsite$Year == (yr-1)]),]

# TMx_sg[[5]] # Fram missing data
# TMx_sg[[4]] # Why does Escalante have numbers larger than 1 for stasis of reproductive? 
# TMx_sg[[9]] # Pond has missing data in 2015 but not issues for the matrices
# TMx_sg[[12]]
elasts <- do.call(rbind,lapply(c(1:length(TMx_sg)), function(i){
            out <- do.call(rbind,lapply(TMx_sg[[i]], function(x){
              # print(i)
              e_ij <- elasticity(x)
              G <- e_ij[2]
              S <- sum(e_ij[c(1,4)])
              R <- e_ij[3]
              data.frame(lam = lambda(x), gentime = generation.time(x), G, S, R)
              }))
            out
            }))

# Estimated generation time
mean(elasts$gentime[!is.infinite(elasts$gentime)], na.rm = TRUE) # 10 years

# Estimated lambda
mean(elasts$lam) # 1.16
max(elasts$lam) # 0.43
min(elasts$lam) # 0.43
lambdasDBG <- lapply(split(elasts, unlist(lapply(row.names(elasts), function(x) strsplit(x, "_")[[1]][1]))),
       function(y){
         out <- data.frame(Mu_lam = mean(y$lam), Median_lam = median(y$lam), SD_lam = sd(y$lam))
       })


# ggtern(elasts, aes(R, G, S, colour = lam))+ #, size = floor(gentim)))+ #, size = as.factor(floor(GenTime))))+ #lam))+
#   geom_point(shape = 16)+
#   scale_color_viridis_c(name = expression(lambda))+
#   scale_size_continuous()+
#   theme_showarrows()+
#   theme_clockwise()


# As a table for Gail at USFWS and Phil and Carol to compare to BLM data
surv_recruit <- do.call(rbind,lapply(split(sg, sg$Site), function(sgsite){
  years <- min(sgsite$Year):max(sgsite$Year)
  if(unique(sgsite$Site) %in% c("Fram","Pond","Pond (old)")){
    years <- c(min(sgsite$Year):2014,2016:max(sgsite$Year))
  } else {
    years <- min(sgsite$Year):max(sgsite$Year)
  }
  if(unique(sgsite$Site) %in% c("Escalante Canyon", "Picnic Site", "Powerline","Pyramid Rock")){
    years <- c(2009:max(sgsite$Year))
  }
  if(unique(sgsite$Site) %in% "Pond (old)"){
    years <- min(sgsite$Year):2013
  }
  sgMinis <- sgsite
  sgsite <- sgsite[!is.na(sgsite$Width.cm.),]
  dfout <- list() # vegetative and reproductive
  for(yr in 2:length(years)){
    repro <- nrow(sgsite[sgsite$Fl == "y" & sgsite$Year == years[yr-1],]) # Reproductive last year
    newtags <- setdiff(sgsite$Tag[sgsite$Year == years[yr]],sgsite$Tag[sgsite$Year == years[yr-1]]) # tags in yr that weren't in yr-1
    if(length(newtags)==0){
      newtags <- NA
      recruit <- 0
      recruitStage <- c(0,0)
    } else {
      recruit <- length(newtags) # how many in t1 that weren't in t0
      recruitStage <- table(sgsite$Stage[sgsite$Tag %in% newtags & sgsite$Year == years[yr]]) # new this year since last year
    }
    minis <- sum(sgMinis$Minis[sgMinis$Year == years[yr]], na.rm = TRUE)  
    fecund2Veg <- (recruitStage[2] + minis)/repro # how many vegetative individuals 
    fecund2Rep <- recruitStage[1]/repro # how many newly discovered individuals are reproductive - this should be assigned to on fertility transition
    R <- (recruit + minis)/repro
    survStage <- table(sgsite$Stage[sgsite$Tag %in% intersect(sgsite$Tag[sgsite$Year == years[yr]],sgsite$Tag[sgsite$Year == years[yr-1]]) &
                                      sgsite$Year == years[yr-1]])
    if(length(survStage) < 2) {
      survStage <- c(survStage, 0)
      names(survStage) <- c(names(survStage)[1], setdiff(c("Reproductive","Vegetative"), names(survStage)))
    }
    # How many tags grew from veg in year-1 to reproductive in year, 
    growthVegRep <- length(intersect(sgsite$Tag[sgsite$Year == years[yr] & sgsite$Stage == "Reproductive"],
                                     sgsite$Tag[sgsite$Year == years[yr-1]& sgsite$Stage == "Vegetative"]))
    G <- growthVegRep/survStage["Vegetative"]
    Staget0 <- table(sgsite$Stage[sgsite$Year == years[yr-1]]) # this is how many there were in year-1 but not all survived
    dfout[[yr-1]] <- data.frame(survVeg = survStage["Vegetative"]/Staget0["Vegetative"], 
                                growthVeg = growthVegRep/Staget0["Vegetative"],
                                survRep = survStage["Reproductive"]/Staget0["Reproductive"],
                                Recruitment2Veg =  fecund2Veg,
                                Recruitment2Rep = fecund2Rep,
                                Site = unique(sgsite$Site), 
                                Pop = unique(sgsite$Population), 
                                Year = years[yr-1],
                                row.names = years[yr-1])
  }
  out <- do.call(rbind,dfout)
  out
}))

write.table(surv_recruit, paste("C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus-glaucus/2020_Sclerocactus-glaucus_SSA/survivalFecundDBG",
                                paste(strsplit(date()," ")[[1]][c(5,2,3)],collapse = ""),
                                ".csv",
                                sep = ""),
            sep=",", row.names = FALSE)
load(file ="C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus-glaucus/2020_Sclerocactus-glaucus_SSA/BLMsurvival.Rdata")
load(file = "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus-glaucus/2020_Sclerocactus-glaucus_SSA/BLMrecruit.Rdata")
load(file = "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus-glaucus/2020_Sclerocactus-glaucus_SSA/BLMsurvXstage.Rdata")


# test
# sgsite <- split(sg, sg$Site)[[1]]
# yr <- 2
# -------------------- Survival by year DBG --------- Annual survival per plot DBG -----------------------------

DBG_surv <- do.call(rbind,lapply(split(sg, sg$Site), function(sgsite){
  years <- min(sgsite$Year):max(sgsite$Year)
  if(unique(sgsite$Site) %in% c("Fram","Pond","Pond (old)")){
    years <- c(min(sgsite$Year):2014,2016:max(sgsite$Year))
  } else {
    years <- min(sgsite$Year):max(sgsite$Year)
  }
  if(unique(sgsite$Site) %in% c("Escalante Canyon", "Picnic Site", "Powerline","Pyramid Rock")){
    years <- c(2009:max(sgsite$Year))
  }
  if(unique(sgsite$Site) %in% "Pond (old)"){
    years <- min(sgsite$Year):2013
  }
  sgMinis <- sgsite
  sgsite <- sgsite[!is.na(sgsite$Width.cm.),]
  dfout <- do.call(rbind,lapply(2:length(years), function(yr){
    repro <- nrow(sgsite[sgsite$Fl == "y" & sgsite$Year == years[yr-1],]) # Reproductive last year
    newtags <- setdiff(sgsite$Tag[sgsite$Year == years[yr]],sgsite$Tag[sgsite$Year == years[yr-1]]) # tags in yr that weren't in yr-1
    if(length(newtags)==0){
      newtags <- NA
      recruit <- 0
      recruitStage <- c(0,0)
    } else {
      recruit <- length(newtags) # how many in t1 that weren't in t0
      recruitStage <- table(sgsite$Stage[sgsite$Tag %in% newtags & sgsite$Year == years[yr]]) # new this year since last year
    }
    minis <- sum(sgMinis$Minis[sgMinis$Year == years[yr]], na.rm = TRUE)  
    # fecund2Veg <- (recruitStage[2] + minis)/repro # how many vegetative individuals 
    # fecund2Rep <- recruitStage[1]/repro # how many newly discovered individuals are reproductive - this should be assigned to on fertility transition
    # R <- (recruit + minis)/repro
    # How many tags were found in year-1 and year, survived, and what was their stage in year-1
    # survStage <- data.frame(Reproductive = 0, Vegetative = 0)
    surv1 <- nrow(sgsite[sgsite$Tag %in% intersect(sgsite$Tag[sgsite$Year == years[yr]],sgsite$Tag[sgsite$Year == years[yr-1]]) &
                                      sgsite$Year == years[yr-1],])
    individuals_t0 <- nrow(sgsite[sgsite$Width.cm. > 0 & sgsite$Year == years[yr-1],])
    surv <- length(intersect(sgsite$Tag[sgsite$Year == years[yr]],sgsite$Tag[sgsite$Year == years[yr-1]])) # the tags in t1 that were in t0
    n_t0 <- nrow(sgsite[sgsite$Year == years[yr-1],])
    data.frame(Site = unique(sgsite$Site), Pop = unique(sgsite$Population), PercSurv = surv/n_t0)
    }))
  dfout
  }))


load(file = "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus glaucus_SSA_2020/BLM_scgl_annualsurivval.Rdata")

BLM_surv
BLM_DBG_survival <- rbind(data.frame(Org = "DBG", DBG_surv[!is.na(DBG_surv$PercSurv),]), # fram and pond, missing years of data
                          data.frame(Org = "BLM", BLM_surv))

ggplot(BLM_DBG_survival, aes(Pop, PercSurv, colour = Org))+
  geom_boxplot()+
  geom_jitter()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Population")+
  ylab("Annual Percent Survival")

aggregate(PercSurv ~ Pop, mean, data = BLM_DBG_survival)

aggregate(PercSurv ~ Pop, sd, data = BLM_DBG_survival)

BLM_DBG_survival$SPP <- "ScGl"
BLM_DBG_survival$SPP[BLM_DBG_survival$Pop %in% c("Roan Creek", "Plateau Creek")] <- "ScDa"




# Add northern and southern group
# Plateau Creek Roan Creek, 
# Palisade, North Gunnison Gorge, and XXX

aggregate(PercSurv ~ Pop, mean, data = BLM_DBG_survival)
aggregate(PercSurv ~ Pop, sd, data = BLM_DBG_survival)


aggregate(PercSurv ~ SPP, mean, data = BLM_DBG_survival)
aggregate(PercSurv ~ SPP, sd, data = BLM_DBG_survival)

# -------------------------------------------------------------------------------------


## BIND surv_recruit WITH BLM recruit by stage, 2Veg -> vegRecruit
head(BLMsurxStage)
head(surv_recruit)

library(readxl)
xl_data <- "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus glaucus_SSA_2020/BLM_demographicdata_2020.11.06.xlsx"
sites <- excel_sheets(path = xl_data)

BLMcp <- read_excel(path = xl_data, sheet = sites[1])

paste("Year", 1:ncol(BLMcp))
BLM_info <- data.frame(SiteCodes = sites, YearSt = c(2011,2011,2011,2019,2013,2014,2017), 
                       Pop = c("Cactus Park","Devils Thumb","Dominguez-Escalante","Roan Creek",
                               "Devil's Thumb","Gunnison River East","Whitewater"))

BLMsurxst <- merge(BLMsurxStage, BLM_info, by.x = "sites", by.y = "SiteCodes")

surv_all <- rbind(data.frame(survVeg = surv_recruit$survVeg, survRep = surv_recruit$survRep, surv_recruit[,c("Pop","Site")]),
                  data.frame(survVeg = BLMsurxst$VegSurv, survRep = BLMsurxst$RepSurv, Pop = BLMsurxst$Pop, Site = BLMsurxst$sites))

surv_all_long <- pivot_longer(surv_all, cols = starts_with("surv"), names_to = "survival")

ggplot(surv_all_long, aes(Pop, value, colour = survival))+  
  geom_jitter()+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Population")+
  ylab("Percent Survival by stages")+
  scale_colour_discrete(name = "Stage",
                      breaks = c("survRep", "survVeg"),
                      labels = c("Reproductive","Vegetative"))

aggregate(value ~ survival  + Pop, mean, data = surv_all_long)
aggregate(value ~ survival  + Pop, sd, data = surv_all_long)



# ----------BIND recruitment to BLM
head(BLMrecruitXstage)
head(surv_recruit)

recruit_all <- rbind(data.frame(recrVeg = surv_recruit$Recruitment2Veg, recrRep = surv_recruit$Recruitment2Rep, surv_recruit[,c("Pop","Site")]),
                  data.frame(recrVeg = BLMrecruitXstage$vegRecruit, recrRep = BLMrecruitXstage$repRecruit, 
                             Pop = BLMrecruitXstage$Pop.y, Site = BLMrecruitXstage$Pop))

recruit_all_long <- pivot_longer(surv_all, cols = starts_with("recr"), names_to = "recruitment")

ggplot(recruit_all_long, aes(Pop, value, colour = recruitment))+  
  geom_boxplot()+
  geom_jitter()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Population")+
  ylab("Recruitment by stages")+
  scale_colour_discrete(name = "Stage",
                        breaks = c("recrRep", "recrVeg"),
                        labels = c("Reproductive","Vegetative"))

aggregate(value ~ recruitment  + Pop, function(x) mean(x, na.rm = TRUE), data = recruit_all_long)
aggregate(value ~ recruitment  + Pop, function(x) sd(x, na.rm = TRUE), data = recruit_all_long)



# or define small and count minis, but that gets confusing when we miss little ones. BLM doesn't know sizes 
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
table(sg$Recruitment, sg$Site)

MPM.scgl1 <- sg[,c("Site","Population","Year","Tag","Transect","Stage","Height.cm.","Width.cm.","Fl","Br","MainTag","Recruitment",
                   "UniqueTag")]
sg.stagefate <- subset(merge(MPM.scgl1, MPM.scgl1, by = c("Site","Population","Tag","Transect","MainTag","UniqueTag")), sort = FALSE, Year.x == Year.y - 1)

sg.stagefate$survived <- 1

sg.stagefate[is.na(sg.stagefate$Width.cm..y)==TRUE,]
sg.raw[sg.raw$Tag == 129.02 & !is.na(sg.raw$Tag),]

sg.stagefate$survived[is.na(sg.stagefate$Width.cm..y)==TRUE] <- 0
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

# Test
# x <- split(sg.stagefate, list(sg.stagefate$Site,sg.stagefate$Stage.x))
# sum(x[[3]]$survived)/(nrow(x[[3]]))
survival <- do.call(rbind,lapply(split(sg.stagefate, list(sg.stagefate$Site,sg.stagefate$Stage.x)), function(x){
  PercSurv <- sum(x$survived)/nrow(x)
  data.frame(Site=(x$Site[1]), Pop=(x$Population[1]),Stage = x$Stage.x[1],PercSurv)
}))

write.table(survival, "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus glaucus_SSA_2020/survival.csv",
            sep=",", row.names = FALSE)



ggplot(survival[!is.na(survival$PercSurv),], aes(Pop, PercSurv, colour = Stage))+
  geom_jitter()+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Population")+
  ylab("Percent Survival by stages")
  
survival_all <- rbind(survival)


surivalxPop <- do.call(rbind, lapply(split(sg.stagefate, sg.stagefate$Population), function(df){
  PercSurv <- sum(df$survived)/nrow(df)
  data.frame(Site=(df$Site[1]), Pop=(df$Population[1]),PercSurv)
}))

load(file ="C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus glaucus_SSA_2020/BLMsurvival.Rdata")

BLMsurvival

# BLMsurvival$Pop <- c("Cactus Park","Devils Thumb","Dominguez-Escalante","Roan Creek","Devil's Thumb","Gunnison River East","Whitewater")
# names(BLMsurvival) <- c(names(BLMsurvival)[1],"Site",names(BLMsurvival)[3])



survival <- rbind(surivalxPop, BLMsurvival[,c(2,3,1)])

ggplot(survival[!is.na(survival$PercSurv),], aes(Pop, PercSurv, colour = Pop))+
  geom_boxplot()+
  theme_bw()+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")+
  xlab("Population")+
  ylab("Percent Survival all stages")

aggregate(PercSurv ~ Pop, mean, data = survival)

aggregate(PercSurv ~ Pop, sd, data = survival)

# -----------------------------------------------------------------------------
load(file = "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus glaucus_SSA_2020/BLMrecruit.Rdata")
load(file = "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus glaucus_SSA_2020/BLMsurvXstage.Rdata")

# ------------------------ Survival by pop and stage --------------------------



# -----------------------------------------------------------------------------
# df <- split(sg.stagefate, sg.stagefate$Site)
# (nrow(df[[1]][df[[1]]$Stage.y == "Seedling",])+sum(df[[1]]$Minis.y, na.rm=TRUE))

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
})))

write.table(recruitmentXyear, "C:/Users/DePrengm/Denver Botanic Gardens/Conservation - General/AllProjectsBySpecies/Sclerocactus glaucus_SSA_2020/recruitmentXyear.csv",
            sep=",", row.names = FALSE)


ggplot(recruitmentXyear, aes( Pop,recruit))+
  geom_boxplot()




# ----------------------------- Imputing data ----------------------------------------------
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

# table(sg$save)
# head(sg[sg$save == 0,],100)
# sg[sg$Year == 2008,]
# sg <- sg[sg$save == 1,]

## add rows for missing years for each tag, make a variable to indicate if the row's size is a dependent variable and how far back to get the last size (will need to take next year for 2008)
# $lagsrtsz for the number of years back you need to go to find a measured size (will mostly just be the one year missed) ==0 for first year and for missing years
sg$lagsrtsz <- 0 # zero means no, not dependent
dats2 <- NULL # placeholder for new data

# tt <- tags[grep("Pond_4", tags)][20]
# tt <- tags[grep("Escalante Canyon_0.17_178", tags)][1]
# Need to ignore 2008 for now where we weren't measuring size
for(tt in tags){
  dds <- sg[which(sg$UniqueTag == tt),] #hold temp
  if (length(dds$Year)>1){
    for(yy in 2:length(dds$Year)){
      pastyrs <- dds$Year[1:(yy-1)] # from the first to second to last added consecutively 
      goodpastyrs <- pastyrs[is.na(dds$Width.cm.[1:(yy-1)])==FALSE]
      if(is.na(dds$Width.cm.[yy])==FALSE) dds$lagsrtsz[yy] <- min(dds$Year[yy] - goodpastyrs)
      # some missing data for all previous times
      if(is.infinite(dds$lagsrtsz[yy])) dds$lagsrtsz[yy] <- -1
    } # end yr loop
    
    # now, find and add in the missing year rows:
    allyrs <- min(dds$Year):max(dds$Year)
    yrs <- c(dds$Year)
    missingyrs <- allyrs[which(allyrs%in%yrs ==FALSE)] # Which years are missing?
    ddsmissing <- do.call('rbind',replicate(length(missingyrs),dds[1,],simplify=FALSE)) # copy the row from the first one for as many missing years
    ddsmissing$Year <- missingyrs
    ddsmissing$Height.cm. <- ddsmissing$Width.cm. <- ddsmissing$Fl <- ddsmissing$Br <- ddsmissing$PhotoNumber <- ddsmissing$Comments <- NA
    ddsmissing$lagsrtsz <- 0
    dds <- rbind(dds,ddsmissing)
    dds <- dds[order(dds$Year),] # reordered, full record for this plt
  } # end if the plt was observed more than once
  dats2=rbind(dats2,dds)
} # end going through each plt

# -1 for when the first measure/measures are missing
missingtags <- dats2$UniqueTag[is.na(dats2$Width.cm.)]
dats2[dats2$UniqueTag == "Escalante Canyon_0.17_178",] # missingtags[2],]
table(dats2$lagsrtsz)
dats2[dats2$UniqueTag == dats2$UniqueTag[is.infinite(dats2$lagsrtsz)][2],]
sg.impute <- dats2
write.csv(sg.impute, paste("C:/Users/DePrengm/OneDrive - Denver Botanic Gardens/P drive/hackathon/ScGl/sg2impute_", currentYr, ".csv",sep=""))
save(sg.impute, file = paste("C:/Users/DePrengm/OneDrive - Denver Botanic Gardens/P drive/hackathon/ScGl/sg2impute_", currentYr, ".Rdata",sep=""))

# -------------------- testing that stochastic is always less than deterministic ------------- 
# geometric < arithmetic mean
# Pretend lambdas
lmds <- rnorm(20, 1, 0.5)
# Arithmetic mean
sum(lmds)/length(lmds)
# Geometric mean
prod(lmds)^(1/length(lmds))
exp(sum(log(lmds)))^(1/length(lmds))
# --------------------------------------------------------------

currentYr <- 2019
load(paste("C:/Users/DePrengm/OneDrive - Denver Botanic Gardens/P drive/hackathon/ScGl/sg2impute_", currentYr, ".Rdata",sep=""))
head(sg.impute)

# growth of individuals vs. addition of seedlings for changes in width
ggplot(sg.impute, aes(Year, Width.cm., colour = Site))+
  geom_point()+
  stat_smooth(method = 'glm', se=FALSE)+
  theme_bw()

# ---------------------------------------- RJAGS ----------------------------------------
# Impute first, then do MPM
library(rjags)
library(lme4)
library(runjags)

# sg.impute$YearZscore <- (sg.impute$Year-mean(sg.impute$Year))/sd(sg.impute$Year)

# model1_negb <- lmer(Width.cm. ~ YearZscore + (1|Site), data = sg.impute)
# summary(model1_negb)

model1_lnorm <- lmer(Width.cm. ~ Year + (1|Site), data = sg.impute)
summary(model1_lnorm)

ggplot(sg.impute, aes(Year, Width.cm., colour = Site))+
  geom_jitter(width = 0.05, height = 0)+
  stat_smooth(method = lm)

# sg.impute2 <- sg.impute[!is.na(sg.impute$Width.cm.),c("Year","Site","Width.cm.")]

# removed all rows with no width reported, just estimate, then worry about imputing
jags.mm.simple <- template.jags(Width.cm. ~ Year + (1|Site), data = sg.impute, 
                                file = "C:/Users/DePrengm/OneDrive - Denver Botanic Gardens/P drive/hackathon/ScGl/jags.sg.simple.r"
                                ,n.chains=3, family = 'gaussian') # for counts with integers: family='negative binomial')

params <- c("resid.sum.sq","Year_coefficient","Site_randomeffect","regression_precision",
            "intercept", "Site_precision", "deviance")

runjagsdata <- sg.impute[,c("Year","Site","Width.cm.")]

jags.sg.simpleout <- run.jags("C:/Users/DePrengm/OneDrive - Denver Botanic Gardens/P drive/hackathon/ScGl/jags.sg.lognorm.r") 
jags.sg.simpleout

sg.mcmc <- as.mcmc(jags.sg.simpleout)
plot(sg.mcmc)

# ---------------- impute -----------------------------
jags.mm.model1 <- template.jags(Width.cm. ~ YearZscore + (1|Site), data=sg.impute, 
                                file="C:/Users/DePrengm/OneDrive - Denver Botanic Gardens/P drive/hackathon/ScGl/jags.sg.model_impute.r"
                                ,n.chains=3, family = 'gaussian')# for counts with integers: family='negative binomial')

#which rows are good dependent values (ending sizes) 
goodrows <- which(sg.impute$lagsrtsz >0)
# what is the lag for these rows: how far back is the last good size measurement? 
lagvals <- sg.impute$lagsrtsz[goodrows]
Ncases <- length(goodrows)
table(lagvals)

# run.jags will take values from the global environment
# jags.sg.impute <- with(sg.impute, list(Width.cm. = Width.cm., YearZscore = YearZscore, Site = Site,
# N = nrow(sg.impute),
# goodrows = goodrows, Ncases = Ncases, lagvals = lagvals))



jag.mm3out <- run.jags("C:/Users/DePrengm/OneDrive - Denver Botanic Gardens/P drive/hackathon/ScGl/jags.sg.model2.r", 
                       data=sg.impute[,c("YearZscore","Site","Width.cm.","lagsrtsz")])
jag.mm3out
