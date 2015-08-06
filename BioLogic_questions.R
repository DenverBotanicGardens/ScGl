# For Alison Graff of BioLogic: Population biology isn’t my forte, so maybe you can help me here. 
# I’m not sure I can compare lambda to survivorship rates for the transplants – I hadn’t thought 
# about this in detail before emailing you and Gina. Since what I’m dealing with is a population 
# that can only decline whereas you were gathering data on new recruits as well as deaths. 
# Your thoughts? I’m thinking the data that would be good for me would be the number of 
# individuals in Year 1 and their fates over time, with no recruitment data. 

library(ggplot2)

rawscgl <- read.csv("Q:/Research/All_Projects_by_Species/Sclerocactus SPECIES/Sclerocactus_glaucus/R_code_ScGl/R_tables/2014_scgl_rawdata.csv")
head(rawscgl)

subset(rawscgl, Tag == 185)
rawscgl[is.na(rawscgl$ScGl_data_id),]

# Add a volume column for individuals
rawscgl$Vol <- pi*(rawscgl$Width.cm.^2)*rawscgl$Height.cm.

# Add a log volume column
rawscgl$logVol <- log(rawscgl$Vol)

head(rawscgl)

# No size data was taken in 2008 so remove
scgl.size <- subset(rawscgl, Year != 2008)

ggplot(scgl.size, aes(Fl, Vol)) +
  geom_boxplot() +
  facet_grid(.~Site)

# cut off sizes as flowering or not and less than 0 for logVol?
ggplot(scgl.size, aes(Fl, logVol)) +
  geom_boxplot() #+
#  facet_grid(.~Site)

# remove rows with missing data
names(scgl.size)
nrow(scgl.size[complete.cases(scgl.size[,c(1:5,10:11)]),])
nrow(scgl.size)


scgl.size <- scgl.size[complete.cases(scgl.size[,c(1:5,10:11)]),]

ggplot(scgl.size, aes(Fl, logVol)) +
  geom_boxplot()

scgl.size$status[scgl.size$Fl == "y"] <- "reproductive"
scgl.size$status[scgl.size$Fl == 'n'] <- "vegetative"
scgl.size$status[scgl.size$logVol < 0] <- "mini" 
scgl.size$status[scgl.size$Height.cm. == 0] <- "dead"

stages <- c("mini","vegetative","reproductive","dead")
scgl.size$status <- ordered(scgl.size$status, levels = stages)

# average change in size from year to year
scgl.size <- subset(merge(scgl.size, scgl.size, by = c("Site","Tag")), sort = FALSE, Year.x == Year.y - 1)
names(scgl.size)
sg <- scgl.size[,c(1:2,4,10:14,19:21,23,29:33,38:40)]
names(sg)
names(sg) <- c("Site","Tag","YRt","HTt","WTHt","FLt","BRt","MNt","VOLt","logVOLt",
               "stage","YRt1","HTt1",
               "WTHt1","FLt1","BRt1","MNt1","VOLt1","logVOLt1","fate")

ggplot(sg, aes(HTt,HTt1, colour = Site)) +
  geom_point() +
#  facet_wrap(~Site) +
  geom_smooth(method="lm", se=FALSE)


ggplot(sg, aes(VOLt,VOLt1, colour = Site)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)


ggplot(sg, aes(VOLt,VOLt1, colour = stage)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

ggplot(subset(sg, !is.infinite(logVOLt) & !is.infinite(logVOLt1)), aes(logVOLt,logVOLt1, colour = Site)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

ggplot(sg, aes(x = log(VOLt)))+
  geom_histogram(binwidth = log(pi), colour = "black", fill = "white")
  

summary(lm(data=sg, VOLt1~VOLt))

# slope is 0.87 


# add stages or size class or age? They grow nearly a cm a year... 
# a year of growth would be estimated as pi
yrgr <- pi*(0.87^2)*0.87
big <- max(sg$VOLt, na.rm = TRUE)
min(sg$VOLt, na.rm = TRUE)
big/yrgr

head(sg)

#Check if suvival is related to stage or size, logisitc regression of survival against size
sg$survive <- 1
sg$survive[sg$fate == "dead"] <- 0

subset(sg, stage == "dead")

table(sg$stage, sg$survive)
table(sg$stage)
sg <- subset(sg, stage != "dead")
table(sg$stage)
table(sg$fate)
table(sg$stage, sg$survive)
sg$logVOLt[is.infinite(sg$logVOLt)] <- 0


summary(glm(survive ~ logVOLt, family = "binomial", data = sg))

for(i in 2009:2013){
  print(i)
  print(summary(glm(survive ~ logVOLt, subset = YRt == i, family = "binomial", data = sg)))
}

ggplot(sg, aes(as.factor(survive), logVOLt)) +
  geom_boxplot() +
  facet_wrap(~stage)

#Log-linear model of survival vs. stage
summary(glm(survive ~ stage, family = poisson, data = sg))





#####################################################################
# Set age ranges assuming they will grow about a cm height and width a year
# No size data was taken in 2008 so remove
scgl.size <- subset(rawscgl, Year != 2008)


# cut off sizes as flowering or not and less than 0 for logVol?
ggplot(scgl.size, aes(Fl, logVol)) +
  geom_boxplot() #+
#  facet_grid(.~Site)

# remove rows with missing data
names(scgl.size)
nrow(scgl.size[complete.cases(scgl.size[,c(1:5,10:11)]),])
nrow(scgl.size)


scgl.size <- scgl.size[complete.cases(scgl.size[,c(1:5,10:11)]),]

ggplot(scgl.size, aes(Fl, logVol)) +
  geom_boxplot()

ggplot(sg, aes(x = log(VOLt)))+
  geom_histogram(binwidth = log(pi), colour = "black", fill = "white")

biggest.cut <- log(pi)*6
bigger.cut <- log(pi)*5
big.cut <- log(pi)*4
large.cut <- log(pi)*3
med.cut <- log(pi)
small.cut <- log(1)

exp(small.cut)
exp(med.cut)
exp(large.cut)
exp(big.cut)
exp(bigger.cut)

# Split into more equal parts young, med, bigger, biggest...
scgl.size$status <- "biggest"
scgl.size$status[scgl.size$logVol < biggest.cut] <- "bigger"
scgl.size$status[scgl.size$logVol < bigger.cut] <- "big"
scgl.size$status[scgl.size$logVol < large.cut] <- "large"
scgl.size$status[scgl.size$logVol < med.cut] <- "med"
scgl.size$status[scgl.size$logVol < 0] <- "mini" 
scgl.size$status[scgl.size$Height.cm. == 0] <- "dead"

stages <- c("mini","med","large","big","bigger","biggest","dead")
scgl.size$status <- ordered(scgl.size$status, levels = stages)

# average change in size from year to year
scgl.size <- subset(merge(scgl.size, scgl.size, by = c("Site","Tag")), sort = FALSE, Year.x == Year.y - 1)
names(scgl.size)
sg <- scgl.size[,c(1:2,4,10:14,19:21,23,29:33,38:40)]
names(sg)
names(sg) <- c("Site","Tag","YRt","HTt","WTHt","FLt","BRt","MNt","VOLt","logVOLt",
               "stage","YRt1","HTt1",
               "WTHt1","FLt1","BRt1","MNt1","VOLt1","logVOLt1","fate")

sg$survive <- 1
sg$survive[sg$fate == "dead"] <- 0

subset(sg, stage == "dead")

table(sg$stage, sg$survive)
table(sg$stage)
sg <- subset(sg, stage != "dead")
table(sg$stage)
table(sg$fate)
table(sg$stage, sg$survive)
sg$logVOLt[is.infinite(sg$logVOLt)] <- 0

#Log-linear model of survival vs. stage
scgl.glm <- glm(survive ~ stage, family = poisson, data = sg)

# with random variable as the site?? No, won't converge
library(lme4)
scgl.glm <- glmer(survive ~ stage + (stage - 1 | Site), family = poisson, data = sg)

loglin <- data.frame(table(sg$stage, sg$survive))
glm(Var2 ~ Var1, offset = log(Freq), family = poisson, data = subset(loglin, Var1 != "dead"))

# likelihood raio test 
anova(scgl.glm, test = "Chisq")

# Check oversipersion
summary(scgl.glm)$dispersion

sites <- unique(sg$Site)
sur <- c()
for(s in sites){
  sur <- rbind(sur,data.frame(table(sg$stage[sg$Site == s], sg$survive[sg$Site == s]),Site = s))
} 

# Different sizes do not have significantly different suvival rates
ggplot(surv.table, aes(as.factor(Var1), Freq, fill = Var2)) +
  geom_bar(stat="identity")

# How many survive despite stage/age by site
table(sg$Site, sg$survive)


