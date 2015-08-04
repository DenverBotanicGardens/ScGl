# For Alison Graff of BioLogic: Population biology isn’t my forte, so maybe you can help me here. 
# I’m not sure I can compare lambda to survivorship rates for the transplants – I hadn’t thought 
# about this in detail before emailing you and Gina. Since what I’m dealing with is a population 
# that can only decline whereas you were gathering data on new recruits as well as deaths. 
# Your thoughts? I’m thinking the data that would be good for me would be the number of 
# individuals in Year 1 and their fates over time, with no recruitment data. 

rawscgl <- read.csv("Q:/Research/All_Projects_by_Species/Sclerocactus SPECIES/Sclerocactus_glaucus/R_code_ScGl/R_tables/2014_scgl_rawdata.csv")
head(rawscgl)

subset(rawscgl, Tag == 185)
rawscgl[is.na(rawscgl$ScGl_data_id),]

# Add a volume column for individuals
rawscgl$Vol <- pi*(rawscgl$Width.cm.^2)*rawscgl$Height.cm.

head(rawscgl)

# No size data was taken in 2008 so remove
scgl.size <- subset(rawscgl, Year != 2008)

ggplot(scgl.size, aes(Fl, Vol)) +
  geom_boxplot() +
  facet_grid(.~Site)

# remove rows with missing data
names(scgl.size)
nrow(scgl.size[complete.cases(scgl.size[,c(1:5,10:11)]),])
nrow(scgl.size)

scgl.size <- scgl.size[complete.cases(scgl.size[,c(1:5,10:11)]),]

ggplot(scgl.size, aes(Fl, Vol)) +
  geom_boxplot()

ggplot(scgl.size, aes())

# add stages or size class ...
scgl.size$stage <- 