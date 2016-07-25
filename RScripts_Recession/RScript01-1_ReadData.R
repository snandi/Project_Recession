rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script reads in and does basic understanding of the recession ##
## dataset
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
RScriptPath <- '~/Project_Recession/RScripts_Recession/'
DataPath <- '~/Project_Recession/Data/data_2015Dec/'
RDataPath <- '~/Project_Recession/RData/data_2015Dec/'
Filename.Header <- paste('~/RScripts/HeaderFile_lmcg.R', sep='')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Recession.R', sep=''))
########################################################################
Today <- Sys.Date()

## ## Load stata dataset
Filename.dta <- paste(DataPath, 'sipp08_longitudinal.dta', sep='')
Data <- read.dta13(file = Filename.dta)
Filename.dta <- paste(DataPath, '2008_material_hardship.dta', sep='')
Data_mathard <- read.dta13(file = Filename.dta)
Filename.dta <- paste(DataPath, '2008_W6_topical_disability_variables.dta', sep='')
Data_disab <- read.dta13(file = Filename.dta)

length(unique(Data$ssuid))
length(unique(Data_disab$ssuid))

## Merge the disability information
Disab <- aggregate(adult_disb ~ ssuid + shhadid, data = Data_disab, FUN = max)
#Data <- merge(Data, Disab, by = c('ssuid', 'shhadid'))
Data <- merge(Data, Disab, by = c('ssuid'))

## Check for wave 15
SplitData <- split(x = Data, f = as.factor(Data$ssuid))
## Data15 <- do.call(what = rbind, sapply(X = SplitData, FUN = fn_keepWave15ehref))
Data15 <- ldply(.data = SplitData, .fun = fn_keepWave15ehref)
length(unique(Data15$ssuid))

## Keep only up to wave 15
Data15 <- subset(Data15, swave < 16)
length(unique(Data15$ssuid))

## Check for age to be above 18
nrow(subset(Data15, tage < 18))

## Save it as RData dataset
Filename.rdata <- paste(RDataPath, 'sipp08_longitudinal.RData', sep='')
save(Data, file = Filename.rdata)

## Save it as RData dataset
Filename.rdata <- paste(RDataPath, 'Data15.RData', sep='')
save(Data15, file = Filename.rdata)

Filename.rdata <- paste(RDataPath, '2008_material_hardship.RData', sep='')
save(Data_mathard, file = Filename.rdata)

Filename.rdata <- paste(RDataPath, '2008_W6_topical_disability_variables.RData', sep='')
save(Data_disab, file = Filename.rdata)

                
