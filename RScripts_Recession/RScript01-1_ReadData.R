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
RDataPath <- '~/Project_Recession/RData/'
Filename.Header <- paste('~/RScripts/HeaderFile_lmcg.R', sep='')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Recession.R', sep=''))
########################################################################
Today <- Sys.Date()

## Load stata dataset
Filename.dta <- paste(DataPath, 'sipp08_longitudinal.dta', sep='')
Data <- read.dta13(file = Filename.dta)
Filename.dta <- paste(DataPath, '2008_material_hardship.dta', sep='')
Data_mathard <- read.dta13(file = Filename.dta)
Filename.dta <- paste(DataPath, '2008_W6_topical_disability_variables.dta', sep='')
Data_disab <- read.dta13(file = Filename.dta)

## Merge the disability information
Disab <- unique(Data_disab[,c('ssuid', 'adult_disb')])
Data <- merge(Data, Disab, by = 'ssuid')

## Check for wave 16
Wave16 <- unique(Data[,c('ssuid', 'swave', 'ehrefper')])
Wave16 <- subset(Wave16, swave == 16)

Data16 <- merge(Data, Wave16[,c('ssuid', 'ehrefper')], by = c('ssuid', 'ehrefper'))

## Save it as RData dataset
Filename.rdata <- paste(DataPath, 'sipp08_longitudinal.RData', sep='')
save(Data, file = Filename.rdata)

## Save it as RData dataset
Filename.rdata <- paste(DataPath, 'Data16.RData', sep='')
save(Data16, file = Filename.rdata)

Filename.rdata <- paste(DataPath, '2008_material_hardship.RData', sep='')
save(Data_mathard, file = Filename.rdata)

Filename.rdata <- paste(DataPath, '2008_W6_topical_disability_variables.RData', sep='')
save(Data_disab, file = Filename.rdata)

                
