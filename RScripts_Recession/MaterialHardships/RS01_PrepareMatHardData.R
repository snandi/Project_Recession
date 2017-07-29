rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )
#dev.off()

########################################################################
## Read in material hardships data and merge it with disability
########################################################################

########################################################################
## Run Path definition file                                           
########################################################################
RScriptPath <- '~/Project_Recession/RScripts_Recession/'
DataPath <- '~/Project_Recession/Data/data_2015Dec/'
RDataPath <- '~/Project_Recession/RData/data_2015Dec/'
Filename.Header <- paste0( RScriptPath, 'HeaderFile_Recession.R', sep = '' )
source( Filename.Header )
source( paste( RScriptPath, 'fn_Library_Recession.R', sep = '' ) )
library( readstata13 )
########################################################################
Today <- Sys.Date()

########################################################################
## Read stata file                                           
########################################################################
filenameMatHard <- paste( DataPath, 'sipp08_material_hardships.dta', sep = '' )
dataMatHard <- readstata13::read.dta13( file = filenameMatHard )

filenameDisab <- paste( RDataPath, '2008_W6_topical_disability_variables.RData', sep = '' )
load( filenameDisab )
dataDisab <- Data_disab
rm( Data_disab )

########################################################################
## create a household id
########################################################################
dataMatHard$hhid <- paste( dataMatHard$ssuid, dataMatHard$shhadid, sep = '_' )

########################################################################
## Get household weights from Master Inc Pov Data
########################################################################
filenameData15 <- paste( RDataPath, 'Data15.RData', sep = '' )
load( filenameData15 )
Data15$hhid <- paste( Data15$ssuid, Data15$shhadid, sep = '_' )

colnamesKeep <- c( 'hhid', 'swave', 'ehrefper', 'epppnum', 'wffinwgt' )
## Get weights
dataMasterWeights <- unique( Data15[, colnamesKeep ] ) 

## Keep weights of wave 6 and 9 only
dataMasterWeights <- subset( dataMasterWeights, swave %in% c( 6,9 ) )

## Keep weights of ehrefper only
dataMasterWeights <- subset( dataMasterWeights, ehrefper == epppnum ) ## No change. Good!

## Average weights of the same wave per household
dataWeights <- aggregate( wffinwgt ~ hhid + swave, data = dataMasterWeights, FUN = mean )

########################################################################
## Merge data with weights
########################################################################
dataMatHard$epppnum <- as.numeric( dataMatHard$epppnum )
dataMatHard$epppnum <- NULL
dataMatHard$tage <- NULL
dataMatHard$sippid <- NULL
dataMatHard <- unique( dataMatHard )
dataMatHardWts <- merge( x = dataMatHard, y = dataWeights, by = c( 'hhid', 'swave' ), 
                         all.x = F, all.y = F )

########################################################################
## Merge data with disab
########################################################################
dataDisab$hhid <- paste( dataDisab$ssuid, dataDisab$shhadid, sep = '_' )
dataDisab <- unique( dataDisab[,c( 'hhid', 'adult_disb' ) ] )

dataMatHardWtsDisab <- merge( x = dataMatHardWts, y = dataDisab, by = 'hhid', all.x = T, all.y = F )

table( dataMatHardWtsDisab$adult_disb)

########################################################################
## Keep only those households who were there in wave 6
########################################################################
wave6HH <- unique( unique( dataMatHardWtsDisab$hhid[ dataMatHardWtsDisab$swave == 6 ] ) )
dataMatHardWtsDisab <- subset( dataMatHardWtsDisab, hhid %in% wave6HH )

########################################################################
## Ensure all households are both in waves 6 and 9
########################################################################
tableHH <- table( dataMatHardWtsDisab$hhid )
length( tableHH[ tableHH == 1 ] )
HH_notInWave9 <- names( which( tableHH == 1 ) )

dataMatHardWtsDisab <- subset( dataMatHardWtsDisab, !( hhid %in% HH_notInWave9 ) )

tableHH <- table( dataMatHardWtsDisab$hhid )
tableHH[ tableHH > 2 ] ## There shouldn't be any household with more than 2 data points

########################################################################
## Save material hardships data for subsequent analysis
########################################################################
filenameMatHardFinal <- paste0( RDataPath, 'MaterialHardshipsData.RData' )
save( dataMatHardWtsDisab, file = filenameMatHardFinal )
