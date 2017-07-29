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
dataWeights <- aggregate( wffinwgt ~ hhid + swave + epppnum, data = dataMasterWeights, FUN = mean )

########################################################################
## Merge data with weights
########################################################################
dataMatHard$epppnum <- as.numeric( dataMatHard$epppnum )
dataMatHardWts <- merge( x = dataMatHard, y = dataWeights, by = c( 'hhid', 'swave', 'epppnum' ), 
                         all.x = F, all.y = F )

########################################################################
## Merge data with disab
########################################################################
