rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )
#dev.off( )

########################################################################
## This script generates the demographics table for the manuscript
########################################################################
loadData <- function( filenameModelData = 'Data_forIncPovModel_v5.RData' ){
  filenameData <- paste0( RDataPath, filenameModelData )
  load( filenameData )
  return( Data_forIncPovModel )
}

getWaveSixDataEdu <- function( waveSixDataQ2, waveSixDataQ3, Data ){
  waveSixDisab <- getWaveSixData_Disab( waveSixDataQ2, waveSixDataQ3, Data )
  
  hhIDsAll <- unique( Data[, c( 'hhid', 'education' ) ] )
  hhIDsQ2 <- unique( waveSixDataQ2[, c( 'hhid', 'education' ) ] )
  hhIDsQ3 <- unique( waveSixDataQ3[, c( 'hhid', 'education' ) ] )
  
  hhIDsQ2Q3 <- unique( rbind( hhIDsQ2, hhIDsQ3 ) )
  hhIDsQ2Q3$eduNum <- as.numeric( hhIDsQ2Q3$education )
  
  waveSixEdu <- aggregate( eduNum ~ hhid, data = hhIDsQ2Q3, FUN = max )
  waveSixEdu <- merge( x = waveSixEdu, y = hhIDsQ2Q3, by = c( 'hhid', 'eduNum' ), all = F )
  
  waveSixEduDisab <- merge( x = waveSixEdu, y = waveSixDisab, by = 'hhid' )
  
  return( waveSixEduDisab )
}

getWaveSixDataRace <- function( waveSixDataQ2, waveSixDataQ3, Data ){
  waveSixDisab <- getWaveSixData_Disab( waveSixDataQ2, waveSixDataQ3, Data )
  
  hhIDsAll <- unique( Data[, c( 'hhid', 'race_origin' ) ] )
  hhIDsQ2 <- unique( waveSixDataQ2[, c( 'hhid', 'race_origin' ) ] )
  hhIDsQ3 <- unique( waveSixDataQ3[, c( 'hhid', 'race_origin' ) ] )
  
  hhIDsQ2Q3 <- unique( rbind( hhIDsQ2, hhIDsQ3 ) )
  hhIDsQ2Q3$raceNum <- as.numeric( hhIDsQ2Q3$race_origin )
  
  waveSixRace <- aggregate( raceNum ~ hhid, data = hhIDsQ2Q3, FUN = max )
  waveSixRace <- merge( x = waveSixRace, y = hhIDsQ2Q3, by = c( 'hhid', 'raceNum' ), all = F )
  
  waveSixRaceDisab <- merge( x = waveSixRace, y = waveSixDisab, by = 'hhid' )
  
  return( waveSixRaceDisab )
}

getWaveSixDataMS <- function( waveSixDataQ2, waveSixDataQ3, Data ){
  waveSixDisab <- getWaveSixData_Disab( waveSixDataQ2, waveSixDataQ3, Data )
  
  hhIDsAll <- unique( Data[, c( 'hhid', 'ms' ) ] )
  hhIDsQ2 <- unique( waveSixDataQ2[, c( 'hhid', 'ms' ) ] )
  hhIDsQ3 <- unique( waveSixDataQ3[, c( 'hhid', 'ms' ) ] )
  
  hhIDsQ2Q3 <- unique( rbind( hhIDsQ2, hhIDsQ3 ) )
  hhIDsQ2Q3$married <- hhIDsQ2Q3$ms == 'Married'
  
  waveSixMarried <- aggregate( married ~ hhid, data = hhIDsQ2Q3, FUN = max )
  
  waveSixMarriedDisab <- merge( x = waveSixMarried, y = waveSixDisab, by = 'hhid' )
  
  return( waveSixMarriedDisab )
}

getWaveSixDataGender <- function( waveSixDataQ2, waveSixDataQ3, Data ){
  waveSixDisab <- getWaveSixData_Disab( waveSixDataQ2, waveSixDataQ3, Data )
  
  hhIDsAll <- unique( Data[, c( 'hhid', 'gender' ) ] )
  hhIDsQ2 <- unique( waveSixDataQ2[, c( 'hhid', 'gender', 'whfnwgt_qtr' ) ] )
  hhIDsQ3 <- unique( waveSixDataQ3[, c( 'hhid', 'gender', 'whfnwgt_qtr' ) ] )
  
  hhIDsQ2Q3 <- unique( rbind( hhIDsQ2, hhIDsQ3 ) )
  hhIDsQ2Q3$male <- hhIDsQ2Q3$gender == 'Male'

  waveSixMale <- aggregate( male ~ hhid, data = hhIDsQ2Q3, FUN = max )

  waveSixGenderDisab <- merge( x = waveSixMale, y = waveSixDisab, by = 'hhid' )

  return( waveSixGenderDisab )
}

getWaveSixData_Disab <- function( waveSixDataQ2, waveSixDataQ3, Data ){
  hhIDsAll <- unique( Data[, c( 'hhid', 'adult_disb' ) ] )
  hhIDsQ2 <- unique( waveSixDataQ2[, c( 'hhid', 'adult_disb', 'whfnwgt_qtr' ) ] )
  hhIDsQ3 <- unique( waveSixDataQ3[, c( 'hhid', 'adult_disb', 'whfnwgt_qtr' ) ] )
  
  hhIDsQ2Q3 <- unique( rbind( hhIDsQ2, hhIDsQ3 ) )
  hhIDsQ2Q3_Avg <- aggregate( whfnwgt_qtr ~ hhid, data = hhIDsQ2Q3, FUN = mean )
  hhIDsQ2Q3_Avg <- merge( x = hhIDsQ2Q3_Avg, y = hhIDsAll, by = 'hhid', all = F )
  hhIDsQ2Q3_Avg$disab <- ( hhIDsQ2Q3_Avg$adult_disb == "yes" )
  
  return( hhIDsQ2Q3_Avg )
}

########################################################################
## Run Path definition file                                           ##
########################################################################
PathPrefix <- '~/'
# PathPrefix <- '/Users/patron/Documents/snandi/'
RScriptPath <- paste0( PathPrefix, 'Project_Recession/RScripts_Recession/' )
DataPath <- paste0( PathPrefix, 'Project_Recession/Data/data_2015Dec/' )
RDataPath <- paste0( PathPrefix, 'Project_Recession/RData/data_2015Dec/' )
PlotPath <- paste0( PathPrefix, 'Project_Recession/Plots/' )
Filename.Header <- paste0( RScriptPath, 'HeaderFile_Recession.R' )
source( Filename.Header )

source( paste( RScriptPath, 'fn_Library_Recession.R', sep='' ) )
source( paste( RScriptPath, 'plotLSMeans.R', sep='' ) )

Today <- Sys.Date( )

########################################################################
## Load Data
########################################################################
Data <- loadData( filenameModelData = 'Data_forIncPovModel_v5.RData' )
waveSixDataQ2 <- subset( Data, yearqtr == '2010 Q2' )
waveSixDataQ3 <- subset( Data, yearqtr == '2010 Q3' )

########################################################################
## Total households, with & without disability
########################################################################
hhIDs_Disab <- getWaveSixData_Disab( waveSixDataQ2, waveSixDataQ3, Data )
N_Sample <- nrow( hhIDs_Disab )
N_Disab <- round( nrow( hhIDs_Disab ) * 
  weighted.mean( x = as.numeric( hhIDs_Disab$disab ), w = hhIDs_Disab$whfnwgt_qtr ), 0 )

N_Disab
N_NotDisab <- N_Sample - N_Disab

# View( subset( Data, hhid == "019128038099_11" ) )

########################################################################
## Gender
########################################################################
waveSixGenderDisab <- getWaveSixDataGender( waveSixDataQ2, waveSixDataQ3, Data )
head( waveSixGenderDisab )

TableGenderDisab <- questionr::wtd.table( x = waveSixGenderDisab$male, y = waveSixGenderDisab$disab, 
                                          weights = waveSixGenderDisab$whfnwgt_qtr )
TableGenderDisab <- round( N_Sample * TableGenderDisab / sum( TableGenderDisab ) )

TableGenderDisab[, 'TRUE']/N_Disab
TableGenderDisab[, 'FALSE']/N_NotDisab

########################################################################
## Marital Status
########################################################################
waveSixMarriedDisab <- getWaveSixDataMS( waveSixDataQ2, waveSixDataQ3, Data )
head( waveSixMarriedDisab )

TableMSDisab <- questionr::wtd.table( x = waveSixMarriedDisab$married, y = waveSixMarriedDisab$disab, 
                                      weights = waveSixMarriedDisab$whfnwgt_qtr )
TableMSDisab <- round( N_Sample * TableMSDisab / sum( TableMSDisab ) )
TableMSDisab[, 'TRUE']/N_Disab
TableMSDisab[, 'FALSE']/N_NotDisab

########################################################################
## Race
########################################################################
waveSixRaceDisab <- getWaveSixDataRace( waveSixDataQ2, waveSixDataQ3, Data )
head( waveSixRaceDisab )

TableRaceDisab <- questionr::wtd.table( x = waveSixRaceDisab$race_origin, y = waveSixRaceDisab$disab, 
                                      weights = waveSixRaceDisab$whfnwgt_qtr )
TableRaceDisab <- round( N_Sample * TableRaceDisab / sum( TableRaceDisab ) )
round( TableRaceDisab[, 'TRUE']/N_Disab, 4 )
round( TableRaceDisab[, 'FALSE']/N_NotDisab, 4 )

########################################################################
## Education
########################################################################
waveSixEduDisab <- getWaveSixDataEdu( waveSixDataQ2, waveSixDataQ3, Data )
head( waveSixEduDisab )

TableEduDisab <- questionr::wtd.table( x = waveSixEduDisab$education, y = waveSixEduDisab$disab, 
                                        weights = waveSixEduDisab$whfnwgt_qtr )
TableEduDisab <- round( N_Sample * TableEduDisab / sum( TableEduDisab ) )
round( TableEduDisab[, 'TRUE']/N_Disab, 4 )
round( TableEduDisab[, 'FALSE']/N_NotDisab, 4 )

########################################################################
## How many quarters did Households participate in?
########################################################################
numHH_Quarters <- aggregate( hhid ~ yearqtr, data = Data, FUN = length )
hhIDs_Quarters <- aggregate( yearqtr ~ hhid, data = Data, FUN = length )

hhIDs_2013Q2 <- unique( subset( Data, yearqtr == "2013 Q2" )[, 'hhid'] )



