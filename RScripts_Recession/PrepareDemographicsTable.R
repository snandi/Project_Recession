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

getWaveSixData_Gender <- function( waveSixDataQ2, waveSixDataQ3, Data ){
  hhIDsAll <- unique( Data[, c( 'hhid', 'gender' ) ] )
  hhIDsQ2 <- unique( waveSixDataQ2[, c( 'hhid', 'gender', 'whfnwgt_qtr' ) ] )
  hhIDsQ3 <- unique( waveSixDataQ3[, c( 'hhid', 'gender', 'whfnwgt_qtr' ) ] )
  
  hhIDsQ2Q3 <- unique( rbind( hhIDsQ2, hhIDsQ3 ) )
  TempWt <- aggregate( whfnwgt_qtr ~ hhid, data = hhIDsQ2Q3, FUN = mean )

  hhIDsQ2Q3$male <- hhIDsQ2Q3$gender == 'Male'
  TempMale <- aggregate( male ~ hhid, data = hhIDsQ2Q3, FUN = max )
  

  return( hhIDsQ2Q3_Avg )
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
N_Disab <- round( nrow( hhIDs_Disab ) * 
  weighted.mean( x = as.numeric( hhIDs_Disab$disab ), w = hhIDs_Disab$whfnwgt_qtr ), 0 )

N_Disab
nrow( hhIDs_Disab ) - N_Disab

# View( subset( Data, hhid == "019128038099_11" ) )

########################################################################
## Gender
########################################################################
hhIDs_Gender <- unique( merge( x = hhIDs_Disab, y = Data[, c( 'hhid', 'gender' ) ], by = 'hhid' ) )

?svytable


table( hhIDs_Gender$gender, hhIDs_Gender$adult_disb )

########################################################################
## How many quarters did Households participate in?
########################################################################
numHH_Quarters <- aggregate( hhid ~ yearqtr, data = Data, FUN = length )
hhIDs_Quarters <- aggregate( yearqtr ~ hhid, data = Data, FUN = length )

hhIDs_2013Q2 <- unique( subset( Data, yearqtr == "2013 Q2" )[, 'hhid'] )



