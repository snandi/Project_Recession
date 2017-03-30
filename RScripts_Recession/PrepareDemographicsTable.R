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
waveSixData <- subset( Data, yearqtr == '2010 Q2' )
waveSixDataQ3 <- subset( Data, yearqtr == '2010 Q3' )

########################################################################
## Total households, with & without disability
########################################################################
hhIDsAll <- unique( Data[, c( 'hhid', 'adult_disb' ) ] )
hhIDsQ2 <- unique( waveSixData[, c( 'hhid', 'adult_disb' ) ] )
hhIDsQ3 <- unique( waveSixDataQ3[, c( 'hhid', 'adult_disb' ) ] )

hhIDsAll$hhid %w/o% hhIDsQ3$hhid

totalHH <- length( unique( hhIDsQ2$hhid, hhIDsQ3$hhid ) )
table( hhIDs$adult_disb )
table( hhIDs$adult_disb )/totalHH

View( subset( Data, hhid == "019128038334_11" ) )

########################################################################
## Gender
########################################################################
hhIDs_Gender <- unique( waveSixData[, c( 'hhid', 'adult_disb', 'gender' ) ] )
table( hhIDs_Gender$gender, hhIDs_Gender$adult_disb )

########################################################################
## How many quarters did Households participate in?
########################################################################
numHH_Quarters <- aggregate( hhid ~ yearqtr, data = Data, FUN = length )
hhIDs_Quarters <- aggregate( yearqtr ~ hhid, data = Data, FUN = length )

hhIDs_2013Q2 <- unique( subset( Data, yearqtr == "2013 Q2" )[, 'hhid'] )



