rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )
#dev.off( )

########################################################################
## This script formats the main regression output table
########################################################################
loadModel <- function( modelFilename ){
  modelFilepath <- paste0( RDataPath, modelFilename )
  load( modelFilepath )
  return( modelData )
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
library( stargazer )

Today <- Sys.Date( )

modelFPL100 <- loadModel( modelFilename = 'modelFPL100.RData' )

stargazer::stargazer( modelFPL100, type = 'latex' )

coef( modelFPL100 )
summary( modelFPL100 )

