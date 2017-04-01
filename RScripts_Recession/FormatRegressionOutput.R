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

modelFPL100_Summary <- loadModel( modelFilename = 'modelFPL100_Summary.RData' )

Coeff_Fixed <- as.data.frame( modelFPL100_Summary$coefficients[, c( 1, 2, 5 )] )
colnames( Coeff_Fixed ) <- c( 'beta', 'stdError', 'pValue' )
Coeff_Fixed <- within( data = Coeff_Fixed, {
  beta = round( beta, 3 )
  stdError = round( stdError, 3 )
  pValue = round( pValue, 4 )
})
rownames( Coeff_Fixed ) <- c(
  'Intercept', 
  'Time', 
  'Time2', 
  'Adult Disability', 
  'Gender: (Female)',
  'Marital status: (Not married)',
  'Race2: (Black)', 
  'Race3: (Hispanic)', 
  'Race4: (Others)', 
  'Education2: (Some college, diploma, assoc)', 
  'Education3: (High School or less)',
  'Adult Disability x Gender',
  'Adult Disability x Education2',
  'Adult Disability x Education3',
  'Adult Disability x Time',
  'Gender x Marital status',
  'Gender x Education2',
  'Gender x Education3',
  'Marital status x Race2', 
  'Marital status x Race3', 
  'Marital status x Race4', 
  'Marital status x Education2', 
  'Marital status x Education3', 
  'Race2 x Education2', 
  'Race3 x Education2', 
  'Race4 x Education2', 
  'Race2 x Education3', 
  'Race3 x Education3', 
  'Race4 x Education3'
  )

print( xtable( Coeff_Fixed, align = "lrrr", digits = c( 0, 3, 3, 4), caption = "Regression result", 
               label = "tab:Table2Reg" ), table.placement = "H" )

#####################################################################
## Disability only 
#####################################################################
modelFPL100Disab_Summary <- loadModel( modelFilename = 'modelFPL100Disab_Summary.RData' )

Coeff_Disab <- as.data.frame( modelFPL100Disab_Summary$coefficients[, c( 1, 2, 5 )] )
colnames( Coeff_Disab ) <- c( 'beta', 'stdError', 'pValue' )
Coeff_Disab <- within( data = Coeff_Disab, {
  beta = round( beta, 3 )
  stdError = round( stdError, 3 )
  pValue = round( pValue, 4 )
})
rownames( Coeff_Disab ) <- c(
  'Intercept', 
  'Time', 
  'Time2', 
  'Gender: (Female)',
  'Marital status: (Not married)',
  'Race2: (Black)', 
  'Race3: (Hispanic)', 
  'Race4: (Others)', 
  'Education2: (Some college, diploma, assoc)', 
  'Education3: (High School or less)',
  'Gender x Marital status',
  'Marital status x Race2', 
  'Marital status x Race3', 
  'Marital status x Race4', 
  'Marital status x Education2', 
  'Marital status x Education3', 
  'Race2 x Education2', 
  'Race3 x Education2', 
  'Race4 x Education2', 
  'Race2 x Education3', 
  'Race3 x Education3', 
  'Race4 x Education3'
)
print( xtable( Coeff_Disab, align = "lrrr", digits = c( 0, 3, 3, 4), caption = "Regression result", 
               label = "tab:Table3Reg" ), table.placement = "H" )
