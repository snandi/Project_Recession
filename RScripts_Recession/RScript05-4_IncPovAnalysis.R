rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )
#dev.off( )

########################################################################
## This script analyzes the income poverty over time. This is similar to
## RScript05-3, with the following differences
## 1. This includes interactions between time and all demographic factors
########################################################################
saveModel <- function( modelData, modelFilename ){
  modelFilepath <- paste0( RDataPath, modelFilename )
  save( modelData, file = modelFilepath )
}

preprocessAndSaveData <- function( Data, filenameModelData = 'Data_forIncPovModel_v5.RData' ){
  Data$year <- substr( x = Data$yearqtr, start = 1, stop = 4 )
  Data$year <- as.factor( Data$year )
  Data$wt <- Data$whfnwgt_qtr/1000
  Data$hhid <- as.factor( Data$hhid )
  Data$yearqtrNum <- as.numeric( Data$yearqtr ) 
  ## This is necessary for post hoc test, otherwise it is throwing exception as the 
  ## object not being a matrix, when difflsmeans is called
  Data$Time <- Data$yearQtrNumCentered <- Data$yearqtrNum - mean( Data$yearqtrNum )
  
  Data$race_origin <- factor( Data$race_origin, levels = c( "White", "Black", "Hispanic", "Others") )
  Data$ms <- factor( Data$ms, levels = c( "Married", "Not married" ) )
  Data$gender <- factor( Data$gender, levels = c( "Male", "Female" ) )
  Data$education <- factor( Data$education, 
                            levels = c( "Bachelors or higher", "Some college, diploma, assoc", "High School or less" ) )
  
  Data_forIncPovModel <- Data
  filenameData <- paste0( RDataPath, filenameModelData )
  save( Data_forIncPovModel, file = filenameData )
  return( Data_forIncPovModel )
}
# 
formatAnovaTableForXtable <- function( anovaTable, multipleCorrection = TRUE, 
                                       multipleCorrectionMethod = 'BH' ){
  if( class( anovaTable )[1] != 'anova' ){
    stop( "Argument not an Anova table" )
  }
  anovaTableDF <- na.omit( as.data.frame( anovaTable ) )
  colnames( anovaTableDF ) <- c( "Sum Sq", "Mean Sq", "NumDF", "DenDF", "F.value", "p.value" )
  anovaTableDF$DenDF <- NULL
  if( multipleCorrection ){
    anovaTableDF$`p.value` <- p.adjust( p = anovaTableDF$`p.value`, method = multipleCorrectionMethod )
  }
  anovaTableDF <- anovaTableDF[ order( anovaTableDF[,'p.value'], -anovaTableDF[,'F.value'], decreasing = F ), ]
  anovaTableDF$`p.value` <- round( anovaTableDF$`p.value`, 4 )
  anovaTableDF$`F.value` <- round( anovaTableDF$`F.value`, 2 )
  
  row.names( anovaTableDF ) <- sapply( X = row.names( anovaTableDF ), FUN = getFactorName )
  return( anovaTableDF[, c( 'F.value', 'p.value' ) ] )
}

formatPostHocTables <- function( postHocTable, multipleCorrection = TRUE, 
                                 multipleCorrectionMethod = 'BH' ){
  postHocTableDF <- as.data.frame( postHocTable$diffs.lsmeans.table )
  rownames( postHocTableDF ) <- gsub( pattern = Factor, replacement = '', x = rownames( postHocTableDF ) )
  postHocTableDF$`Factor Levels` <- rownames( postHocTableDF )
  
  if( multipleCorrection ){
    postHocTableDF$`p-value` <- p.adjust( p = postHocTableDF$`p-value`, method = multipleCorrectionMethod )
  }
  
  #   postHocTableDF <- postHocTableDF[ order( postHocTableDF$`p-value`, decreasing = F ), ]
  postHocTableDF <- postHocTableDF[ order( postHocTableDF[,'p-value'], -abs( postHocTableDF[,'t-value'] ), 
                                           decreasing = F ), ]
  
  postHocTableDF$`Factor Levels` <- sapply( X = postHocTableDF$`Factor Levels`, FUN = getFactorName )
  row.names( postHocTableDF ) <- sapply( X = row.names( postHocTableDF ), FUN = getFactorName )
  return( postHocTableDF )
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
## load income poverty data
########################################################################
#Filename <- paste0( RDataPath, 'Data_forIncPov_byRace.RData' )
#load( file = Filename )

##Filename <- paste0( RDataPath, 'Data_forIncPov.RData' )
##load( file = Filename )
Filename <- paste0( RDataPath, 'Data_forIncPov_v5_newWts.RData' )
load( file = Filename )

Data <- preprocessAndSaveData( Data = Data_forIncPov, filenameModelData = 'Data_forIncPovModel_v5.RData' )
rm( Data_forIncPov )

#######################################################################
## Mixed Effects Model ( MEM ) of Income Poverty Ratio
########################################################################
#library( lsmeans )

# FULLmodelFPL100 <- lmerTest::lmer( 
#   FPL100_num ~ 1 + Time + I( Time^2 ) + adult_disb + gender + ms + race_origin + education + 
#     adult_disb*gender + adult_disb*ms + adult_disb*race_origin + adult_disb*education + adult_disb*Time +
#     gender*ms + gender*race_origin + gender*education +
#     ms*race_origin + ms*education + race_origin*education +
#     ( 1 | hhid ), data = Data, weights = wt 
# )
# finalModel <- lmerTest::step( model = FULLmodelFPL100 )

modelFPL100 <- lmerTest::lmer( 
  FPL100_num ~ 1 + Time + I( Time^2 ) + adult_disb + gender + ms + race_origin + education + 
    adult_disb*Time + adult_disb*gender + adult_disb*education + 
    Time*gender + Time*ms + Time*race_origin + Time*education +
    gender*ms + gender*education + ms*race_origin + ms*education + race_origin*education +
    ( 1 | hhid ), data = Data, weights = wt 
)
saveModel( modelData = modelFPL100, modelFilename = 'modelFPL100_RS05-4.RData' )
# lmerTest::summary( modelFPL100 )
# lmerTest::anova( modelFPL100 )

# Residuals <- residuals( modelFPL100 )
# FittedValues <- fitted.values( modelFPL100 )
# qplot() + geom_point( aes( x = FittedValues, y = Residuals ) )

modelFPL100_Anova <- lmerTest::anova( modelFPL100 )
print( modelFPL100_Anova )

modelFPL100_Summary <- lmerTest::summary( modelFPL100 )
print( modelFPL100_Summary )
saveModel( modelData = modelFPL100_Summary, modelFilename = 'modelFPL100_Summary_RS05-4.RData' )

#######################################################################
## Model with Disabled only
########################################################################
DataDisb <- subset( Data, adult_disb == "yes" )

# FULLmodelFPL100Disab <- lmerTest::lmer( 
#   FPL100_ ~ 1 + Time + I( Time^2 ) + gender + ms + race_origin + education + 
#     gender*ms + gender*race_origin + gender*education +
#     ms*race_origin + ms*education + race_origin*education +
#     ( 1 | hhid ), data = DataDisb, weights = wt
# )
# finalModel <- lmerTest::step( model = FULLmodelFPL100Disab )
modelFPL100Disab <- lmerTest::lmer( 
  FPL100_num ~ 1 + Time + I( Time^2 ) + gender + ms + race_origin + education + 
    Time*gender + Time*ms + Time*race_origin + Time*education +
    gender*ms + gender*education + ms*race_origin + ms*education + race_origin*education +
    ( 1 | hhid ), data = DataDisb, weights = wt
)
saveModel( modelData = modelFPL100Disab, modelFilename = 'modelFPL100Disab_RS05-4.RData' )

modelFPL100Disab_Anova <- lmerTest::anova( modelFPL100Disab )
print( modelFPL100Disab_Anova )

modelFPL100Disab_Summary <- lmerTest::summary( modelFPL100Disab )
print( modelFPL100Disab_Summary )
saveModel( modelData = modelFPL100Disab_Summary, modelFilename = 'modelFPL100Disab_Summary_RS05-4.RData' )

