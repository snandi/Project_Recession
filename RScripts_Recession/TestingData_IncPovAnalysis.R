rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )
#dev.off( )

########################################################################
## This script prepares testing data for mean plots of Income Poverty
## models
########################################################################
returnPredictedFPL <- function( HHID, modelData ){
  print( HHID )
  dataTemp <- subset( Data, hhid == HHID )
  dataTemp <- merge( x = dataTemp, y = weightMeans, by = 'yearqtr', all.y = F )
  colnames( dataTemp )[colnames( dataTemp ) == 'wt.y' ] <- 'wt'
  dataToPredict <- dataTemp[ , c( 'hhid', 'yearqtr', 'Time', 'wt', 'adult_disb', 'gender', 'ms',
                                  'education', 'race_origin' ) ]
  dataToPredict$predictedFPL <- try( predict( object = modelData, newdata = dataToPredict ) )
  
  predictedData <- dataToPredict[, c( 'yearqtr', 'predictedFPL' ) ]
  return( predictedData )  
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
nCores <- max( 1, parallel::detectCores( logical = TRUE ) - 3 )

########################################################################
## load income poverty data
########################################################################
filenameData <- paste0( RDataPath, 'Data_forIncPovModel_v5.RData' )
load( filenameData )
assign( x = 'Data', value = Data_forIncPovModel )
rm( Data_forIncPovModel )

########################################################################
## load income poverty Model
########################################################################
modelFilepath <- paste0( RDataPath, 'modelFPL100.RData' )
load( modelFilepath )
assign( x = 'modelFPL100', value = modelData )
rm( modelData )

########################################################################
## Mean weight
########################################################################
weightData <- Data[, c('yearqtr', 'wt')]
weightMeans <- aggregate ( wt ~ yearqtr, data = weightData, FUN = mean )
str( weightMeans )

########################################################################
## Married Male White Bachelors Non-Disab
########################################################################
MS <- 'Married'
Gender <- 'Male'
Ethnicity <- 'White'
Education <- 'Bachelors or higher'
Disab <- 'yes'
HH_TYPE <- paste( MS, Gender, Ethnicity, Education, 'Disab:', Disab )

hhidList <- as.vector( unique( subset( Data, ms == MS & gender == Gender & race_origin == Ethnicity & 
                                         education == Education & adult_disb == Disab )[, 'hhid'] ) )

cl <- makePSOCKcluster( nCores )
doParallel::registerDoParallel( cl )

predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                  modelData = modelFPL100, .parallel = TRUE,
                                  .paropts = list( .packages = c('plyr'), .export = ls() )
)
stopCluster( cl )
gc()

predictedFPLMean1 <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
predictedFPLMean1$hhType <- HH_TYPE

########################################################################
## Married Female White Bachelors Non-Disab
########################################################################
MS <- 'Not married'
Gender <- 'Female'
Ethnicity <- 'White'
Education <- 'Bachelors or higher'
Disab <- 'yes'
HH_TYPE <- paste( MS, Gender, Ethnicity, Education, 'Disab:', Disab )

hhidList <- as.vector( unique( subset( Data, ms == MS & gender == Gender & race_origin == Ethnicity & 
                                         education == Education & adult_disb == Disab )[, 'hhid'] ) )

cl <- makePSOCKcluster( nCores )
doParallel::registerDoParallel( cl )

predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                  modelData = modelFPL100, .parallel = TRUE,
                                  .paropts = list( .packages = c('plyr'), .export = ls() )
)
stopCluster( cl )
gc()

predictedFPLMean2 <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
predictedFPLMean2$hhType <- HH_TYPE

########################################################################
## Prepare testing data for white, married, male, High School or less
########################################################################
MS <- 'Not married'
Gender <- 'Female'
Ethnicity <- 'Black'
Education <- 'High School or less'
Disab <- 'yes'
HH_TYPE <- paste( MS, Gender, Ethnicity, Education, 'Disab:', Disab )

hhidList <- as.vector( unique( subset( Data, ms == MS & gender == Gender & race_origin == Ethnicity & 
                                         education == Education & adult_disb == Disab )[, 'hhid'] ) )

cl <- makePSOCKcluster( nCores )
doParallel::registerDoParallel( cl )

predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                  modelData = modelFPL100, .parallel = TRUE,
                                  .paropts = list( .packages = c('plyr'), .export = ls() )
)

stopCluster( cl )
gc()

predictedFPLMean3 <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
predictedFPLMean3$hhType <- HH_TYPE

########################################################################
## Prepare testing data for white, married, male, High School or less
########################################################################
MS <- 'Married'
Gender <- 'Male'
Ethnicity <- 'Black'
Education <- 'High School or less'
Disab <- 'yes'
HH_TYPE <- paste( MS, Gender, Ethnicity, Education, 'Disab:', Disab )

hhidList <- as.vector( unique( subset( Data, ms == MS & gender == Gender & race_origin == Ethnicity & 
                                         education == Education & adult_disb == Disab )[, 'hhid'] ) )

cl <- makePSOCKcluster( nCores )
doParallel::registerDoParallel( cl )

predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                  modelData = modelFPL100, .parallel = TRUE,
                                  .paropts = list( .packages = c('plyr'), .export = ls() )
)

stopCluster( cl )
gc()
predictedFPLMean4 <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
predictedFPLMean4$hhType <- HH_TYPE

########################################################################
## Rbind all household types
########################################################################
predictedFPLMeanAll <- rbind( predictedFPLMean1, predictedFPLMean3, 
                              predictedFPLMean2, predictedFPLMean4 )
# predictedFPLMeanAll$hhType <- factor( x = predictedFPLMeanAll$hhType, labels = 1:4, 
#                                       levels = c( 'Married Male White Bachelors or higher Disab: no', 
#                                                   'Not married Female White Some college, diploma, assoc Disab: no', 
#                                                   'Not married Female Black High School or less Disab: yes', 
#                                                   'Not married Female Hispanic High School or less Disab: yes' ) )
str( predictedFPLMeanAll )

qplot() + geom_point( aes( x = as.numeric( yearqtr ), y = predictedFPL, col = hhType ), data = predictedFPLMeanAll ) + 
  geom_smooth( aes( x = as.numeric( yearqtr ), y = predictedFPL, col = hhType ), method = 'loess', 
               data = predictedFPLMeanAll ) +
  facet_wrap( ~ hhType, scales = 'free', ncol = 2 ) + 
  theme( legend.position = '' )


########################################################################
## Disabled
########################################################################
fixef( modelFPL100 )

