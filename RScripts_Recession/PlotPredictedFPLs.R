rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )
#dev.off( )

########################################################################
## This script prepares testing data for mean prediction plots of 
## Income Poverty models
########################################################################
savePredictions <- function( predictedFPLMean, plotFPLMean, Factor ){
  filePlotPath <- paste0( PlotPath, 'PredictedFPLPlot_', Factor, '.jpg' )
  ggsave( filename = filePlotPath, plot = plotFPLMean, device = 'jpeg', width = 5, height = 4 )
  
  filePlotPath <- paste0( PlotPath, 'PredictedFPLPlot_', Factor, '.pdf' )
  ggsave( filename = filePlotPath, plot = plotFPLMean, device = 'pdf', width = 5, height = 4 )
  
  fileMeanPath <- paste0( RDataPath, 'PredictedFPLPlot_', Factor, '.RData' )
  save( predictedFPLMean, file = fileMeanPath )  
}

plotPredictedFPL <- function( predictedFPL_Factor, legendTitle = "Reference person", 
                              lmLineSize = 0 ){ 
  
  plotFPL <- qplot() + geom_point( aes( x = as.numeric( yearqtr ), y = predictedFPL, pch = hhType ), 
                                   data = predictedFPL_Factor, size = 2 ) + 
    geom_smooth( aes( x = as.numeric( yearqtr ), y = predictedFPL, group = hhType ), 
                 method = 'lm', se = FALSE, size = lmLineSize, col = 'gray40',
                 data = predictedFPL_Factor ) +
    geom_smooth( aes( x = as.numeric( yearqtr ), y = predictedFPL, group = hhType ), 
                 method = 'loess', se = FALSE, size = 0.75, col = 'gray20',
                 data = predictedFPL_Factor ) +
    # facet_wrap( ~ hhType, ncol = 2, scales = 'free_y' ) + 
    theme( legend.position = 'top', 
           panel.background  = element_rect( fill = "white", colour = NA ),
           panel.border      = element_rect( fill = NA, colour = "black" ),
           panel.grid.major  = element_line( colour = "grey80", size = 0.5 ),
           panel.grid.minor  = element_line( colour = "grey95", size = 0.3 )
    ) +
    scale_shape_discrete( name  = legendTitle ) + ## To change legend title
    xlab( label = '' ) + ylab( label = 'Average FPL100-ratio' )
  
  return( plotFPL )
}

returnPredictedFPL <- function( HHID, modelData, weightMeans = weightMeans ){
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
## Gender
########################################################################
predictedFPLMean_Gender <- c()
Factor <- 'gender'
for( factorLevel in levels( Data[, Factor ] ) ){
  print( factorLevel )
  Disab <- 'yes'
  # HH_TYPE <- paste( factorLevel, 'Disab:', Disab )
  HH_TYPE <- factorLevel
  
  hhidList <- as.vector( unique( subset( Data, get( Factor ) == factorLevel & adult_disb == Disab )[, 'hhid'] ) )
  print( length( hhidList ) )
  
  ## objectsExport <- unique( c( ls(), ls( envir = .GlobalEnv ) ) )
  ## print( objectsExport )
  cl <- makePSOCKcluster( nCores )
  doParallel::registerDoParallel( cl )
  
  predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                    modelData = modelFPL100, weightMeans = weightMeans,
                                    .parallel = TRUE, 
                                    .paropts = list( .packages = c( 'plyr' ), .export = ls() )
  )
  
  stopCluster( cl )
  gc()
  predictedFPLMeanLevel <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
  predictedFPLMeanLevel$hhType <- HH_TYPE
  
  predictedFPLMean_Gender <- rbind( predictedFPLMean_Gender, predictedFPLMeanLevel )
  rm( predictedFPLMeanLevel )
}

plotFPLMean_Gender <- plotPredictedFPL( predictedFPL_Factor = predictedFPLMean_Gender )
savePredictions( predictedFPLMean = predictedFPLMean_Gender, plotFPLMean = plotFPLMean_Gender, 
                 Factor = 'Gender' )

########################################################################
## Marital status
########################################################################
predictedFPLMean_MS <- c()
Factor <- 'ms'
for( factorLevel in levels( Data[, Factor ] ) ){
  print( factorLevel )
  Disab <- 'yes'
  # HH_TYPE <- paste( factorLevel, 'Disab:', Disab )
  HH_TYPE <- factorLevel
  
  hhidList <- as.vector( unique( subset( Data, get( Factor ) == factorLevel & adult_disb == Disab )[, 'hhid'] ) )
  print( length( hhidList ) )
  
  ## objectsExport <- unique( c( ls(), ls( envir = .GlobalEnv ) ) )
  ## print( objectsExport )
  cl <- makePSOCKcluster( nCores )
  doParallel::registerDoParallel( cl )
  
  predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                    modelData = modelFPL100, weightMeans = weightMeans,
                                    .parallel = TRUE, 
                                    .paropts = list( .packages = c( 'plyr' ), .export = ls() )
  )
  
  stopCluster( cl )
  gc()
  predictedFPLMeanLevel <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
  predictedFPLMeanLevel$hhType <- HH_TYPE
  
  predictedFPLMean_MS <- rbind( predictedFPLMean_MS, predictedFPLMeanLevel )
  rm( predictedFPLMeanLevel )
}

plotFPLMean_MS <- plotPredictedFPL( predictedFPL_Factor = predictedFPLMean_MS )
savePredictions( predictedFPLMean = predictedFPLMean_MS, plotFPLMean = plotFPLMean_MS, 
                 Factor = 'MS' )

########################################################################
## Ethnicity
########################################################################
predictedFPLMean_Ethnicity <- c()
Factor <- 'race_origin'
for( factorLevel in levels( Data[, Factor ] ) ){
  print( factorLevel )
  Disab <- 'yes'
  # HH_TYPE <- paste( factorLevel, 'Disab:', Disab )
  HH_TYPE <- factorLevel
  
  hhidList <- as.vector( unique( subset( Data, get( Factor ) == factorLevel & adult_disb == Disab )[, 'hhid'] ) )
  print( length( hhidList ) )
  
  ## objectsExport <- unique( c( ls(), ls( envir = .GlobalEnv ) ) )
  ## print( objectsExport )
  cl <- makePSOCKcluster( nCores )
  doParallel::registerDoParallel( cl )
  
  predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                    modelData = modelFPL100, weightMeans = weightMeans,
                                    .parallel = TRUE, 
                                    .paropts = list( .packages = c( 'plyr' ), .export = ls() )
  )
  
  stopCluster( cl )
  gc()
  predictedFPLMeanLevel <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
  predictedFPLMeanLevel$hhType <- HH_TYPE
  
  predictedFPLMean_Ethnicity <- rbind( predictedFPLMean_Ethnicity, predictedFPLMeanLevel )
  rm( predictedFPLMeanLevel )
}

plotFPLMean_Ethnicity <- plotPredictedFPL( predictedFPL_Factor = predictedFPLMean_Ethnicity )
savePredictions( predictedFPLMean = predictedFPLMean_Ethnicity, plotFPLMean = plotFPLMean_Ethnicity, 
                 Factor = 'Ethnicity' )

########################################################################
## Education
########################################################################
predictedFPLMean_Education <- c()

Factor <- 'education'
for( factorLevel in levels( Data[, Factor ] ) ){
  print( factorLevel )
  Disab <- 'yes'
  # HH_TYPE <- paste( factorLevel, 'Disab:', Disab )
  HH_TYPE <- factorLevel
  
  hhidList <- as.vector( unique( subset( Data, get( Factor ) == factorLevel & adult_disb == Disab )[, 'hhid'] ) )
  print( length( hhidList ) )
  
  ## objectsExport <- unique( c( ls(), ls( envir = .GlobalEnv ) ) )
  ## print( objectsExport )
  cl <- makePSOCKcluster( nCores )
  doParallel::registerDoParallel( cl )
  
  predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                    modelData = modelFPL100, weightMeans = weightMeans,
                                    .parallel = TRUE, 
                                    .paropts = list( .packages = c( 'plyr' ), .export = ls() )
  )
  
  stopCluster( cl )
  gc()
  predictedFPLMeanLevel <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
  predictedFPLMeanLevel$hhType <- HH_TYPE
  
  predictedFPLMean_Education <- rbind( predictedFPLMean_Education, predictedFPLMeanLevel )
  rm( predictedFPLMeanLevel )
}

plotFPLMean_Education <- plotPredictedFPL( predictedFPL_Factor = predictedFPLMean_Education )
savePredictions( predictedFPLMean = predictedFPLMean_Education, plotFPLMean = plotFPLMean_Education, 
                 Factor = 'Education' )

########################################################################
## DISABILITY
########################################################################
predictedFPLMean_Disability <- c()
Factor <- 'adult_disb'
for( factorLevel in levels( Data[, Factor ] ) ){
  print( factorLevel )
  # HH_TYPE <- paste( factorLevel, 'Disab:', Disab )
  HH_TYPE <- factorLevel
  
  hhidList <- as.vector( unique( subset( Data, adult_disb == factorLevel )[, 'hhid'] ) )
  print( length( hhidList ) )
  
  ## objectsExport <- unique( c( ls(), ls( envir = .GlobalEnv ) ) )
  ## print( objectsExport )
  cl <- makePSOCKcluster( nCores )
  doParallel::registerDoParallel( cl )
  
  predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                    modelData = modelFPL100, weightMeans = weightMeans,
                                    .parallel = TRUE, 
                                    .paropts = list( .packages = c( 'plyr' ), .export = ls() )
  )
  
  stopCluster( cl )
  gc()
  predictedFPLMeanLevel <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
  predictedFPLMeanLevel$hhType <- HH_TYPE
  
  predictedFPLMean_Disability <- rbind( predictedFPLMean_Disability, predictedFPLMeanLevel )
  rm( predictedFPLMeanLevel )
}

plotFPLMean_Disability <- plotPredictedFPL( predictedFPL_Factor = predictedFPLMean_Disability, 
                                            legendTitle = 'Disability', lmLineSize = 0 )
savePredictions( predictedFPLMean = predictedFPLMean_Disability, plotFPLMean = plotFPLMean_Disability, 
                 Factor = 'Disability' )

########################################################################
## Married Male White Bachelors Non-Disab
########################################################################
MS <- 'Married'
Gender <- 'Male'
Ethnicity <- 'White'
Education <- 'Bachelors or higher'
Disab <- 'no'
HH_TYPE <- paste( MS, ', ', Gender, ', ', Ethnicity, ', ', Education, ', Disability:', Disab )

hhidList <- as.vector( unique( subset( Data, ms == MS & gender == Gender & race_origin == Ethnicity &
                                         education == Education & adult_disb == Disab )[, 'hhid'] ) )

cl <- makePSOCKcluster( nCores )
doParallel::registerDoParallel( cl )

predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                  modelData = modelFPL100, weightMeans = weightMeans,
                                  .parallel = TRUE, 
                                  .paropts = list( .packages = c( 'plyr' ), .export = ls() )
)

stopCluster( cl )
gc()

predictedFPLMean1 <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
predictedFPLMean1$hhType <- HH_TYPE

########################################################################
## Non Married Female Hispanic High school or less
########################################################################
MS <- 'Not married'
Gender <- 'Female'
Ethnicity <- 'Black'
Education <- 'High School or less'
Disab <- 'yes'
HH_TYPE <- paste( MS, ', ', Gender, ', ', Ethnicity, ', ', Education, ', Disability:', Disab )

hhidList <- as.vector( unique( subset( Data, ms == MS & gender == Gender & race_origin == Ethnicity &
                                         education == Education & adult_disb == Disab )[, 'hhid'] ) )

cl <- makePSOCKcluster( nCores )
doParallel::registerDoParallel( cl )

predictedFPL_Long <- plyr::ldply( .data = hhidList, .fun = returnPredictedFPL, 
                                  modelData = modelFPL100, weightMeans = weightMeans,
                                  .parallel = TRUE, 
                                  .paropts = list( .packages = c( 'plyr' ), .export = ls() )
)
stopCluster( cl )
gc()

predictedFPLMean2 <- aggregate ( predictedFPL ~ yearqtr, data = predictedFPL_Long, FUN = mean )
predictedFPLMean2$hhType <- HH_TYPE

predictedFPLMean_Contrasts <- rbind( predictedFPLMean1, predictedFPLMean2 )
predictedFPLMean_ContrastsToPlot <- subset( predictedFPLMean_Contrasts, yearqtr < 2013.5 )

plotFPLMean_Contrasts <- qplot() + geom_point( aes( x = as.numeric( yearqtr ), y = predictedFPL, pch = hhType ),
                      data = predictedFPLMean_ContrastsToPlot, size = 2 ) +
  geom_smooth( aes( x = as.numeric( yearqtr ), y = predictedFPL ), method = 'loess',
               data = predictedFPLMean_ContrastsToPlot, se = FALSE, size = 0.75, col = 'gray20' ) +
  facet_wrap( ~ hhType, scales = 'free', ncol = 1 ) +
  theme( legend.position = '',
         panel.background  = element_rect( fill = "white", colour = NA ),
         panel.border      = element_rect( fill = NA, colour = "black" ),
         panel.grid.major  = element_line( colour = "grey80", size = 0.5 ),
         panel.grid.minor  = element_line( colour = "grey95", size = 0.3 )
  ) +
  #    scale_shape_discrete( name  = legendTitle ) + ## To change legend title
  xlab( label = '' ) + ylab( label = 'Average FPL100-ratio' )

savePredictions( predictedFPLMean = predictedFPLMean_Contrasts, plotFPLMean = plotFPLMean_Contrasts, 
                 Factor = 'Contrasts' )

