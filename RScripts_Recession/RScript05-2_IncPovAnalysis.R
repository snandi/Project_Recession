rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )
#dev.off( )

########################################################################
## This script analyzes the income poverty over time. This is similar to
## RScript05, with the following differences
## 1. This uses Data_forIncPov_v5_newWts.RData
## 2. This does not fit log( FPL100 )
########################################################################
preprocessAndSaveData <- function( Data, filenameModelData = 'Data_forIncPovModel_v5.RData' ){
  Data$year <- substr( x = Data$yearqtr, start = 1, stop = 4 )
  Data$year <- as.factor( Data$year )
  Data$wt <- Data$whfnwgt_qtr/1000
  Data$hhid <- as.factor( Data$hhid )
  Data$yearqtrNum <- as.numeric( Data$yearqtr ) 
  ## This is necessary for post hoc test, otherwise it is throwing exception as the 
  ## object not being a matrix, when difflsmeans is called
  Data$Time <- Data$yearQtrNumCentered <- Data$yearqtrNum - mean( Data$yearqtrNum )
  Data_forIncPovModel <- Data
  filenameData <- paste0( RDataPath, filenameModelData )
  save( Data_forIncPovModel, file = filenameData )
  return( Data_forIncPovModel )
}

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
  anovaTableDF <- anovaTableDF[ order( anovaTableDF$`p.value`, decreasing = F ), ]
  return( anovaTableDF )
}

formatPostHocTables <- function( postHocTable, multipleCorrection = TRUE, 
                                 multipleCorrectionMethod = 'BH' ){
  postHocTableDF <- as.data.frame( postHocTable$diffs.lsmeans.table )
  rownames( postHocTableDF ) <- gsub( pattern = Factor, replacement = '', x = rownames( postHocTableDF ) )
  postHocTableDF$`Factor Levels` <- rownames( postHocTableDF )
  
  if( multipleCorrection ){
    postHocTableDF$`p.value` <- p.adjust( p = postHocTableDF$`p.value`, method = multipleCorrectionMethod )
  }
  
  postHocTableDF <- postHocTableDF[ order( postHocTableDF$`p-value`, decreasing = F ), ]
  return( postHocTableDF )
}

mergePostHocTables <- function( postHoc1, postHoc2, multipleCorrection = TRUE ){
  
  ttplot1 <- formatPostHocTables( postHocTable = postHoc1, multipleCorrection = multipleCorrection )
  ttplot2 <- formatPostHocTables( postHocTable = postHoc2, multipleCorrection = multipleCorrection )
  
  columnsToMerge <- c( 'Factor Levels', 'Estimate', 'Standard Error', 't-value', 'p-value' )
  ttplot <- merge( x = ttplot1[,columnsToMerge], y = ttplot2[,columnsToMerge], 
                   by = 'Factor Levels', all = T )  
  
  ttplot <- ttplot[ order( ttplot$`t-value.y`, ttplot$`t-value.x`, decreasing = T ), ]
  colnames( ttplot ) <- c( 'Factor Levels', 'Est 1', 'Std Err 1', 't1', 'p-value 1', 
                           'Est 2', 'Std Err 2', 't2', 'p-value 2' )
  ttplot$t1 <- ttplot$t2 <- NULL
  
  return( ttplot )
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

########################################################################
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

time1 <- Sys.time( )
modelFPL100 <- lmerTest::lmer( 
  FPL100_num ~ 1 + Time + I( Time^2 ) + gender + ms + race_origin + adult_disb + education + 
    race_origin:gender + gender:ms + race_origin:ms + race_origin:adult_disb + gender:adult_disb + 
    ms:adult_disb + gender:ms:adult_disb + adult_disb:Time + education:adult_disb + 
    ( 1 | hhid ), data = Data, weights = wt 
)

# lmerTest::summary( modelFPL100 )
# lmerTest::anova( modelFPL100 )
time2 <- Sys.time( )
print( time2 - time1 )

modelFPL100_Anova <- lmerTest::anova( modelFPL100 )
modelFPL100_Summary <- lmerTest::summary( modelFPL100 )
print( modelFPL100_Summary )

modelFPL100_AnovaDF <- formatAnovaTableForXtable( anovaTable = modelFPL100_Anova )
print( xtable( modelFPL100_AnovaDF, digits = c( 0, 2, 2, 0, 2, 4 ) , 
               caption = "Model 1: FPL100 vs demographic factors, time and disability", 
               floating = TRUE, latex.environments = "center"
) )

#######################################################################
## Mixed Effects Model ( MEM ) of Income Poverty Ratio, controlled for 
## Baseline value of FPL100
########################################################################
modelFPL100NoBaseline <- lmerTest::lmer( 
  FPL100_noBaseline ~ 1 + Time + I( Time^2 ) + gender + ms + race_origin + adult_disb + education + 
    race_origin*gender + gender*ms + race_origin*ms + race_origin*adult_disb + gender*adult_disb + 
    adult_disb*ms + gender*ms*adult_disb + adult_disb*Time + education*adult_disb + 
    ( 1 | hhid ), data = Data, weights = wt
)

modelFPL100NoBaseline_Summary <- lmerTest::summary( modelFPL100NoBaseline )
print( modelFPL100NoBaseline_Summary )
modelFPL100NoBaseline_Anova <- lmerTest::anova( modelFPL100NoBaseline )

time3 <- Sys.time( )
print( time3 - time2 )

modelFPL100NoBaseline_AnovaDF <- formatAnovaTableForXtable( anovaTable = modelFPL100NoBaseline_Anova )
print( xtable( modelFPL100NoBaseline_AnovaDF, digits = c( 0, 2, 2, 0, 2, 4 ) , 
               caption = "Model 2: FPL100 vs demographic factors, time and disability \n with baseline differences in FPL100 eliminated", 
               floating = TRUE, latex.environments = "center"
) )

#######################################################################
## Print summaries of the two models
########################################################################
# source( paste( RScriptPath, 'stargazer_lme4.R', sep='' ) )
# stargazer( modelFPL100NoBaseline_Summary$coefficients, modelFPL100_Summary$coefficients, type = 'latex' )
summaryNoBaseline <- round( modelFPL100NoBaseline_Summary$coefficients[,c( "Estimate", "Std. Error", "Pr( >|t| )" )], 4 )
summaryWithBaseline <- round( modelFPL100_Summary$coefficients[,c( "Estimate", "Std. Error", "Pr( >|t| )" )], 4 )

print( summaryWithBaseline )

print( summaryNoBaseline )

#######################################################################
## Model with Disabled only
########################################################################
DataDisb <- subset( Data, adult_disb == "yes" )

modelFPL100NoBaselineDisab <- lmerTest::lmer( 
  FPL100_noBaseline ~ 1 + Time + I( Time^2 ) + gender + ms + race_origin + education + 
    race_origin*gender + gender*ms + race_origin*ms + ( 1 | hhid ), 
  data = DataDisb, weights = wt
)

modelFPL100NoBaselineDisab_Anova <- lmerTest::anova( modelFPL100NoBaselineDisab )
modelFPL100NoBaselineDisab_Summary <- lmerTest::summary( modelFPL100NoBaselineDisab )
print( modelFPL100NoBaselineDisab_Summary )

modelFPL100NoBaselineDisab_AnovaDF <- formatAnovaTableForXtable( anovaTable = modelFPL100NoBaselineDisab_Anova )
print( xtable( modelFPL100NoBaselineDisab_AnovaDF, digits = c( 0, 2, 2, 0, 2, 4 ) , 
               caption = "Model 3: FPL100 vs demographic factors, time and disability, disability only", 
               floating = TRUE, latex.environments = "center"
) )


#######################################################################
## Model with Non Disabled only
########################################################################
DataNoDisb <- subset( Data, adult_disb == "no" )

modelFPL100NoBaselineNoDisab2 <- lmerTest::lmer( 
  FPL100_noBaseline ~ 1 + Time + I( Time^2 ) + gender + ms + race_origin + education + 
    race_origin*gender + gender*ms + race_origin*ms + ( 1 | hhid ), 
  data = DataNoDisb, weights = wt
)
modelFPL100NoBaselineNoDisab2_Anova <- lmerTest::anova( modelFPL100NoBaselineNoDisab2 )
modelFPL100NoBaselineNoDisab2_Summary <- lmerTest::summary( modelFPL100NoBaselineNoDisab2 )
print( modelFPL100NoBaselineNoDisab2_Summary )

#######################################################################
## Post hoc tests
#######################################################################
postHocFactors <- c( 'race_origin', 'education', 'gender:ms', 'ms:race_origin', 
                     'gender:race_origin', 'race_origin:adult_disb', 'gender:adult_disb', 
                     'ms:adult_disb', 'gender:ms:adult_disb', 'adult_disb:education'
)

Factor <- postHocFactors[3]
# plotFilename <- paste0( PlotPath, 'PlotsPostHoc_RScript05-2.pdf' )
# pdf( file = plotFilename, onefile = TRUE )

for( Factor in postHocFactors ){
  print( "#############################################################" )
  print( Factor )
  print( "#############################################################" )
  
  postHoc <- lmerTest::difflsmeans( 
    model = modelFPL100, 
    test.effs = Factor
  )
  
  plotPostHoc <- try( plotLSMeans( 
    response = postHoc$response,
    table = postHoc$diffs.lsmeans.table, 
    which.plot = 'DIFF of LSMEANS', 
    mult = TRUE
  ) )
  
  postHocNoBaseline <- lmerTest::difflsmeans( 
    model = modelFPL100NoBaseline, 
    test.effs = Factor
  )
  
  plotPostHocNoBaseline <- try( plotLSMeans( 
    response = postHocNoBaseline$response,
    table = postHocNoBaseline$diffs.lsmeans.table, 
    which.plot = 'DIFF of LSMEANS', 
    mult = TRUE
  ) )
  
  CAPTION <- paste( 'Post-hoc test of', Factor )
  postHocMerged <- try( mergePostHocTables( postHoc1 = postHoc, postHoc2 = postHocNoBaseline ) )
  postHocMerged <- postHocMerged[ order( postHocMerged$`p-value 2`, postHocMerged$`p-value 1`, 
                                         decreasing = F ), ]
  
  try( print( xtable( postHocMerged, digits = c( 0, 0, 2, 2, 4, 2, 2, 4 ), 
                      caption = CAPTION ), include.rownames = FALSE ), )
  
  try( print( postHoc ) )
  try( print( postHocNoBaseline ) )
  
  # try( plot( plotPostHoc ) )
  # try( plot( plotPostHocNoBaseline ) )
  print( "#############################################################" )  
  try( rm( postHoc, postHocNoBaseline, plotPostHoc, plotPostHocNoBaseline ) )
}

# dev.off( )

time4 <- Sys.time( )
print( time4 - time3 )

#######################################################################
## Post hoc tests for disability only
#######################################################################
print( "#############################################################" )
print( "Post hoc tests for disability only" )
print( "#############################################################" )

postHocFactors <- c( 'race_origin', 'education', 'gender:ms', 'ms:race_origin', 
                     'gender:race_origin' )

for( Factor in postHocFactors ){
  print( "#############################################################" )  
  print( Factor )
  print( "#############################################################" )
  
  postHocNoBaseline <- lmerTest::difflsmeans( 
    model = modelFPL100NoBaselineDisab, 
    test.effs = Factor
  )
  
  plotPostHocNoBaseline <- try( plotLSMeans( 
    response = postHocNoBaseline$response,
    table = postHocNoBaseline$diffs.lsmeans.table, 
    which.plot = 'DIFF of LSMEANS', 
    mult = TRUE
  ) )
  
  CAPTION <- paste( 'Post-hoc test of', Factor )
  columnsToMerge <- c( 'Factor Levels', 'Estimate', 'Standard Error', 't-value', 'p-value' )  
  postHocNoBaselineDF <- formatPostHocTables( postHocTable = postHocNoBaseline )
  try( print( xtable( postHocNoBaselineDF[, columnsToMerge], digits = c( 0, 0, 2, 2, 2, 4 ), 
                      caption = CAPTION ), include.rownames = FALSE ), )
  
  try( print( postHocNoBaseline ) )
  
  print( "#############################################################" )
  
  try( rm( postHocNoBaseline, plotPostHocNoBaseline ) )
}

