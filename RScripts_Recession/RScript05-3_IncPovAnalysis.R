rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )
#dev.off( )

########################################################################
## This script analyzes the income poverty over time. This is similar to
## RScript05-2, with the following differences
## 1. This analyzes FPL100Num, with baselines 
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
    postHocTableDF$`p-value` <- p.adjust( p = postHocTableDF$`p-value`, method = multipleCorrectionMethod )
  }
  
  postHocTableDF <- postHocTableDF[ order( postHocTableDF$`p-value`, decreasing = F ), ]
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
    adult_disb*gender + adult_disb*education + adult_disb*Time +
    gender*ms + gender*education + ms*race_origin + ms*education + race_origin*education +
    ( 1 | hhid ), data = Data, weights = wt 
)
time2 <- Sys.time( )
print( time2 - time1 )
# lmerTest::summary( modelFPL100 )
# lmerTest::anova( modelFPL100 )

# Residuals <- residuals( modelFPL100 )
# FittedValues <- fitted.values( modelFPL100 )
# qplot() + geom_point( aes( x = FittedValues, y = Residuals ) )

modelFPL100_Anova <- lmerTest::anova( modelFPL100 )
modelFPL100_Summary <- lmerTest::summary( modelFPL100 )
print( modelFPL100_Summary )

modelFPL100_AnovaDF <- formatAnovaTableForXtable( anovaTable = modelFPL100_Anova )
print( xtable( modelFPL100_AnovaDF, digits = c( 0, 2, 2, 0, 2, 4 ) , 
               caption = "Model 1: FPL100 vs demographic factors, time and disability", 
               floating = TRUE, latex.environments = "center"
) )

#######################################################################
## Print summaries of the two models
########################################################################
# source( paste( RScriptPath, 'stargazer_lme4.R', sep='' ) )
# stargazer( modelFPL100_Summary$coefficients, modelFPL100_Summary$coefficients, type = 'latex' )
summaryWithBaseline <- round( modelFPL100_Summary$coefficients[,c( "Estimate", "Std. Error", "Pr(>|t|)" )], 4 )

print( summaryWithBaseline )

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
  FPL100 ~ 1 + Time + I( Time^2 ) + gender + ms + race_origin + education + 
    gender*ms + ms*race_origin + ms*education + race_origin*education +
    ( 1 | hhid ), data = DataDisb, weights = wt
)

modelFPL100Disab_Anova <- lmerTest::anova( modelFPL100Disab )
modelFPL100Disab_Summary <- lmerTest::summary( modelFPL100Disab )
print( modelFPL100Disab_Summary )

modelFPL100Disab_AnovaDF <- formatAnovaTableForXtable( anovaTable = modelFPL100Disab_Anova )
print( xtable( modelFPL100Disab_AnovaDF, digits = c( 0, 2, 2, 0, 2, 4 ) , 
               caption = "Model 3: FPL100 vs demographic factors, time and disability, disability only", 
               floating = TRUE, latex.environments = "center"
) )

#######################################################################
## Model with Non Disabled only
########################################################################
DataNoDisb <- subset( Data, adult_disb == "no" )

modelFPL100NoDisab <- lmerTest::lmer( 
  FPL100 ~ 1 + Time + I( Time^2 ) + gender + ms + race_origin + education + 
    gender*ms + ms*race_origin + ms*education + race_origin*education +
    ( 1 | hhid ), data = DataNoDisb, weights = wt
)
modelFPL100NoDisab_Anova <- lmerTest::anova( modelFPL100NoDisab )
modelFPL100NoDisab_Summary <- lmerTest::summary( modelFPL100NoDisab )
print( modelFPL100NoDisab_Anova )
print( modelFPL100NoDisab_Summary )

#######################################################################
## Post hoc tests
#######################################################################
postHocFactors <- c( 'race_origin', 'education', 
                     'gender:ms', 'gender:education', 'ms:race_origin', 'ms:education',
                     'race_origin:education'
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
    
  CAPTION <- paste( 'Post-hoc test of', Factor )
  columnsToDisplay <- c( 'Factor Levels', 'Estimate', 'Standard Error', 't-value', 'p-value' )  
  postHocDF <- formatPostHocTables( postHocTable = postHoc )
  try( print( xtable( postHocDF[, columnsToDisplay], align = 'llrrrr', digits = c( 0, 0, 2, 2, 2, 4 ), 
                      caption = CAPTION ), include.rownames = FALSE ) )
  
  try( print( postHoc ) )
  
  # try( plot( plotPostHoc ) )
  # try( plot( plotPostHocNoBaseline ) )
  try( rm( postHoc, plotPostHoc ) )
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

postHocFactors <- c( 'gender', 'ms', 'race_origin', 'education', 'gender:ms', 'ms:race_origin', 
                     'ms:education', 'race_origin:education' )

for( Factor in postHocFactors ){
  print( "#############################################################" )  
  print( Factor )
  print( "#############################################################" )
  
  postHoc <- lmerTest::difflsmeans( 
    model = modelFPL100Disab, 
    test.effs = Factor
  )
  
  plotPostHoc <- try( plotLSMeans( 
    response = postHoc$response,
    table = postHoc$diffs.lsmeans.table, 
    which.plot = 'DIFF of LSMEANS', 
    mult = TRUE
  ) )
  
  CAPTION <- paste( 'Post-hoc test of', Factor )
  columnsToMerge <- c( 'Factor Levels', 'Estimate', 'Standard Error', 't-value', 'p-value' )  
  postHocDF <- formatPostHocTables( postHocTable = postHoc )
  try( print( xtable( postHocDF[, columnsToMerge], align = 'llrrrr', digits = c( 0, 0, 2, 2, 2, 4 ), 
                      caption = CAPTION ), include.rownames = FALSE ) )
  
  try( print( postHoc ) )
  
  print( "#############################################################" )
  
  try( rm( postHoc, plotPostHoc ) )
}

