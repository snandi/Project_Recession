rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )
#dev.off( )

########################################################################
## This script analyzes the income poverty over time. This is similar to
## RScript05-2, with the following differences
## 1. This analyzes FPL100Num, with baselines 
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
  postHocTableDF <- postHocTableDF[ order( postHocTableDF[,'p.value'], -abs( postHocTableDF[,'t-value'] ), 
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
modelFPL100_lmer <- lme4::lmer( 
  FPL100_num ~ 1 + Time + I( Time^2 ) + adult_disb + gender + ms + race_origin + education + 
    adult_disb*gender + adult_disb*education + adult_disb*Time +
    gender*ms + gender*education + ms*race_origin + ms*education + race_origin*education +
    ( 1 | hhid ), data = Data, weights = wt 
)
summary( modelFPL100_lmer )
stargazer::stargazer( modelFPL100_lmer )

modelFPL100 <- lmerTest::lmer( 
  FPL100_num ~ 1 + Time + I( Time^2 ) + adult_disb + gender + ms + race_origin + education + 
    adult_disb*gender + adult_disb*education + adult_disb*Time +
    gender*ms + gender*education + ms*race_origin + ms*education + race_origin*education +
    ( 1 | hhid ), data = Data, weights = wt 
)
# lmerTest::summary( modelFPL100 )
# lmerTest::anova( modelFPL100 )

# Residuals <- residuals( modelFPL100 )
# FittedValues <- fitted.values( modelFPL100 )
# qplot() + geom_point( aes( x = FittedValues, y = Residuals ) )

modelFPL100_Anova <- lmerTest::anova( modelFPL100 )
modelFPL100_Summary <- lmerTest::summary( modelFPL100 )
print( modelFPL100_Summary )

modelFPL100_AnovaDF <- formatAnovaTableForXtable( anovaTable = modelFPL100_Anova )
CAPTION <- "FPL100 vs demographic factors and time and disability status" 
LABEL <- 'tab:Anova1'

print( xtable( modelFPL100_AnovaDF, digits = c( 0, 2, 2, 0, 2, 4 ) , align = 'lrrrrr', 
               caption = CAPTION, label = LABEL, floating = TRUE, latex.environments = "center" ), 
       table.placement = "H" )

saveModel( modelData = modelFPL100, modelFilename = 'modelFPL100.RData' )

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
    gender*Time + ms*Time + race_origin*Time + education*Time + 
    gender*ms*Time + race_origin*I(Time^2) +
    ( 1 | hhid ), data = DataDisb, weights = wt
)
saveModel( modelData = modelFPL100Disab, modelFilename = 'modelFPL100Disab.RData' )

modelFPL100Disab_Anova <- lmerTest::anova( modelFPL100Disab )
modelFPL100Disab_Summary <- lmerTest::summary( modelFPL100Disab )
print( modelFPL100Disab_Summary )

modelFPL100Disab_AnovaDF <- formatAnovaTableForXtable( anovaTable = modelFPL100Disab_Anova )
CAPTION <- "FPL100 vs demographic factors and time, for households with Disability" 
LABEL <- 'tab:Anova2'
print( xtable( modelFPL100Disab_AnovaDF, digits = c( 0, 2, 4 ), align = 'lrr', 
               caption = CAPTION, label = LABEL, floating = TRUE, latex.environments = "center" ),
       table.placement = "H" )

#######################################################################
## Post hoc tests
#######################################################################
postHocFactors <- c( 'race_origin', 'education', 
                     'gender:ms', 'gender:education', 'ms:race_origin', 'ms:education',
                     'race_origin:education'
)

Factor <- postHocFactors[3]

for( Factor in postHocFactors ){
  print( "#############################################################" )
  print( Factor )
  print( "#############################################################" )
  
  postHoc <- lmerTest::difflsmeans( 
    model = modelFPL100, 
    test.effs = Factor
  )
  
  CAPTION <- paste( 'Post-hoc test of', getFactorName( Factor ) )
  LABEL <- paste0( 'tab:', Factor )
  columnsToDisplay <- c( 'Factor Levels', 'Estimate', 'Standard Error', 't-value', 'p-value' )  
  postHocDF <- formatPostHocTables( postHocTable = postHoc )
  try( print( xtable( postHocDF[, columnsToDisplay], align = 'llrrrr', digits = c( 0, 0, 2, 2, 2, 4 ), 
                      caption = CAPTION, label = LABEL ), 
              include.rownames = FALSE, table.placement = "H", size = 'footnotesize' ) )
  
  try( print( postHoc ) )
  
  try( rm( postHoc ) )
}

# dev.off( )

#######################################################################
## Post hoc tests for disability only
#######################################################################
print( "#############################################################" )
print( "Post hoc tests for disability only" )
print( "#############################################################" )

postHocFactors <- c( 'gender', 'ms', 'race_origin', 'education', 'gender:ms', 'ms:race_origin', 
                     'ms:education', 'race_origin:education' )
Factor <- 'gender:ms'
for( Factor in postHocFactors ){
  print( "#############################################################" )  
  print( Factor )
  print( "#############################################################" )
  
  postHoc <- lmerTest::difflsmeans( 
    model = modelFPL100Disab, 
    test.effs = Factor
  )
  
  postHoc <- lmerTest::lsmeans( 
    model = modelFPL100Disab, 
    test.effs = Factor
  )

  CAPTION <- paste( 'Post-hoc test of', getFactorName( Factor ), 'for households with Disability' )
  LABEL <- paste0( 'tab:', gsub( pattern = ':', replacement = '_', x = Factor ), '_Disb' )
  columnsToMerge <- c( 'Factor Levels', 'Estimate', 'Standard Error', 't-value', 'p-value' )  
  postHocDF <- formatPostHocTables( postHocTable = postHoc,  )
  try( print( xtable( postHocDF[, columnsToMerge], align = 'llrrrr', digits = c( 0, 0, 2, 2, 2, 4 ), 
                      caption = CAPTION, label = LABEL ), include.rownames = FALSE, 
              table.placement = "H", size = 'footnotesize' ) )
  
  try( print( postHoc ) )
  try( rm( postHoc ) )
}

