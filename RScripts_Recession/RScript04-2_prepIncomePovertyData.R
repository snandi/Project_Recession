rm( list=ls( all.names=TRUE ) )
rm( list=objects( all.names=TRUE ) )
#dev.off( )

########################################################################
## This script produces the data for income poverty analysis 
## This script added variable eorigin, to include hispanics in the analysis
## This uses whfnwgt as the survey weights, instead of Weights_Long.RData
## This removes observations from 2008 Q2 and 2013 Q3
########################################################################


dropSomeMonths <- function( dropMonths = c( 'May 2008', 'Jun 2008', 'Jul 2013' ), Data ){
  dropMonthsYrMon <- as.yearmon( dropMonths )
  keepMonths <- unique( Data$yearmon ) %w/o% dropMonths
  Data <- subset( Data, yearmon %in% keepMonths )
  return( Data )  
}

fn_DataforIncPov_v3 <- function( Data ){
  Data$rhpov2 <- 2 * Data$rhpov_qtr
  
  Data$FPL200 <- ( Data$thtotinc_qtr < Data$rhpov2 )
  Data$FPL100 <- ( Data$thtotinc_qtr < Data$rhpov_qtr )
  Data$FPL200[Data$FPL100 == TRUE] <- FALSE
  
  Data$Pct_rhpov <- Data$thtotinc_qtr/Data$rhpov_qtr
  
  Data$adult_disb <- factor( Data$adult_disb, labels = c( 'no', 'yes' ) )
  
  rownames( Data ) <- NULL
  #Data <- subset( Data, yearmon != 'May 2008' )
  Data$FPL100_num <- Data$thtotinc_qtr/Data$rhpov_qtr
  Data$FPL200_num <- Data$thtotinc_qtr/Data$rhpov2
  
  ## Normalize FPL100
  SplitByssuid <- split( x = Data, f = as.factor( Data$ssuid ) )
  Data$FPL100_noBaseline <- do.call( what = c, args = lapply( X = SplitByssuid, FUN = normalize_baseline, Colname = 'FPL100_num' ) )
  rownames( Data ) <- NULL
  
  ## Normalize FPL200
  Data$FPL200_noBaseline <- do.call( what = c, args = lapply( X = SplitByssuid, FUN = normalize_baseline, Colname = 'FPL200_num' ) )
  
  try( rm( SplitByssuid ) )
  gc( )
  
  comment( Data ) <- 'The lower the value of Pct_rhpov, the worse off the household is'
  return( Data )
}

########################################################################
## Run Path definition file                                           ##
########################################################################
PathPrefix <- '/Users/patron/Documents/snandi/'
PathPrefix <- '~/'
RScriptPath <- paste0( PathPrefix, 'Project_Recession/RScripts_Recession/' )
DataPath <- paste0( PathPrefix, 'Project_Recession/Data/data_2015Dec/' )
RDataPath <- paste0( PathPrefix, 'Project_Recession/RData/data_2015Dec/' )
PlotPath <- paste0( PathPrefix, 'Project_Recession/Plots/' )
Filename.Header <- paste0(RScriptPath, 'HeaderFile_Recession.R')
source(Filename.Header)

source( paste( RScriptPath, 'fn_Library_Recession.R', sep='' ) )

########################################################################
Today <- Sys.Date( )

########################################################################
## STATIC VARIABLES
########################################################################
EDUCATION_MAP <- as.data.frame( cbind( 
  from = c( "Not in Universe", "Less Than 1st Grade", "1st, 2nd, 3rd or 4th grade", "5th Or 6th Grade", 
           "7th Or 8th Grade", "9th Grade", "10th Grade", "11th Grade", "12th grade, no diploma", 
           "High School Graduate - (diploma", "Some college, but no degree", "Diploma or certificate from a", 
           "Associate (2-yr) college degree", "Bachelor's degree (for example:", "Master's degree (For example: MA,", 
           "Professional School degree (for", "Doctorate degree (for example:" ),
  to = c( 'High School or less', 'High School or less', 'High School or less', 'High School or less', 
         'High School or less', 'High School or less', 'High School or less', 'High School or less', 
         'High School or less', 'High School or less', 'Some college, diploma, assoc', 
         'Some college, diploma, assoc','Some college, diploma, assoc', 'Bachelors or higher', 'Bachelors or higher', 
         'Bachelors or higher', 'Bachelors or higher' )
 ), stringsAsFactors = FALSE )

RACE_MAP <- as.data.frame( cbind( 
  from = c( "White alone", "Black alone", "Asian alone", "Residual" ),
  to = c( 'White', 'Black', 'Others', 'Others' )
 ), stringsAsFactors = FALSE )

ORIGIN_MAP <- as.data.frame( cbind( 
  from = c( "Yes", "No" ),
  to = c( 'Hispanic', 'Non-Hispanic' )
 ), stringsAsFactors = FALSE )

MARITAL_STATUS_MAP <- as.data.frame( cbind( 
  from = c( "Married, spouse present", "Married, spouse absent",
           "Widowed", "Divorced", "Separated",
           "Never Married" ),
  to = c( "Married", "Married", rep( "Not married", 4 ) )
 ), stringsAsFactors = FALSE )

########################################################################
## load sipp08_MASTER.RData
########################################################################
filepathData <- paste( RDataPath, 'Data15.RData', sep = '' )
load( filepathData )

length( unique( as.numeric( Data15$ssuid ) ) )
nrow( unique( Data15[,c( 'ssuid', 'shhadid' )] ) ) ## This identifies unique households
nrow( unique( Data15[,c( 'ssuid', 'ehrefper' )] ) )
nrow( unique( Data15[,c( 'ssuid', 'epppnum' )] ) )
## 22002 unique ssuids, and 1 ehrefper per hh

Data15$yearmon <- as.yearmon( paste( Data15$rhcalmn, Data15$rhcalyr ) )
Data15$yearqtr <- as.yearqtr( Data15$yearmon )
Data15 <- Data15[order( Data15$ssuid, Data15$yearmon ),]

Colnames_keep <- c( 'ssuid',
                   'shhadid',
                   'yearmon',
                   'yearqtr',
                   'ehrefper',
                   'epppnum',
                   'rhtype',
                   'whfnwgt',
                   'thtotinc',
                   'rhpov',
                   'erace',
                   'eorigin',
                   'esex',
                   'ems',
                   'eeducate',
                   'adult_disb' )
Data <- unique( Data15[, Colnames_keep] )

Data <- dropSomeMonths( dropMonths = c( 'May 2008', 'Jun 2008', 'Jul 2013' ), Data = Data )

########################################################################
## Map education levels
########################################################################
Data$education <- mapvalues( 
  Data$eeducate,
  from = EDUCATION_MAP$from,
  to = EDUCATION_MAP$to
 )

########################################################################
## Map Race variable
########################################################################
Data$race <- mapvalues( 
  Data$erace,
  from = RACE_MAP$from,
  to = RACE_MAP$to
 )

########################################################################
## Map Origin variable
########################################################################
Data$origin <- mapvalues( 
  Data$eorigin,
  from = ORIGIN_MAP$from,
  to = ORIGIN_MAP$to
 )

########################################################################
## Combine Race and Origin
########################################################################
Data$race_origin <- as.vector( Data$origin )
Data$race_origin[Data$origin == 'Non-Hispanic'] <- as.vector( Data$race[Data$origin == 'Non-Hispanic'] )
Data$race_origin <- as.factor( Data$race_origin )

########################################################################
## Get Income Poverty by Gender & Marital status of head of household
########################################################################
Data$ms <- mapvalues( 
  Data$ems,
  from = MARITAL_STATUS_MAP$from,
  to = MARITAL_STATUS_MAP$to
 )
Data$gender_ms <- with( Data, interaction( esex, ms ) )

Data$gender <- Data$esex

Data$year <- format( Data$yearqtr, "%Y" )

########################################################################
## Check weights
########################################################################
Data$whfnwgt[ Data$ssuid == "365958123770" & Data$yearmon == "Jul 2008"] <- 
  Data$whfnwgt[ Data$ssuid == "365958123770" & Data$yearmon == "Aug 2008"]

summary( Data$whfnwgt )

########################################################################
## Aggregate total income data by quarter
########################################################################
Data <- dplyr::arrange( .data = Data, ssuid, yearqtr, yearmon )

#View( subset( Data, ssuid == "955925986509" ) )

nCores <- max( 1, detectCores() - 2 )
print( nCores )
cl <- parallel::makeCluster( nCores )
doParallel::registerDoParallel( cl )
# data_qtr <- ddply( .data = subset( Data, ssuid == "019128000276" ), 
data_qtr <- ddply( .data = Data, .parallel = TRUE, 
                   .( ssuid, shhadid, yearqtr, ms, gender, gender_ms, race_origin, education ), 
                   function( x ) data.frame( cbind( thtotinc_qtr = weighted.mean( x$thtotinc, x$whfnwgt ), 
                                                    whfnwgt_qtr = mean( x$whfnwgt ), 
                                                    rhpov_qtr = mean( x$rhpov ), 
                                                    adult_disb = max( x$adult_disb ) ) ) )

stopCluster(cl)  
gc()

Data_forIncPov <- fn_DataforIncPov_v3( Data = data_qtr )

########################################################################
## create a household id
########################################################################
Data_forIncPov$hhid <- paste( Data_forIncPov$ssuid, Data_forIncPov$shhadid, sep = '_' )

########################################################################
## save the data
########################################################################
Filename <- paste0( RDataPath, 'Data_forIncPov_v5_newWts.RData' )
save( Data_forIncPov, file = Filename )
try( rm( Data, Temp, Data_forIncPov ) )
gc( )

