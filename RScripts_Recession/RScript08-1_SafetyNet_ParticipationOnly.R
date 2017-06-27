rm( list=ls( all.names=TRUE ) )
rm( list=objects( all.names=TRUE ) )
#dev.off()

########################################################################
## This script prepares the data for safety net program analysis
## with new weights and revised data
########################################################################
dropSomeMonths <- function( dropMonths = c( 'May 2008', 'Jun 2008', 'Jul 2013' ), Data ){
  dropMonthsYrMon <- as.yearmon( dropMonths )
  keepMonths <- unique( Data$yearmon ) %w/o% dropMonthsYrMon
  Data <- subset( Data, yearmon %in% keepMonths )
  return( Data )  
}

getParticipationProportion <- function( Data, columnName, programName ){
  TotalHH_Disab <- sum( unique( Data[, c( 'hhid', 'adult_disb' ) ] )$adult_disb )
  TotalHH_NonDisab <- length( unique( Data$hhid ) ) - TotalHH_Disab
  
  Data$participateProgram <- ( Data[, columnName] > 0 )
  
  participationData <- aggregate( 
    participateProgram ~ yearmon + adult_disb, 
    data = Data, 
    FUN = sum )
  
  participationData <- as.data.frame( participationData )
  participationData$eligible <- ifelse( test = participationData$adult_disb == 1, 
                                        yes = TotalHH_Disab, 
                                        no = TotalHH_NonDisab )
  participationData$propProgram <- participationData$participateProgram/participationData$eligible
  participationData$programName <- programName
  return( participationData )
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
library( RFunctionsSN )
source( paste( RScriptPath, 'fn_Library_Recession.R', sep='' ) )
SlidePath <- paste0( PathPrefix, 'Project_Recession/Slides_Recession/' )
########################################################################
Today <- Sys.Date()

## Load weights
Filename <- paste0( RDataPath, 'Weights_Long.RData' )
load( Filename )

########################################################################
## load the merged dataset
########################################################################
Filepath1 <- paste( RDataPath, 'Data15.RData', sep = '' )
load( Filepath1 )

length( unique( as.numeric( Data15$ssuid ) ) )
nrow( unique( Data15[,c( 'ssuid', 'shhadid' )] ) ) ## This identifies unique households

Data15$yearmon <- as.yearmon( paste( Data15$rhcalmn, Data15$rhcalyr ) )
Data15$yearqtr <- as.yearqtr( Data15$yearmon )
Data15 <- Data15[order( Data15$ssuid, Data15$shhadid, Data15$yearmon ),]
Data15$hhid <- paste( Data15$ssuid, Data15$shhadid, sep = '_' )

##Variable names for safety net:
#thsocsec: Total Household Social Security Income Recode 
#thssi: Total Household Supplemental Security Income Recode 
#thunemp: Total Household Unemployment Income Recode 
#thafdc: Total household public assistance payments 
#thfdstp:Total Household food stamps Received Recode  
#thnoncsh: Total Household Noncash Income Recode 

########################################################################
## Drop some months
########################################################################
Data15 <- dropSomeMonths( dropMonths = c( 'May 2008', 'Jun 2008', 'Jul 2008', 
                                          'May 2013', 'Jun 2013', 'Jul 2013' ), 
                          Data = Data15 )

########################################################################
## Define employment income
########################################################################
Data15$employmentIncome <- Data15$thtotinc - rowSums( Data15[, c( "thnoncsh", "thsocsec", "thssi", 
                                                                  "thunemp", "thvets", "thafdc", "thfdstp" )] )

########################################################################
## Income below FPL130
########################################################################
Data15$FPL130 <- ( Data15$employmentIncome < 1.3 * Data15$rhpov )
Data15$FPL200 <- ( Data15$employmentIncome < 2 * Data15$rhpov )

hhid_FPL130 <- aggregate( FPL130 ~ hhid, data = Data15, FUN = max )
hhid_FPL130 <- subset( hhid_FPL130, FPL130 == 1 )$hhid

hhid_FPL200 <- aggregate( FPL200 ~ hhid, data = Data15, FUN = max )
hhid_FPL200 <- subset( hhid_FPL200, FPL200 == 1 )$hhid

# View( subset( DataFPL200, hhid == "019133469324_11" | hhid == "019133651116_11" ) )

DataFPL130 <- subset( Data15, hhid %in% hhid_FPL130 )
DataFPL200 <- subset( Data15, hhid %in% hhid_FPL200 )

########################################################################
## Get the program participation proportion data
########################################################################
proportionData <- getParticipationProportion( Data = DataFPL200, columnName = 'thssi', programName = 'SSI' )
proportionData <- rbind( proportionData, 
                         getParticipationProportion( Data = DataFPL200, columnName = 'thfdstp', programName = 'Food Stamps' ) 
) 
proportionData <- rbind( proportionData, 
                         getParticipationProportion( Data = DataFPL200, columnName = 'thunemp', programName = 'Unemployment' ) 
) 
proportionData <- rbind( proportionData, 
                         getParticipationProportion( Data = DataFPL200, columnName = 'thafdc', programName = 'TANF' ) 
) 

proportionData$adult_disb <- factor( proportionData$adult_disb, levels = c( 1, 0 ), 
                                     labels = c( 'with Disab', 'with No Disab' ) )

########################################################################
## Get the program participation proportion Plot
########################################################################
proportionPlot <- qplot() + geom_line( aes( x = yearmon, y = propProgram, linetype = adult_disb ), 
                                       data = proportionData, size = 1 ) +
  facet_wrap( ~ programName, nrow = 2 ) +
  scale_y_continuous( labels = scales::percent ) +
  xlab( label = 'time' ) + ylab( label = 'program participation rate' ) +
  theme( legend.position   = 'top', 
         legend.title      = element_blank(), 
         panel.background  = element_rect( fill = "white", colour = NA ),
         panel.border      = element_rect( fill = NA, colour = "black" ),
         panel.grid.major  = element_line( colour = "grey80", size = 0.5 ),
         panel.grid.minor  = element_line( colour = "grey95", size = 0.3 )
  ) 

filenameSave <- paste0( PlotPath, 'ProgramParticipationPlots.pdf' )
ggsave( filename = filenameSave, plot = proportionPlot, device = 'pdf' )

filenameSave <- paste0( PlotPath, 'ProgramParticipationPlots.jpg' )
ggsave( filename = filenameSave, plot = proportionPlot, device = 'jpeg' )
