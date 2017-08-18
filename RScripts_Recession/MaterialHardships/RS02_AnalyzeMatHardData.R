rm( list = ls( all.names = TRUE ) )
rm( list = objects( all.names = TRUE ) )
#dev.off()

########################################################################
## Analyze material hardships data
########################################################################

########################################################################
## Run Path definition file                                           
########################################################################
RScriptPath <- '~/Project_Recession/RScripts_Recession/'
DataPath <- '~/Project_Recession/Data/data_2015Dec/'
RDataPath <- '~/Project_Recession/RData/data_2015Dec/'
Filename.Header <- paste0( RScriptPath, 'HeaderFile_Recession.R', sep = '' )
source( Filename.Header )
source( paste( RScriptPath, 'fn_Library_Recession.R', sep = '' ) )
library( readstata13 )
########################################################################

fixDisabFactor <- function( data ){
  data$adult_disb <- factor( data$adult_disb, levels = c( 0, 1 ), labels = c( 'No', 'Yes' ) )
  colnames( data )[colnames( data ) == 'adult_disb' ] <- 'Disability'
  return( data )
}

getMHprop <- function( hardshipVar = 'hardships_two_more', data = dataMatHardWtsDisab ){
  if( class( data[,hardshipVar] ) == 'factor' ){
    data[,hardshipVar] <- as.numeric( data[,hardshipVar] ) - 1
  }
  
  propData <- aggregate( get( hardshipVar ) ~ adult_disb + swave, data = data, FUN = sum )
  colnames( propData )[3] <- hardshipVar
  propData$N_ <- ifelse( propData$adult_disb == 1, N_Disab, N_nonDisab )
  propData$prop <- propData[, hardshipVar ] / propData$N_
  
  propData <- fixDisabFactor( data = propData )
  return( propData )
}

getMHpvalues <- function( hardshipVar ){
  propData <- getMHprop( hardshipVar = hardshipVar, data = dataMatHardWtsDisab )
  
  testDisab <- prop.test( x = propData[propData$Disability == 'Yes',hardshipVar], 
                          n = propData$N_[propData$Disability == 'Yes' ] , correct = FALSE )
  pDisab <- round( testDisab$p.value, 4 )
  testNonDisab <- prop.test( x = propData[propData$Disability == 'No',hardshipVar], 
                             n = propData$N_[propData$Disability == 'No' ] , correct = FALSE )
  pNonDisab <- round( testNonDisab$p.value, 4 )
  pValueData <- as.data.frame( cbind( measure = hardshipVar, 
         Disab_6 = round( 100*propData$prop[propData$Disability == 'Yes' & propData$swave == 6 ], 2 ),
         Disab_9 = round( 100*propData$prop[propData$Disability == 'Yes' & propData$swave == 9 ], 2 ),
         pDisab = pDisab,
         NonDisab_6 = round( 100*propData$prop[propData$Disability == 'No' & propData$swave == 6 ], 2 ),
         NonDisab_9 = round( 100*propData$prop[propData$Disability == 'No' & propData$swave == 9 ], 2 ),
         pNonDisab = pNonDisab
  ), stringsAsFactors = F )
  return( pValueData )
}

########################################################################
## Load material hardships data
########################################################################
filenameMatHardFinal <- paste0( RDataPath, 'MaterialHardshipsData.RData' )
load( filenameMatHardFinal )

N_Disab <- sum( dataMatHardWtsDisab$adult_disb )/2 ## Because there are two time points, wave 6 and 9
N_Household <- length( unique( dataMatHardWtsDisab$hhid ) )
N_nonDisab <- N_Household - N_Disab

## proportion of individual material hardship measures
getMHprop( hardshipVar = 'gas', data = dataMatHardWtsDisab )
getMHpvalues( hardshipVar = 'gas' )

getMHprop( hardshipVar = 'rent', data = dataMatHardWtsDisab )
getMHprop( hardshipVar = 'meet_expenses', data = dataMatHardWtsDisab )
getMHprop( hardshipVar = 'phone', data = dataMatHardWtsDisab )
getMHprop( hardshipVar = 'doctor', data = dataMatHardWtsDisab )
getMHprop( hardshipVar = 'not_enough_food', data = dataMatHardWtsDisab )
getMHprop( hardshipVar = 'no_balan_food', data = dataMatHardWtsDisab )
getMHprop( hardshipVar = 'food_not_last', data = dataMatHardWtsDisab )
getMHprop( hardshipVar = 'skip_meal', data = dataMatHardWtsDisab )
getMHprop( hardshipVar = 'eat_less', data = dataMatHardWtsDisab )

listMHmeasures <- c( 'gas', 'rent', 'meet_expenses', 'phone', 'doctor', 'not_enough_food', 
                     'no_balan_food', 'food_not_last', 'skip_meal', 'eat_less',
                     'any_hardships_coded', 'hardships_two_more', 'hardships_three_more' )
pValuesMHmeasures <- plyr::ldply( .data = listMHmeasures, .fun = getMHpvalues )

filename <- paste0( '~/Project_Recession/Report_Recession/MaterialHardships_pValues.csv' )
write.csv( x = pValuesMHmeasures, file = filename, quote = F, row.names = F )

# ## any_hardships_coded
# propData <- getMHprop( hardshipVar = 'any_hardships_coded', data = dataMatHardWtsDisab )
# testDisab <- prop.test( x = propData$any_hardships_coded[propData$Disability == 'Yes' ], 
#                         n = propData$N_[propData$Disability == 'Yes' ] , correct = FALSE )
# pDisab <- round( testDisab$p.value, 4 )
# testNonDisab <- prop.test( x = propData$any_hardships_coded[propData$Disability == 'No' ], 
#                            n = propData$N_[propData$Disability == 'No' ] , correct = FALSE )
# pNonDisab <- round( testNonDisab$p.value, 4 )
# 
# propData$pValue <- c( '', '', pNonDisab, pDisab )
# propData$propText <- paste0( round( 100*propData$prop, 2 ), ifelse( propData$pValue != '', paste0( ', p = ', propData$pValue ), '' ) )
# plotAny <- ggplot( data = propData, aes( x = factor( swave ) , y = 100*prop) ) +
#   geom_point( aes( shape = Disability ), size = 4 ) +
#   geom_text( aes( label = propText, hjust = -0.25 ) ) +
#   ylab( label = 'proportion' ) + xlab( label = 'wave' ) +
#   ggtitle( label = 'Any type of hardships' ) +
#   theme( legend.position = 'top', 
#          plot.title = element_text(hjust = 0.5) )
# filenamePlot <- paste0( RScriptPath, 'MaterialHardships/', 'Plot_AnyHardships.pdf' )
# ggsave( filename = filenamePlot, plot = plotAny, device = 'pdf', width = 7, height = 7, units = 'in' )
# 
# ggplot( data = propData, aes( x = Disability, y = 100*prop ) ) +
#   geom_bar( aes( fill = factor( swave ) ), stat = 'identity', position = position_dodge() )
# 
# 
# ## hardships_two_more
# hardships_two_more <- getMHprop( hardshipVar = 'hardships_two_more', data = dataMatHardWtsDisab )
# ggplot( data = hardships_two_more, aes( x = factor( swave ) , y = prop) ) +
#   geom_point( aes( shape = Disability ), size = 4 ) +
#   theme( legend.position = 'top') 
# data <- hardships_two_more
# prop.test( x = data$hardships_two_more[data$Disability == 'Yes' ], 
#            n = data$N_[data$Disability == 'Yes' ] , correct = FALSE )
# prop.test( x = data$hardships_two_more[data$Disability == 'No' ], 
#            n = data$N_[data$Disability == 'No' ] , correct = FALSE )
# 
# ## hardships_three_more
# hardships_three_more <- getMHprop( hardshipVar = 'hardships_three_more', data = dataMatHardWtsDisab )
# ggplot( data = hardships_three_more, aes( x = factor( swave ) , y = prop) ) +
#   geom_point( aes( shape = Disability ), size = 4 ) +
#   theme( legend.position = 'top') 
# data <- hardships_three_more
# prop.test( x = data$hardships_three_more[data$Disability == 'Yes' ], 
#            n = data$N_[data$Disability == 'Yes' ] , correct = FALSE )
# prop.test( x = data$hardships_three_more[data$Disability == 'No' ], 
#            n = data$N_[data$Disability == 'No' ] , correct = FALSE )
# 
