rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script reads in and does basic understanding of the recession ##
## dataset
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
RScriptPath <- '~/Project_Recession/RScripts_Recession/'
DataPath <- '~/Project_Recession/Data/data_2015Dec/'
RDataPath <- '~/Project_Recession/RData/data_2015Dec/'
Filename.Header <- paste('~/RScripts/HeaderFile_lmcg.R', sep='')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Recession.R', sep=''))
########################################################################
Today <- Sys.Date()

########################################################################
fn_substring <- function(String, Keys = Keys){
  ReturnString <- vector(length = 14)
  for(Var in 1:14){
    Start <- Keys[Var,'BEGIN']
    Stop <- Start + Keys[Var,'SIZE'] - 1
    
    ReturnString[Var] <- substr(x = String, start = Start, stop = Stop)
  }
  return(ReturnString)
}
########################################################################

## Load weights
Filename <- paste0(DataPath, 'lgtwgt2008w16.dat')
Wts.dat <- read.table(Filename, colClasses = 'character')
String <- Wts.dat[1,]
Wts.list <- as.list(as.vector(Wts.dat))[[1]]

Filename <- paste0(DataPath, 'keys_wts.txt')
Keys <- read.table(Filename, sep = ',', header = T, stringsAsFactors = FALSE)
Keys$DATA <- tolower(Keys$DATA)
str(Keys)

Time1 <- Sys.time()
Weights <- t(sapply(X = Wts.list, FUN = fn_substring, Keys = Keys))
Time2 <- Sys.time()
print(Time2 - Time1)

# Weights <- as.data.frame(matrix(data = 0, nrow = nrow(Wts.dat), ncol = nrow(Keys)))
# for(Row in 1:nrow(Weights)){
#   print(Row)
#   String <- Wts.dat[Row,]
#   for(Var in 1:nrow(Keys)){
#     Start <- Keys[Var,'BEGIN']
#     Stop <- Start + Keys[Var,'SIZE'] - 1
#     
#     Weights[Row,Var] <- substr(x = String, start = Start, stop = Stop)
#   }
# }
colnames(Weights) <- Keys$DATA
Weights <- as.data.frame(Weights, stringsAsFactors = FALSE)

for(Col in 5:14){
  Weights[,Col] <- round(as.numeric(Weights[,Col])/10000, 4)
}
str(Weights)
rownames(Weights) <- NULL

Weights$epppnum <- as.numeric(Weights$epppnum)
LongWeights <- c("lgtcy1wt", "lgtcy2wt", "lgtcy3wt", "lgtcy4wt", "lgtcy5wt")
Weights_Long <- melt(data = Weights, id.vars = c('ssuid', 'epppnum'), 
                     measure.vars = LongWeights)
colnames(Weights_Long) <- c('ssuid', 'epppnum', 'lgtcy', 'weight')

Weights_Long$year <- mapvalues(
  x = Weights_Long$lgtcy,
  from = c("lgtcy1wt", "lgtcy2wt", "lgtcy3wt", "lgtcy4wt", "lgtcy5wt"), 
  to = c("2008", "2009", "2010", "2011", "2012")
)

Weights_Long$year <- as.vector(Weights_Long$year)

Weights2013 <- subset(Weights_Long, year == 2012)
Weights2013$year <- 2013

Weights_Long <- rbind(Weights_Long, Weights2013)

Weights_Long <- Weights_Long[order(Weights_Long$ssuid, Weights_Long$epppnum, Weights_Long$year),]
str(Weights_Long)

Filename <- paste0(RDataPath, 'Weights.RData')
save(Weights, file = Filename)

Filename <- paste0(DataPath, 'Weights.txt')
write.table(x = Weights, file = Filename, sep = ',', row.names = F, col.names = T)

Filename <- paste0(RDataPath, 'Weights_Long.RData')
save(Weights_Long, file = Filename)

Filename <- paste0(DataPath, 'Weights_Long.txt')
write.table(x = Weights_Long, file = Filename, sep = ',', row.names = F, col.names = T)
