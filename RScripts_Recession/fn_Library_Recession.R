is.integer0 <- function(x)
{
  is.integer(x) && !length(x)
}

## Function library for Curve registration

fn_get_pValue <- function (lmobject) {
  if (class(lmobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(lmobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(round(p, 6))
}

################################################################## 
## Returns the SE of mean of each row of a dataset
##################################################################
rowSE <- function(Data){
  SE <- apply(X = Data, MARGIN = 1, FUN=function(Row){sd(Row)/sqrt(length(Row))})
  return(SE)
}
################################################################## 

################################################################## 
## Returns the SD of each row of a dataset
##################################################################
rowSD <- function(Data){
  SD <- apply(X = Data, MARGIN = 1, FUN=function(Row){sd(Row)})
  return(SD)
}

colSD <- function(Data){
  rowSD(t(Data))
}
################################################################## 

################################################################## 
## Returns the SD of each row of a dataset
##################################################################
rowVar <- function(Data){
  SD <- apply(X = Data, MARGIN = 1, FUN=function(Row){var(Row)})
  return(SD)
}

colVar <- function(Data){
  rowVar(t(Data))
}
################################################################## 

