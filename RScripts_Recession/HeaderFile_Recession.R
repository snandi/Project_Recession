# library( doParallel )
# library( foreach )
library( ggplot2 )
library( ggthemes )
# library( Hmisc )
library( lme4 )
library( lmerTest )
library( MASS )
library( Matrix )
# library( parallel )
library( plyr )
library( dplyr )
#library( questionr )
library( reshape2 )
library( RFunctionsSN )
#library( stargazer )
#library( survey )
# library( survival )
library( xtable )
library( zoo )

# fn_getPackages <- function( ){
#   Packages <- c( 
#     'boot',  
#     'car',
#     'caret',                        ## For prediction algorithms like boosting, random forest, etc 
#     'chron', 
#     'epicalc', 
#     'faraway', 
#     'fda', 
#     'fdakma',
#     'fdasrvf',
#     'fda.usc',                      ## For functional data depth
#     'flexclust',                    ## distance measures and centroid computation for clustering
#     'foreach',
#     'forecast',                     ## for time series modeling  
#     'foreign', 
#     'fpc',                          ## For cluster comparisons ## Wont install on lmcg
#     'gam', 
#     'glmnet', 
#     'gmodels',
#     'ggfortify',                    ## For plotting lm( ) and glm( ) diagnostic plots
#     'ggplot2',
#     'GGally',                       ## For plotting functions like ggpairs
#     'graphics', 
#     'grid',                         ## Needed by animint
#     'gridExtra', 
#     'gsubfn', 
#     'gtools', 
#     'hexbin',                       ## Needed by animint
#     'Hmisc',
#     'inline', 
#     'iterators', 
#     'jpeg',	                    ## To read/write jpg images
#     'kernlab', 
#     'knitr',
#     'kSamples',                     ## For k-sample Anderson-Darling test
#     'lme4',
#     'lmerTest',            
#     'lmtest',  
#     'longitudinalData', 
#     'lpSolve', 
#     # 'lsmeans',
#     'MASS',
#     'matrixStats', 
#     'mclust',
#     'memisc',                       ## For better display of outputs in R markdown
#     'mgcv', 
#     'modeest',
#     'NCStats',                      ## For chi-square post hoc tests
#     'nlme', 
#     'nortest', 
#     'numDeriv',                     ## To calculate gradient, hessian, jacobian of functions
#     'nws',
#     'orthogonalsplinebasis',
#     'pander',                       ## For better R markdown outputs
#     'pdfCluster',   	            ## Cluster pdfs from mixtures, non-parametric
#     'pgmm',                         ## Used in Coursera machine learning course
#     'phangorn', 
#     'plotrix', 
#     'pls', 
#     'plyr', 
#     'PMCMR',                        ## For multiple comparison & post hoc test
#     'png',                          ## To read and write png images
#     'profr',  
#     'proto',                        ## Needed by animint
#     'psych', 
#     'quantreg',
#     'rattle',                       ## To get better plots of regression tree outputs
#     'R2HTML', 
#     'rbenchmark', 
#     'readr',  		            ## Read flat/tabular text files from disk
#     'readstata13',                  ## Read stata 13 objects
#     'refund',                       ## Required by Pomann_Staicu_etal 2 sample tests
#     'reshape',  
#     'reshape2',  
#     'RMySQL',   
#     'robustbase', 
#     'robustX',                      ## For multivariate median and other experimental stats
#     'rpart',                        ## Used in Coursera machine learning course
#     'RJSONIO',                      ## Required by animint
#     'Rsymphony', 
#     'sandwich', 
#     'scales',                       ## Needed by animint
#     'SenSrivastava',
#     'seqinr',                       
#     'servr',                        ## To plot html & js objects produced by animint
#     'sets',                         ## Set operations      
#     'slam',   
#     'SnowballC',                    ## For text mining
#     'stargazer',                    ## Export tables from different regression outputs into latex
#     'stats', 
#     'stringr',                      ## For regular expressions
#     'survey', 
#     'svMisc',
#     'tm',                           ## Text mining package
#     'UsingR',    
#     'vrtest', 
#     'wle', 
#     'wordcloud', 
#     'xgboost',                      ## Extreme Gradient Boosting
#     'XML', 
#     'xtable', 
#     'zipcode'
#   )
#   return( Packages )  
# }
# 
# Packages <- fn_getPackages( )
# 
# ## Choose USA ( IA ) as the CRAN mirror
# Mirrors <- getCRANmirrors( all = FALSE, local.only = FALSE )
# chooseCRANmirror( graphics = F, ind = which( Mirrors$Name == 'USA ( IA )' ) )
# 
# Packages_Installed <- Packages
# ## For loop for requiring packages and installing them if something doesnt exist
# for( Package in Packages ){
#   if( require( package=Package, character.only = T, quietly = T ) == F ){
#     print( paste( Package, 'not Installed' ) )
#     Packages_Installed <- Packages_Installed[Packages_Installed != Package]
#     #     try( install.packages( Package, dependencies = TRUE ) )
#   } else{
#     #    print( paste( Package, 'already exists' ) )
#     require( package = Package, character.only = T, quietly = T )
#   }
# }
# 
# ## For parallel processing, when passing the list of packages to load
# ## in all the cores. Could be different from Packages
# MyAutoLoads <- Packages_Installed
# Packages_Par <- MyAutoLoads
