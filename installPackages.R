


wlevel = getOption("warn")
options(warn=-1)
pkgs = c('devtools', 'ROCR', 'ggplot2', 'grid', 'gridExtra', 'plyr',
         'reshape2', 'dplyr', 'pROC', 'knitr', 'rmarkdown',
         'stringr', 'RColorBrewer', 'boot',
         'rSymPy', 'randomForest', 'e1071', 'vtreat')
for(pkgi in pkgs) {
  if(!require(pkgi,character.only = TRUE, 
              warn.conflicts=FALSE, quietly=TRUE)) {
    suppressMessages(install.packages(pkgi))
  }
}

if((!require('WVPlots',warn.conflicts=FALSE, quietly=TRUE)) && 
   require('devtools',warn.conflicts=FALSE, quietly=TRUE)) {
  devtools::install_github("WinVector/WVPlots",
                           build_vignettes = TRUE)
}

if(!require('caret', warn.conflicts=FALSE, quietly=TRUE)) {
  tryCatch(suppressMessages(install.packages('caret')),
           error = function(e) {print(e)})
}
if(!require('caret', warn.conflicts=FALSE, quietly=TRUE)) {
  # work around versions problems in installing caret
  # see: http://stackoverflow.com/questions/35207624/package-pbkrtest-is-not-available-for-r-version-3-2-2
  suppressMessages(install.packages('lme4'))
  suppressMessages(install.packages("https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz",
                                    repos=NULL, type="source"))
  suppressMessages(install.packages('caret'))
}
library('caret',warn.conflicts=FALSE, quietly=TRUE, verbose=FALSE)
# caret::confusionMatrix() depends on 'e1071'
options(warn=wlevel)

