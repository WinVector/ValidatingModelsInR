
pkgs = c('devtools', 'ROCR', 'ggplot2', 'grid', 'gridExtra', 'plyr',
         'reshape2','dplyr',
         'stringr', 'RColorBrewer', 'boot',
         'rSymPy', 'randomForest', 'e1071', 'vtreat')
for(pkgi in pkgs) {
  if(!require(pkgi,character.only = TRUE)) {
    install.packages(pkgi)
  }
}

if((!require('WVPlots')) && require('devtools')) {
  devtools::install_github("WinVector/WVPlots")
}

if(!require('caret')) {
  tryCatch(install.packages('caret'),
           error = function(e) {print(e)})
}
if(!require('caret')) {
  # work around versions problems in installing caret
  # see: http://stackoverflow.com/questions/35207624/package-pbkrtest-is-not-available-for-r-version-3-2-2
  install.packages('lme4')
  install.packages("https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz",
                   repos=NULL, type="source")
  install.packages('caret')
}
library('caret')
# caret::confusionMatrix() depends on 'e1071'
