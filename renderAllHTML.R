
# Render all the lab *.Rmd files to *.html
dir <- getwd()
labs <- list.files(pattern='^Lab.*')
for(li in labs) {
  setwd(li)
  rmds <- list.files(pattern='.*\\.Rmd')
  for(ri in rmds) {
    knitr::knit2html(ri)
  }
  setwd(dir)
}


