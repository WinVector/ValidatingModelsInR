
# Render all the lab *.Rmd files to *.html
dir <- getwd()
labs <- list.files(pattern='^Lab.*')
for(li in labs) {
  setwd(li)
  rmds <- list.files(pattern='.*\\.Rmd')
  for(ri in rmds) {
    rmarkdown::render(ri)
  }
  setwd(dir)
}


