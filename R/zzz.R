
.pkgenv <- new.env(parent=emptyenv())
loc <- rappdirs::user_data_dir('snippie')

.onLoad <- function(libname, pkgname){
  library(shiny)
  library(DT)
  if (!dir.exists(loc)) dir.create(loc)
  fname <- file.path(loc, 'data.csv')
  if (file.exists(fname)){
    d <- read.csv(fname)
  }else{
    d <- data.frame(Id='', Name='', Packages='', Tags='', stringsAsFactors=F)[-1, ]
  }
  assign(x='d', value=d, envir=.pkgenv)
}

.onUnload <- function(libname, pkgname){
  write.csv(.pkgenv[['d']], file.path(loc, 'data.csv'), na='', row.names=F)
}
