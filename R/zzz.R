
.pkgenv <- new.env(parent=emptyenv())
loc <- rappdirs::user_data_dir('snippie')

.onLoad <- function(libname, pkgname){
  library(shiny)
  library(DT)
  if (!dir.exists(loc)) dir.create(loc, recursive=TRUE)
  fname <- file.path(loc, 'data.csv')
  if (file.exists(fname)){
    d <- data.table::fread(fname)
  }else{
    d <- data.table::data.table(Id=0, Name='', Packages='', Tags='')[-1, ]
  }
  assign(x='d', value=d, envir=.pkgenv)
}

.onUnload <- function(libname, pkgname){
  data.table::fwrite(.pkgenv[['d']], file.path(loc, 'data.csv'), na='', row.names=F)
}
