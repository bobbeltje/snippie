
.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname){
  loc <- rappdirs::user_data_dir('snippie')
  if (!dir.exists(loc)) dir.create(loc)
  fname <- file.path(loc, 'data.csv')
  if (file.exists(fname)){
    d <- read.csv(fname)
  }else{
    d <- data.frame(topic='', package='', name='', stringsAsFactors=F)[-1, ]
  }
  assign(x='d', value=d, envir=.pkgenv)
}

.onUnload <- function(libname, pkgname){
  loc <- rappdirs::user_data_dir('snippie')
  write.csv(.pkgenv[['d']], file.path(loc, 'data.csv'), na='', row.names=F)
}
