
#' Filename for new snip
#'
#' Create filename to save a new snip
#'
#' @return path_to_file
get_filename <- function(n=10L){
  if (n < 1){
    stop('Failed to generate file name')
  }
  loc <- file.path(loc, 'snippets')
  if (!dir.exists(loc)) dir.create(loc)
  x <- sample(1e7, 1L)
  fname <- paste0('snip_', x, '.R')
  if (file.exists(fname)) return(get_filename(n - 1L))
  return(fname)
}

#' Create a skeleton for your snippet
#'
#' This will create a temporary file to create a snippet
#' Once done, save it with snip_save()
#'
#' @return
#' @export
snip_create <- function(){
  loc <- tempfile('snip', fileext='.R')
  .pkgenv[['snip']] <- loc
  file.copy(system.file('templates/snip_skeleton.R', package='snippie'), loc)
  file.edit(loc)
  invisible(TRUE)
}

#' Save snippet
#'
#' This will save the latest opened snippet
#'
#' @return
#' @export
snip_save <- function(){
  x <- .pkgenv[['snip']]
  # v <- snip_validate(x)
  # if (!v) return(NULL)
  fname <- get_filename()
  file.copy(x, file.path(loc, 'snippets', fname))
  # update_d()
  message('Snippet saved!')
  invisible(TRUE)
}
