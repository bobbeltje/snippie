
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
}
