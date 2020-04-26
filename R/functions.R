
#' Extract section info from snippet
#'
#' @param fname File name of snippet
#' @param type The section
#' @param filter_comments Bool
#'
#' @return The information as character vector
#'
extract_info <- function(fname, type, filter_comments=FALSE){
  x <- readLines(fname)
  x <- x[!grepl('^[[:space:]]*$', x)]

  i <- grep(paste0('^# ', type, ' ####$'), x)
  sections <- grep('^#.*####$', x)
  if (i == sections[length(sections)]){
    next_section <- length(x) + 1
  }else{
    next_section <- sections[sections > i][[1]]
  }

  if (i + 1L == next_section) return('')

  x <- x[(i+1L):(next_section-1L)]
  if (filter_comments){
    x <- grep('^#', x, value=T, invert=T)
  }
  return(x)
}

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

#' Get the snip id from location
#'
#' @param x Character string with snip location or snip name
#'
#' @return The id as character
get_id <- function(x){
  x <- basename(x)
  x <- sub('.R', '', sub('snip_', '', x))
  return(x)
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
  f_old <- .pkgenv[['snip']]
  # v <- snip_validate(x)
  # if (!v) return(NULL)
  f_new <- file.path(loc, 'snippets', get_filename())
  file.copy(f_old, f_new)
  update_d(f_new)
  message('Snippet saved!')
  invisible(TRUE)
}

#' View available snips
#'
#' Without arguments, all snippets are shown. Select a subset using ...
#'
#' @param ... Subset on any of the columns, eg, snip_view(Package=data.table, Tags=column)
#'
#' @return The snippets as data.frame
#' @export
#'
#' @examples snip_view(Package=data.table, Tags=column); snip_view(p=plotly)
snip_view <- function(exact=F, ...){
  f <- if (exact) grepl else agrepl
  d <- .pkgenv[['d']]
  l <- as.list(substitute(...()))
  for (i in names(l)){
    col <- grep(toupper(i), substr(colnames(d), 1, 1))
    if (length(col) != 1L) next
    col <- colnames(d)[col]
    d <- d[f(l[[i]], d[[col]]), ]
  }
  return(d)
}

#' Update data.frame with available snippets
#'
#' @param x Filename where latest snip got saved
#'
#' @return invisible(TRUE) when successful
#'
update_d <- function(x){
  df <- data.frame(
    Id=get_id(x),
    Name=extract_info(x, 'Name'),
    Packages=paste(extract_info(x, 'Packages'), collapse=', '),
    Tags=paste(extract_info(x, 'Tags'), collapse=', ')
  )
  .pkgenv[['d']] <- rbind(.pkgenv[['d']], df)
  invisible(TRUE)
}
