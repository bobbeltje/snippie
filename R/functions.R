
#' Add an Id to the snippet
#'
#' @param fname File location
#'
#' @return Id of the file (either the one already present, or a new one)
add_id <- function(fname){
  snip <- readLines(fname)
  id <- extract_info(snip, 'Id', filter_comments=T)
  if (id == ''){
    id <- get_id(get_filename())
    snip <- c('# Id ####', id, snip)
    writeLines(snip, fname)
  }
  return(id)
}

#' Extract section info from snippet
#'
#' @param fname File name of snippet
#' @param type The section
#' @param filter_comments Bool
#'
#' @return The information as character vector
#'
extract_info <- function(snip, type, filter_comments=TRUE){
  snip <- snip[!grepl('^[[:space:]]*$', snip)]

  i <- grep(paste0('^# ', type, ' ####$'), snip)
  if (length(i) != 1L) return('')

  sections <- grep('^#.*####$', snip)
  if (i == sections[length(sections)]){
    next_section <- length(snip) + 1
  }else{
    next_section <- sections[sections > i][[1]]
  }

  if (i + 1L == next_section) return('')

  x <- snip[(i+1L):(next_section-1L)]
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

make_fname <- function(id){
  file.path(loc, 'snippets', paste0('snip_', id, '.R'))
}

#' Create a skeleton for your snippet
#'
#' This will create a temporary file to create a snippet.
#' Once done, save it with snip_save().
#'
#' @param snip_id If NULL then new snip, else edit snippet identified by snip_id
#'
#' @return
#' @export
snip_create <- function(snip_id=NULL){
  if (is.null(snip_id)){
    src <- system.file('templates/snip_skeleton.R', package='snippie')
  }else{
    src <- file.path(loc, 'snippets', paste0('snip_', snip_id, '.R'))
  }
  dst <- tempfile('snip', fileext='.R')
  .pkgenv[['snip']] <- dst
  file.copy(src, dst)
  file.edit(dst)
  invisible(TRUE)
}

#' Delete a snippet
#'
#' Use either the snippet Id or its row number from snip_view to select the snippet to delete.
#'
#' @param i The row number as indicated by snip_view()
#' @param Id The snippet's Id
#'
#' @return
#' @export
#'
#' @examples snip_delete(i=1)
#' snip_delete(Id=7059297)
snip_delete <- function(i=NULL, Id=NULL){
  if (is.null(i) && is.null(Id)) stop('Specify i (the row index from snip_view) or Id')
  if (!is.null(i) && !is.null(Id)) stop('Specify snippet with either i or Id')
  d <- .pkgenv[['d']]
  if (!is.null(i)){
    Id <- d$Id[i]
  }
  d <- d[d$Id != Id, ]
  .pkgenv[['d']] <- d
  unlink(file.path(loc, 'snippets', paste0('snip_', Id, '.R')))
  message('Snippet deleted')
  return(Id)
}

#' Attempt to fix snippie by reloading snippet information
#'
#' @return
#' @export
#'
#' @examples snip_fix()
snip_fix <- function(play_it_safe=TRUE){
  files <- dir(file.path(loc, 'snippets'), full.names=T)
  zip(file.path(loc, 'backup.zip'))
  l <- lapply(files, function(fname){
    # deleting invalid files
    if (!isTRUE(validate_snip(fname))){
      unlink(x)
      return(NULL)
    }
    add_id(fname)
    snip <- readLines(fname)
    data.frame(
      Id=extract_info(snip, 'Id'),
      Name=extract_info(snip, 'Name'),
      Packages=paste(extract_info(snip, 'Packages'), collapse=', '),
      Tags=paste(extract_info(snip, 'Tags'), collapse=', ')
    )
  })
  d <- do.call(rbind, l)
  .pkgenv[['d']] <- d
}

#' Run snippie interactively
#'
#' @export
snip_interactive <- function(){
  shiny::shinyApp(ui=ui(), server=server)
}

#' Save snippet
#'
#' This will save the latest opened snippet.
#'
#' @return
#' @export
snip_save <- function(){
  f_old <- .pkgenv[['snip']]

  v <- snip_validate(f_old)
  if (!v) return(NULL)

  id <- add_id(f_old)
  f_new <- file.path(loc, 'snippets', paste0('snip_', id, '.R'))
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
#' @param n Can be used to view and edit all information regarding a specific snippet.
#' n refers to the row.number returned by snip_view()
#' @param exact How precise the subsetting should be. Exact==TRUE is not fully exact (yet).
#'
#' @return The snippets as data.frame
#' @export
#'
#' @examples
#' snip_view(Package=data.table, Tags=column)
#' snip_view(p=plotly)
snip_view <- function(n, exact=F, ...){
  if (!missing(n)){
    snip_create(.pkgenv[['d']]$Id[n])
    return(invisible())
  }
  f <- if (exact) grepl else agrepl
  d <- .pkgenv[['d']]
  l <- as.list(substitute(...()))
  for (i in names(l)){
    col <- grep(toupper(substring(i, 1, 1)), substr(colnames(d), 1, 1))
    if (length(col) != 1L) next
    col <- colnames(d)[col]
    d <- d[f(l[[i]], d[[col]]), ]
  }
  return(d)
}

#' Update data.frame with available snippets
#'
#' @param fname Filename where latest snip got saved
#'
#' @return invisible(TRUE) when successful
#'
update_d <- function(fname){
  id <- get_id(fname)
  snip <- readLines(fname)
  d <- .pkgenv[['d']]
  if (id %in% d$Id){
    d[d$Id == id, c('Name', 'Packages', 'Tags')] <- c(
      extract_info(snip, 'Name'),
      paste(extract_info(snip, 'Packages'), collapse=', '),
      paste(extract_info(snip, 'Tags'), collapse=', ')
    )
    .pkgenv[['d']] <- d
  }else{
    df <- data.frame(
      Id=id,
      Name=extract_info(snip, 'Name'),
      Packages=paste(extract_info(snip, 'Packages'), collapse=', '),
      Tags=paste(extract_info(snip, 'Tags'), collapse=', ')
    )
    .pkgenv[['d']] <- rbind(.pkgenv[['d']], df)
  }
  invisible(TRUE)
}

#' Validate snippet before saving
#'
#' @param fname Location of snippet
#'
#' @return TRUE if successful, otherwise it calls stop
validate_snip <- function(fname){
  # fname <- snippie:::.pkgenv$snip
  x <- c('Name', 'Tags', 'Description', 'Packages')
  l <- sapply(x, extract_info, fname=fname, simplify=F, USE.NAMES=T)
  if (typeof(l$Name) != 'character' || length(l$Name) != 1 || nchar(l$Name) < 1){
    return('Invalid Name')
  }
  if (typeof(l$Tags) != 'character'){
    return('Invalid Tags')
    if (length(l$Tags) > 10) return('Too Many Tags (maximum allowed: 10)')
  }
  if (typeof(l$Description) != 'character'){
    return('Invalid Description')
    if (length(l$Description) > 10) return('Too Long Description (maximum lines allowed: 10)')
  }
  if (typeof(l$Packages) != 'character'){
    return('Invalid Packages')
    if (length(l$Packages) > 10) return('Too Many Packages (maximum allowed: 10)')
  }
  return(invisible(TRUE))
}

