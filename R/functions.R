
#' Add an Id to the snippet
#'
#' @param snip The snippet
#'
#' @return Id of the file (either the one already present, or a new one)
add_id <- function(snip){
  id <- extract_info(snip, 'Id', filter_comments=T)
  if (id == ''){
    id <- get_id(get_filename())
    snip <- c('# Id ####', id, snip)
  }
  return(snip)
}

#' Extract headers from a snippet
#'
#' @param snip The snippet
#' @param code_only If true, only the ones with code
#'
#' @return Character vector of headers
extract_headers <- function(snip, code_only=T){
  headers <- grep(paste0('^# .* ####$'), snip, value=T)
  if (code_only) {
    main <- c("# Id ####", "# Name ####", "# Tags ####", "# Description ####", "# Packages ####")
    headers <- headers[!headers %in% main]
  }
  return(headers)
}

#' Extract section info from snippet
#'
#' @param snip A snippet
#' @param type Section to extract (Name, Tags, Description, Packages, etc)
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

#' Extract code section title from string
#'
#' Gets rid of leading and trailing pound signs
#'
#' @param x Header(s)
#'
#' @return Title(s)
extract_titles <- function(x){
  sub(' ####$', '', sub('^# ', '', x))
}

#' Filename for new snip
#'
#' Create filename to save a new snip
#'
#' @param n Recursive counter
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
#' @param fname Character string with snip location or snip name
#'
#' @return The id as character
get_id <- function(fname){
  fname <- basename(fname)
  fname <- sub('.R', '', sub('snip_', '', fname))
  return(fname)
}

#' Create full path to file (in saved snippets folder) given an id
#'
#' @param id Id of a snippet
#'
#' @return File path for snippet
make_fname <- function(id){
  file.path(loc, 'snippets', paste0('snip_', id, '.R'))
}

#' Remove Id from a snippet
#'
#' @param snip The snippet
#'
#' @return The snippet minus Id
remove_id <- function(snip){
  headers <- extract_headers(snip, code_only=F)
  if (!'# Id ####' %in% headers) return(snip)

  i <- which(headers == '# Id ####') + 1
  if (i > length(headers)){
    snip <- snip[-(which(snip == '# Id ####'):length(snip))]
  }else{
    snip <- snip[-(which(snip == '# Id ####') : (which(snip == headers[i]) - 1))]
  }

  return(snip)
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

#' Exports snippets a zip file
#'
#'Choose a directory interactively (or specify directly with path argument).
#'
#' @param path Path to directory. Leave empty to choose one interactively.
#'
#' @export
snip_export <- function(path=NULL){

  if (is.null(path)){
    caption <- 'Select directory to save the snippets'
    if (exists('tk_choose.dir', where=asNamespace('utils'), mode='function')) {
      path <- utils::choose.dir(caption = caption)
    } else if (exists('selectDirectory', where=asNamespace('rstudioapi'), mode='function')) {
      path <- rstudioapi::selectDirectory(caption = caption)
    } else if (exists('tk_choose.dir', where=asNamespace('tcltk'), mode='function')) {
      path <- tcltk::tk_choose.dir(caption = caption)
    }else{
      path <- '~'
    }
  }

  if (is.null(path) || is.na(path)){
    stop('Unable to save snippets; path not valid')
  }

  files <- dir(file.path(loc, 'snippets'), full.names=T)
  zip(file.path(path, 'snippets.zip'), files, flags='-jq')

  message('Snippets saved in ', path)
}

#' Attempt to fix snippie by reloading snippet information
#'
#' @param play_it_safe Bool, if TRUE (default) creates a zip of all saved snippets
#'
#' @return data.table of available snippets
#' @export
#'
#' @examples snip_fix()
snip_fix <- function(play_it_safe=TRUE){
  files <- dir(file.path(loc, 'snippets'), full.names=T)
  zip(file.path(loc, 'backup.zip'), files)
  l <- lapply(files, function(fname){
    # deleting invalid files
    if (!isTRUE(validate_snip(fname))){
      unlink(x)
      return(NULL)
    }
    update_snip(fname, add_id)
    snip <- readLines(fname)
    data.frame(
      Id=extract_info(snip, 'Id'),
      Name=extract_info(snip, 'Name'),
      Packages=paste(extract_info(snip, 'Packages'), collapse=', '),
      Tags=paste(extract_info(snip, 'Tags'), collapse=', ')
    )
  })
  d <- data.table::rbindlist(l)
  .pkgenv[['d']] <- d
  return(invisible(d))
}

#' Import snippets from zip file
#'
#' @export
snip_import <- function(){
  fname <- try(file.choose(), silent=T)
  if (inherits(fname, 'try-error')) return(NULL)

  path <- file.path(tempdir(), 'new_snippets')
  unzip(fname, exdir=path)

  files <- dir(path)

  new_files <- files[files %in% dir(loc)]
  file.rename(file.path(path, new_files), file.path(loc, 'snippets', new_files))

  files <- files[!files %in% new_files]
  for (fname in files){
    snip <- readLines(file.path(path, fname))
    snip <- snip %>%
      remove_id() %>%
      add_id()
    writeLines(snip, make_fname(extract_info(snip, 'Id')))
    file.remove(file.path(path, fname))
  }
  snip_fix()
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

  v <- validate_snip(f_old)
  if (!v) return(NULL)

  id <- update_snip(f_old, add_id)
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
#' @param n Can be used to view and edit all information regarding a specific snippet.
#' n refers to the row.number returned by snip_view()
#' @param exact How precise the subsetting should be. Exact==TRUE is not fully exact (yet).
#' @param ... Subset on any of the columns, eg, snip_view(Package=data.table, Tags=column)
#'
#' @return The snippets as data.table
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

#' Function wrapper to update snippet
#'
#' @param fname File name of snippet
#' @param f Function to apply to the snippet
#'
#' @export
update_snip <- function(fname, f){
  readLines(fname) %>%
    f() %>%
    writeLines(fname)
}

#' Validate snippet before saving
#'
#' @param fname Location of snippet
#'
#' @return TRUE if successful, otherwise it calls stop
validate_snip <- function(fname){
  # fname <- snippie:::.pkgenv$snip
  x <- c('Name', 'Tags', 'Description', 'Packages')
  snip <- readLines(fname)
  l <- sapply(x, extract_info, snip=snip, simplify=F, USE.NAMES=T)
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

