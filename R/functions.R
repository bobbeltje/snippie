
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

#' Add a section to a snip
#'
#' The function does not verify if the section exists already
#'
#' @param snip A snippet
#' @param section Section header
#' @param x Content
#'
#' @return An updated snippet
add_section <- function(snip, section, x){
  snip <- c(paste('#', section, '####'), x, snip)
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
  type <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", type)  # escape regex characters from type
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

#' Interactively get a path to folder
#'
#' @param caption Window caption
#'
#' @return Path
get_path_to_folder <- function(caption){
  if (nzchar(system.file(package='utils'))){
    if (exists('tk_choose.dir', where=asNamespace('utils'), mode='function')) {
      return(utils::choose.dir(caption = caption))
    }
  }
  if (nzchar(system.file(package='rstudioapi'))){
    if (exists('selectDirectory', where=asNamespace('rstudioapi'), mode='function')) {
      return(rstudioapi::selectDirectory(caption = caption))
    }
  }
  if (nzchar(system.file(package='tcltk'))){
    if (exists('tk_choose.dir', where=asNamespace('tcltk'), mode='function')) {
      return(tcltk::tk_choose.dir(caption = caption))
    }
  }
  message('Using home folder')
  return('~')
}

#' Make dirs
#'
#' Create the directories specified in x
#'
#' @param x A character vector of dirs
make_dirs <- function(x){
  for (i in x){
    if (!dir.exists(i)) dir.create(i)
  }
}

#' Create full path to file (in saved snippets folder) given an id
#'
#' @param id Id of a snippet
#'
#' @return File path for snippet
make_fname <- function(id){
  file.path(loc, 'snippets', paste0('snip_', id, '.R'))
}

#' Read snippet specified by row number of snip_view
#'
#' Read a snippet from file. Specify snippet with row number in snip_view
#'
#' @param i Row number
#'
#' @return Character vector with snippet
read_snip <- function(i){
  l <- validate_i_and_id(i, NULL)
  readLines(make_fname(l$id))
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

#' Remove invalid headers
#'
#' Removes trailing pound signs from any header that is not one of the main ones, or an Item_ one
#'
#' @param snip A snippe
#'
#' @return A snippet with invalid headers removed
remove_invalid_headers <- function(snip){
  headers <- extract_headers(snip)
  main <- c("# Id ####", "# Name ####", "# Tags ####", "# Description ####", "# Packages ####")
  headers <- headers[!headers %in% main]
  invalid_headers <- character()
  for (h in headers){
    x <- sub('^# Item_', '', sub(' ####$', '', h))
    if (is.na(suppressWarnings(as.integer(x)))){
      invalid_headers <- c(invalid_headers, h)
    }
  }
  snip[snip %in% invalid_headers] <- sub('#*$', '', snip[snip %in% invalid_headers])
  return(snip)
}

#' Delete a snippet
#'
#' Use either the snippet Id or its row number from snip_view to select the snippet to delete.
#'
#' @param i The row number as indicated by snip_view()
#' @param id The snippet's Id
#'
#' @return
#' @export
#'
#' @examples snip_delete(i=1)
#' snip_delete(id=7059297)
snip_delete <- function(i=NULL, id=NULL){
  l <- validate_i_and_id(i, id)
  .pkgenv[['d']] <- .pkgenv[['d']][-l$i, ]
  unlink(file.path(loc, 'snippets', paste0('snip_', l$id, '.R')))
  message('Snippet deleted')
  return(invisible(l$id))
}

#' Edit a new snippet
#'
#' Edit an existing snippet.
#' Edit the file. Once done, save the file and save the changes with snip_save().
#'
#' @return
#' @export
snip_edit <- function(i=NULL, id=NULL){
  l <- validate_i_and_id(i, id)
  src <- file.path(loc, 'snippets', paste0('snip_', l$id, '.R'))
  dst <- tempfile('snip', fileext='.R')
  .pkgenv[['snip']] <- dst
  file.copy(src, dst)
  file.edit(dst)
  invisible(TRUE)
}

#' Exports snippets a zip file
#'
#' Choose a directory interactively (or specify directly with path argument).
#'
#' @param path Path to directory. Leave empty to choose one interactively.
#' @param fname Name of the exported zip
#'
#' @export
snip_export <- function(path=NULL, fname='snippets.zip'){
  shiny_call <- nzchar(system.file(package='shiny')) && !is.null(shiny::getDefaultReactiveDomain())

  if (!shiny_call){
    if (is.null(path)) path <- get_path_to_folder('Select directory to save the snippets')
    if (is.null(path) || is.na(path)) stop('Unable to save snippets; path not valid')
  }

  files <- dir(file.path(loc, 'snippets'), full.names=T)
  fname <- if (shiny_call) fname else file.path(path, fname)
  if (file.exists(fname)) file.remove(fname)
  zip(fname, files, flags='-jqFS')

  if (!shiny_call) message('snippets.zip saved in ', path)
  invisible(NULL)
}

#' Attempt to fix snippie by reloading snippet information
#'
#' @param play_it_safe Bool, if TRUE (default) creates a zip of all saved snippets
#'
#' @return data.frame of available snippets
#' @export
#'
#' @examples snip_fix()
snip_fix <- function(play_it_safe=TRUE){
  files <- dir(file.path(loc, 'snippets'), full.names=T)
  if (play_it_safe) zip(file.path(loc, 'backup.zip'), files, flags='-jqFS')
  l <- lapply(files, function(fname){
    snip <- readLines(fname)
    # deleting invalid files
    if (!isTRUE(validate_snip(snip=snip))){
      unlink(fname)
      return(NULL)
    }
    snip <- add_id(snip)

    snip_id <- extract_info(snip, 'Id')
    fname_id <- get_id(fname)

    if (snip_id != fname_id){
      unlink(fname)
      writeLines(snip, make_fname(snip_id))
    }

    data.frame(
      Id=snip_id,
      Name=extract_info(snip, 'Name'),
      Packages=paste(extract_info(snip, 'Packages'), collapse=', '),
      Tags=paste(extract_info(snip, 'Tags'), collapse=', '),
      stringsAsFactors=F
    )
  })
  d <- do.call(rbind, l)
  # d$Id <- as.integer(as.character(d$Id))
  .pkgenv[['d']] <- d
  return(invisible(d))
}

#' Select a folder full op undocumented snippets to add to the snippie library
#'
#' All files in a folder can be added to snippie in one go.
#' snippie will use the file name as Title and add an Id.
#'
#' @param path Path to directory. Leave empty to choose one interactively.
#' @param recursive Boolean
#' @param extensions A character vector of extensions, eg c('py', 'R', 'txt')
#'
#' @return
#' @export
#'
#' @examples snip_folder(recursive=TRUE, extensions=c('txt', 'R'))
snip_folder <- function(path=NULL, recursive=TRUE, extensions=NULL){

  if (is.null(path)) path <- get_path_to_folder('Select folder with undocumented snippets')
  if (is.null(path) || is.na(path)) stop('Unable to save snippets; path not valid')

  Tags <- readline('Add tags (comma separated): ')
  Packages <- readline('Add packages (comma separated): ')

  if (is.null(extensions)){
    pattern <- NULL
  }else{
    pattern <- paste0('.', extensions, '$')
  }
  files <- dir(path, pattern=pattern, full.names=T, recursive=recursive)

  for (fname in files){
    readLines(fname) %>%
      remove_invalid_headers() %>%
      add_section('Item_1', '') %>%
      add_section('Packages', Packages) %>%
      add_section('Description', '') %>%
      add_section('Tags', Tags) %>%
      add_section('Name', sub("(.*)\\..*$", "\\1", basename(fname))) %>%
      add_id() %>%
      writeLines(., make_fname(extract_info(., 'Id')))
  }
  message('snippets added, updating data')
  snip_fix()
  message('done')
}

#' Import snippets from zip file
#'
#' @export
snip_import <- function(){
  fname <- try(file.choose(), silent=T)
  if (inherits(fname, 'try-error')) return(invsible(NULL))

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

#' Create a new snippet
#'
#' Create a new snippet.
#' This will create a template to fill in.
#' Once done, save the file and add it to your snippets with snip_save().
#'
#' @return
#' @export
snip_new <- function(){
  src <- system.file('templates/snip_skeleton.R', package='snippie')
  dst <- tempfile('snip', fileext='.R')
  .pkgenv[['snip']] <- dst
  file.copy(src, dst)
  file.edit(dst)
  invisible(TRUE)
}

#' Save snippet
#'
#' This will save the latest opened snippet.
#'
#' @return
#' @export
snip_save <- function(){
  f_old <- .pkgenv[['snip']]
  snip <- readLines(f_old)

  v <- validate_snip(snip=snip)
  if (!v) return(NULL)

  snip <- add_id(snip)
  id <- extract_info(snip, 'Id', filter_comments=TRUE)
  f_new <- file.path(loc, 'snippets', paste0('snip_', id, '.R'))
  writeLines(snip, f_new)

  update_d(snip=snip, id=id)
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
#' @return The snippets as data.frame
#' @export
#'
#' @examples
#' snip_view(Package=data.table, Tags=column)
#' snip_view(p=plotly)
snip_view <- function(n, exact=F, ...){
  if (!missing(n)){
    snip_edit(id=.pkgenv[['d']]$Id[n])
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
update_d <- function(fname, snip=NULL, id=NULL){
  if (is.null(id)) id <- get_id(fname)
  if (is.null(snip)) snip <- readLines(fname)
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
update_snip <- function(fname, f){
  readLines(fname) %>%
    f() %>%
    writeLines(fname)
}

#' Validate i and id arguments
#'
#' Functions that use either i (row index) or id (snip id) can use
#' this function to validate correct input
#'
#' @param i The row index for the snippet
#' @param id The id of the snippet
#'
#' @return Either calls stop or returns list with i and id
validate_i_and_id <- function(i, id){
  if (is.null(i) && is.null(id)) stop('Either i or id needs to be provided', call.=F)
  if (!is.null(i) && !is.null(id)) stop('Either i or id needs to be provided, not both', call.=F)

  if (!is.null(i)){
    if (length(i) > 1) stop('length of i > 1', call.=F)
    if (i > nrow(.pkgenv$d)){
      stop(paste0('i (', i, ') is larger than available snippets (', nrow(.pkgenv$d), ')'), call.=F)
    }
    return(list(i=i, id=.pkgenv$d$Id[i]))
  }

  if (!is.null(id)){
    if (length(id) > 1) stop('length of id > 1', call.=F)
    if (!id %in% .pkgenv$d$Id) stop(paste0('id (', id, ') is not a valid snippet id'), call.=F)
    return(list(id=id, i=which(.pkgenv$d$Id == id)))
  }
}

#' Validate snippet before saving
#'
#' @param fname Location of snippet
#'
#' @return TRUE if successful, otherwise it calls stop
validate_snip <- function(fname, snip=NULL){
  # fname <- snippie:::.pkgenv$snip
  x <- c('Name', 'Tags', 'Description', 'Packages')
  if (is.null(snip)) snip <- readLines(fname)
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

