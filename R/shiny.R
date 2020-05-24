
#' Create UI with inputs for current page in modal for creating snippets
#'
#' First page is the meta info (Name, Tags, etc). Second to fifth pages are code
#'
#' @param rv reactiveValues()
#'
#' @return UI for the modal
get_modal_page <- function(rv){
  if (is.null(rv[['snip']])){
    rv$snip <- list(page=1)
    return(tagList(
      textInput('snipname', 'Name'),
      textAreaInput('sniptags', 'Tags', rows=3),
      textAreaInput('snipdesc', 'Description', rows=3),
      textAreaInput('snippkgs', 'Packages', rows=3)
    ))
  }
  if (rv$snip$page == 1){
    return(tagList(
      textInput('snipname', 'Name', rv$snip$Name),
      textAreaInput('sniptags', 'Tags', paste(rv$snip$Tags, collapse='\n'), rows=3),
      textAreaInput('snipdesc', 'Description', paste(rv$snip$Description, collapse='\n'), rows=3),
      textAreaInput('snippkgs', 'Packages', paste(rv$snip$Packages, collapse='\n'), rows=3)
    ))
  }else{
    i <- rv$snip$page - 1
    return(
      div(
        id='wide_textarea',
        textAreaInput('snipitem', tags$h3(paste0('Item_', i)), rows=20, width='100%',
                      value=paste(rv$snip[[paste0('Item_', i)]], collapse='\n'))
      )
    )
  }
}

#' Update snippet info in rv
#'
#' Whenever the user changes the page inside the modal for creating snippets the info needs to be updated.
#' Since the code input pages will use the same input bindings.
#'
#' @param rv reactiveValues
#' @param input input
#'
#' @return
update_snip_info <- function(rv, input){
  i <- rv$snip$page
  if (i == 1){
    rv$snip$Name <- input$snipname
    rv$snip$Tags <- input$sniptags
    rv$snip$Description <- input$snipdesc
    rv$snip$Packages <- input$snippkgs
  }else{
    rv$snip[[paste0('Item_', i - 1)]] <- input$snipitem
  }
}
