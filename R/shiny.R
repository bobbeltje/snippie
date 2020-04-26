
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
      textInput('snipname', 'Name', rv$snip$name),
      textAreaInput('sniptags', 'Tags', paste(rv$snip$tags, collapse='\n'), rows=3),
      textAreaInput('snipdesc', 'Description', paste(rv$snip$desc, collapse='\n'), rows=3),
      textAreaInput('snippkgs', 'Packages', paste(rv$snip$pkgs, collapse='\n'), rows=3)
    ))
  }else{
    i <- rv$snip$page - 1
    return(tagList(
      textAreaInput('snipitem', tags$h3(paste('Item', i)),
                    value=paste(rv$snip[[paste0('item_', i)]], collapse='\n'))
    ))
  }
}

update_snip_info <- function(rv, input){
  i <- rv$snip$page
  if (i == 1){
    rv$snip$name <- input$snipname
    rv$snip$tags <- input$sniptags
    rv$snip$desc <- input$snipdesc
    rv$snip$pkgs <- input$snippkgs
  }else{
    rv$snip[[paste0('item_', i - 1)]] <- input$snipitem
  }
}

ui <- function(){
  fluidPage(
    fluidRow(
      column(
        width=12L,
        actionButton('create', 'Create snippet'),
        actionButton('delete', 'Delete snippet')
      )
    ),
    fluidRow(
      column(
        width=6L,
        dataTableOutput('tbl')
      ),
      column(
        width=6L,
        fluidRow(
          column(
            width=12L,
            actionButton('left', '<'),
            actionButton('right', '>')
          )
        ),
        fluidRow(
          column(
            width=12L,
            htmlOutput('out')
          )
        )
      )
    )
  )
}

server <- function(input, output, session){
  rv <- reactiveValues()
  d <- .pkgenv[['d']]

  observeEvent(input$create, {
    showModal(modalDialog(
      fluidRow(column(
        width=12,
        actionButton('modal_left', '<'),
        actionButton('modal_right', '>')
      )),
      fluidRow(column(
        width=12,
        uiOutput('modal_body')
      ))
    ))
  })

  observeEvent(input$modal_left, {
    update_snip_info(rv, input)
    i <- rv$snip$page
    i <- i - 1
    if (i < 1){
      rv$snip$page <- 6
    }else{
      rv$snip$page <- i
    }
  })

  observeEvent(input$modal_right, {
    update_snip_info(rv, input)
    i <- rv$snip$page
    i <- i + 1
    if (i > 6){
      rv$snip$page <- 1
    }else{
      rv$snip$page <- i
    }
  })

  output$modal_body <- renderUI({
    get_modal_page(rv)
  })

  output$tbl <- renderDataTable({
    datatable(d, selection='single')
  })

  output$out <- renderUI({
    if (is.null(input$tbl_rows_selected)) return(NULL)
    id <- d$Id[input$tbl_rows_selected]
    x <- extract_info(make_fname(id), 'Item 1', filter_comments=F)
    HTML(paste('<pre>', paste(x, collapse='<br>'), '</pre>'))
  })
}
