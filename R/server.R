
server <- function(input, output, session){

  rv <- reactiveValues(
    d=.pkgenv[['d']]
  )


  # CREATE ####

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
      )),
      footer=tagList(
        actionButton('create_cancel', 'Cancel'),
        actionButton('create_save', 'Save')
      )
    ))
  })

  output$modal_body <- renderUI({
    get_modal_page(rv)
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

  observeEvent(input$create_cancel, {
    rv$snip <- NULL
    removeModal()
  })

  observeEvent(input$create_save, {
    update_snip_info(rv, input)
    id <- get_id(get_filename())
    snip <- unlist(c(
      '# Id ####', id,
      '# Name ####', rv$snip$name,
      '# Tags ####', rv$snip$tags,
      '# Description ####', rv$snip$desc,
      '# Packages ####', rv$snip$pkgs,
      c(sapply(
        grep('item_', names(rv$snip), value=T),
        function(x){
          c(
            paste('# Item', sub('item_', '', x), '####'),
            rv$snip[[x]]
          )
        }
      ))
    ))
    fname <- make_fname(id)
    writeLines(snip, fname)
    update_d(fname)
    rv$d <- .pkgenv[['d']]
    rv$snip <- NULL
    removeModal()
  })


  # VIEW ####

  output$tbl <- renderDataTable({
    datatable(rv$d, selection='single')
  })


  # DETAILS ####

  output$out <- renderUI({
    if (is.null(input$tbl_rows_selected)) return(NULL)
    d <- rv$d
    id <- d$Id[input$tbl_rows_selected]
    x <- extract_info(make_fname(id), 'Item 1', filter_comments=F)
    HTML(paste('<pre>', paste(x, collapse='<br>'), '</pre>'))
  })
}
