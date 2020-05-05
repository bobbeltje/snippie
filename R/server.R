
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

  # DELETE ####

  observeEvent(input$delete, {
    if (is.null(input$tbl_rows_selected)) return(NULL)
    id <- rv$d$Id[input$tbl_rows_selected]
    showModal(modalDialog(
      tags$h2(paste0('Deleting ', id, ' (', rv$d$Name[input$tbl_rows_selected], ')')),
      tags$h3('Are you sure?'),
      footer=tagList(
        modalButton('cancel'),
        actionButton('delete_confirm', 'Confirm')
      )
    ))
  })

  observeEvent(input$delete_confirm, {
    removeModal()
    id <- rv$d$Id[input$tbl_rows_selected]
    snip_delete(Id=id)
    rv$d <- snip_view()
  })


  # VIEW ####

  output$tbl <- renderDataTable({
    datatable(rv$d, selection='single')  # , filter='top'
  })


  # DETAILS ####

  current_snip <- reactive({
    if (is.null(input$tbl_rows_selected)) return(NULL)
    id <- rv$d$Id[input$tbl_rows_selected]
    if (is.na(id)) return(NULL)
    readLines(make_fname(id))
  })

  observeEvent(input$left, {
    req(current_snip())
    i <- if (is.null(rv$snip_idx)) 1 else rv$snip_idx
    i <- i - 1
    if (i == 0) i <- length(extract_headers(current_snip()))
    rv$snip_idx <- i
  })

  observeEvent(input$right, {
    req(current_snip())
    i <- if (is.null(rv$snip_idx)) 1 else rv$snip_idx
    i <- i + 1
    if (i > length(extract_headers(current_snip()))) i <- 1
    rv$snip_idx <- i
  })

  output$out <- renderUI({
    req(current_snip())
    snip <- current_snip()
    headers <-
      extract_headers(snip, code_only=TRUE) %>%
      extract_titles()
    i <- if (is.null(rv$snip_idx) || rv$snip_idx > length(headers)) 1L else rv$snip_idx
    x <- extract_info(snip, headers[i], filter_comments=F)
    HTML(paste0('<pre>', paste(x, collapse='<br>'), '</pre>'))
  })
}
