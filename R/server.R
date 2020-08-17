
server <- function(input, output, session){

  make_dirs('tmp')

  rv <- reactiveValues(
    d=.pkgenv[['d']]
  )


  # BUTTONS ####

  # ** create ####

  observeEvent(input$create, {
    showModal(get_modal())
  })

  output$modal_body <- renderUI({
    get_modal_body(rv)
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
    id <- if (is.null(rv$snip$id)) get_id(get_filename()) else rv$snip$id
    snip <- unlist(c(
      '# Id ####', id,
      '# Name ####', rv$snip$Name,
      '# Tags ####', rv$snip$Tags,
      '# Description ####', rv$snip$Description,
      '# Packages ####', rv$snip$Packages,
      c(sapply(
        grep('Item_', names(rv$snip), value=T),
        function(x){
          c(
            paste('#', x, '####'),
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

  # ** edit ####

  observeEvent(input$edit, {
    snip <- current_snip()

    rv$snip <- list(
      page=1,
      id=extract_info(snip, 'Id'),
      Name=extract_info(snip, 'Name'),
      Tags=extract_info(snip, 'Tags'),
      Description=extract_info(snip, 'Description'),
      Packages=extract_info(snip, 'Packages')
    )

    headers <- extract_titles(extract_headers(snip))
    for (h in headers){
      rv$snip[[h]] <- extract_info(snip, h, F)
    }

    showModal(get_modal())
  })

  # ** delete ####

  observeEvent(input$delete, {
    if (is.null(input$tbl_rows_selected)) return(NULL)
    id <- rv$d$Id[input$tbl_rows_selected]
    showModal(modalDialog(
      tags$h2(paste0('Deleting ', id, ' (', rv$d$Name[input$tbl_rows_selected], ')')),
      tags$h3('Are you sure?'),
      footer=tagList(
        modalButton('Cancel'),
        actionButton('delete_confirm', 'Confirm', class='btn-danger')
      )
    ))
  })

  observeEvent(input$delete_confirm, {
    removeModal()
    id <- rv$d$Id[input$tbl_rows_selected]
    snip_delete(id=id)
    rv$d <- snip_view()
  })

  # ** export ####

  output$export <- downloadHandler(
    'snippets.zip',
    function(file) snip_export(fname=file)
  )


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

  output$multiple_snips <- reactive({
    req(current_snip())
    return(length(extract_headers(current_snip())) > 1)
  })
  outputOptions(output, 'multiple_snips', suspendWhenHidden=FALSE)

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

  output$snip_title <- renderUI({
    req(current_snip())
    i <- if (is.null(rv$snip_idx)) 1 else rv$snip_idx
    if (i > length(extract_headers(current_snip()))) i <- 1
    h3(extract_titles(extract_headers(current_snip())[i]), class='inl_item')
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
