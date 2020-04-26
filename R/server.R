
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
