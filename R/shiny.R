
ui <- function(){
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width=12L,
        shiny::actionButton('create', 'Create snippet'),
        shiny::actionButton('delete', 'Delete snippet')
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width=6L,
        DT::dataTableOutput('tbl')
      ),
      shiny::column(
        width=6L,
        shiny::fluidRow(
          shiny::column(
            width=12L,
            shiny::actionButton('left', '<'),
            shiny::actionButton('right', '>')
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width=12L,
            shiny::htmlOutput('out')
          )
        )
      )
    )
  )
}

server <- function(input, output, session){

  d <- .pkgenv[['d']]

  output$tbl <- DT::renderDataTable({
    DT::datatable(d, selection='single')
  })

  output$out <- shiny::renderUI({
    if (is.null(input$tbl_rows_selected)) return(NULL)
    id <- d$Id[input$tbl_rows_selected]
    x <- extract_info(make_fname(id), 'Item 1', filter_comments=F)
    shiny::HTML(paste('<pre>', paste(x, collapse='<br>'), '</pre>'))
  })
}
