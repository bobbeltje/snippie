
ui <- function(){
  fluidPage(
    tags$head(tags$style('#wide_textarea * {width: 100%;}')),
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
