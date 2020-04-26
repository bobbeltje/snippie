
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
