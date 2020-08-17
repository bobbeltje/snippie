
ui <- function(){
  fluidPage(
    includeCSS(file.path(system.file(package='snippie'), 'www', 'style.css')),
    tags$head(tags$style('#wide_textarea * {width: 100%;}', 'body {background-color: #dcedff;}')),
    tags$h1('Snippie'),
    fluidRow(
      column(
        width=12L, style='margin-bottom: 5px;',
        actionButton('create', 'Create snippet', class='btn-success'),
        actionButton('edit', 'Edit snippet', class='btn-warning'),
        actionButton('delete', 'Delete snippet', class='btn-danger'),
        actionButton('load', 'Load snippets', class='btn-primary'),
        downloadButton('export', 'Export all')
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
            conditionalPanel(
              'output.multiple_snips', class='inl_item',
              actionButton('left', '<', class='btn-info'),
              actionButton('right', '>', class='btn-info')
            ),
            htmlOutput('snip_title', inline=T)
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
