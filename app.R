library(shiny)
library(bslib)
library(ggplot2)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "../rclone-datasus/logs.duckdb", read_only = TRUE)
logs_tb <- dplyr::tbl(con, "logs")

ui <- page_sidebar(
  heigth = "100px",
  title = "DataSUS updates log",
  sidebar = sidebar(
    dateInput("date_selector", label = "Date")
  ),
  layout_columns(
    col_widths = c(5,7),
    row_heights = c(2, 3),
    card(
      card_header("Summary"),
      card_body(
        vchartr::vchartOutput("summary_graph")
      )
    ),
    card(
      card_header("Events"),
      DT::DTOutput("files_table")
    )
  )

)

server <- function(input, output) {

  logs_sel <- reactive({
    logs_tb |>
      dplyr::filter(date == input$date_selector) |>
      dplyr::select(date, system, file, action) |>
      dplyr::collect()
  })

  output$summary_graph <- vchartr::renderVchart(
    logs_sel() |>
    dplyr::summarise(files = dplyr::n(), .by = c(system, action)) |>
    vchartr::vchart() |>
      vchartr::v_circlepacking(
        vchartr::aes(lvl1 = system, lvl2 = action, value = files)
      )
  )

  output$files_table <- DT::renderDataTable(
    logs_sel(),
    options = list(info = FALSE),
    rownames= FALSE
  )
}

shinyApp(ui, server)