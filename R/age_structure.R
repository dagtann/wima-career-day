#' Shiny demo app ui
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @noRd
age_structure_ui <- shiny::fluidPage(
  shiny::tags$h1("Altersstrukturanalyse der offenen Forderungen"),
  plotly::plotlyOutput(outputId = "past_due_distribution"),
  shiny::tags$br(),
  shiny::splitLayout(
    "Mittlere \u00DCberschreitung:",
    shiny::textOutput(outputId = "mean_past_due"),
    "Vertrauensintervall (95%):",
    shiny::textOutput(outputId = "confint_past_due")
  )
)

#' Shiny demo app server
#'
#' @param input,output,session Shiny boilerplate
#' @param data, data.frame, Receivables data
#'
#' @importFrom plotly plot_ly add_trace layout `%>%` renderPlotly
#' @import shiny
#' @importFrom stats t.test
#' @noRd
age_structure_server <- function(input, output, session, data = create_receivables()) {
  output$past_due_distribution <- plotly::renderPlotly(
    expr = {
      plotly::plot_ly(
        data = data[data$period == 0, ],
        x = ~ days_past_due,
        type = "box",
        name = "Vorjahr"
      ) %>%
      plotly::add_trace(
        data = data[data$period == 1, ],
        x = ~ days_past_due,
        type = "box",
        name = "Laufendes Jahr"
      ) %>%
      plotly::layout(
        xaxis = list(title = "\u00DCberschreitung des Zahlungsziels in Tagen")
      )
    }
  )

  test_results <- stats::t.test(
    days_past_due ~ period,
    data = data,
    alternative = "two.sided",
    conf.level = .95
  )

  output$mean_past_due <- shiny::renderText(
    round(test_results$estimate, digits = 0)
  )

  output$confint_past_due <- shiny::renderText(
    round(test_results$conf.int, digits = 2)
  )
}

#' Run demo app
#' @param ui Shiny UI defintion
#' @param server Shiny server function

#' @import shiny
#'
#' @export
age_structure <- function(ui = age_structure_ui,
                          server = age_structure_server) {
  shiny::shinyApp(ui = age_structure_ui, server = age_structure_server)
}
