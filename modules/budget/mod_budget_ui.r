mod_budget_ui <- function(id) {
ns <- NS(id)
tagList(
  tags$style(HTML("
    .reactable .rt-th:first-child {
      background-color: #3563bf !important;
      color: white !important;
    }    
    .reactable .rt-td, .reactable .rt-th {
      border-left: none !important;
      border-right: none !important;
    }
    .sidebar-button {
      background-color: green;
      color: white;
      font-weight: bold;
      font-size: 16px;
      height: 40px;
      border: 2px solid black;
    }
    .sidebar-button-red {
      background-color: red;
      color: white;
      font-weight: bold;
      font-size: 16px;
      height: 40px;
      border: 2px solid black;
    }
  ")),
fluidRow(
      column(1, textInput(ns("jan"), "January", value = "-")),
      column(1, textInput(ns("feb"), "Februrary", value = "-")),
      column(1, textInput(ns("mar"), "March", value = "-")),
      column(1, textInput(ns("apr"), "April", value = "-")),
      column(1, textInput(ns("may"), "May", value = "-")),
      column(1, textInput(ns("jun"), "June", value = "-")),
      column(1, textInput(ns("jul"), "July", value = "-")),
      column(1, textInput(ns("aug"), "August", value = "-")),
      column(1, textInput(ns("sep"), "September", value = "-")),
      column(1, textInput(ns("oct"), "October", value = "-")),
      column(1, textInput(ns("nov"), "November", value = "-")),
      column(1, textInput(ns("dec"), "December", value = "-"))),
fluidRow(
  column(
    12,
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      column(4, selectInput(
        ns("action_selector"), "Choose Action:",
        choices = c("", "Change info", "Save"),
        selected = NULL
      )),
      actionButton(
        ns("perform_action"), "Go",
        icon = icon("play"), class = "sidebar-button"
      ),
      actionButton(
        ns("clear_selection"), "Deselect All",
        class = "sidebar-button-red"
      )
    )
  )),
fluidRow(reactableOutput(ns("data_table")))
    )
}
