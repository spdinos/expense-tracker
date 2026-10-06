mod_drp_ui <- function(id, dropdown) {
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
      width: 100%;
      border: 2px solid black;
    }
    .sidebar-button-red {
      background-color: red;
      color: white;
      font-weight: bold;
      font-size: 16px;
      height: 40px;
      width: 100%;
      border: 2px solid black;
    }
  ")),
fluidRow(
  column(12,
    div(style = "display: flex; align-items: flex-end; gap: 10px;",
      column(4, selectizeInput(ns("prim_cat"), "Primary Category", choices = c("", sort(unique(dropdown$`Primary Category`))), selected = NULL, multiple = FALSE, options = list(create = TRUE))),
      column(4, selectizeInput(ns("sec_cat"), "Secondary Category", choices = c("", sort(unique(dropdown$`Secondary Category`))), selected = NULL, multiple = FALSE, options = list(create = TRUE))),
      div(style = "flex: 1;", selectInput(ns("action_selector"), "Choose Action:",
        choices = c("", "Add new row", "Delete rows", "Change info", "Save"),
        selected = NULL
      )),
      actionButton(ns("perform_action"), "Go", icon = icon("play"), class = "sidebar-button"),
      actionButton(ns("clear_selection"), "Deselect All", class = "sidebar-button-red")
    )
  )
),
fluidRow(reactableOutput(ns("data_table")))
    )
}
