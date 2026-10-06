main_dashboard_ui <- function() {

  dashboardPage(

    skin = "green",

    dashboardHeader(
      title = div(
        "Financial Management"
      ),

      tags$li(
        class = "dropdown",

        actionButton(
          "refresh_data",
          "Refresh Data",
          icon = icon("refresh")
        )
      )
    ),

    dashboardSidebar(

      sidebarMenu(

        id = "tabs",

        menuItem(
          "Main",
          tabName = "main_tab",
          icon = icon("dashboard")
        ),

        menuItem(
          "Expenses",
          tabName = "expenses_tab",
          icon = icon("list-check")
        ),

        menuItem(
          "Income",
          tabName = "income_tab",
          icon = icon("list-check")
        ),

        menuItem(
          "Budget",
          tabName = "budget_tab",
          icon = icon("list-check")
        ),

        menuItem(
          "Bank balance",
          tabName = "bank_tab",
          icon = icon("list-check")
        ),

        menuItem(
          "Drop down",
          tabName = "drop_down_tab",
          icon = icon("chart-bar")
        ),

        conditionalPanel(
          condition =
            "input.tabs == 'bank_tab' | input.tabs == 'expenses_tab'",

          mod_filters_ui("filters_expense")
        ),

        conditionalPanel(
          condition = "input.tabs == 'income_tab'",

          mod_filters_ui("filters_income")
        ),

        conditionalPanel(
          condition = "input.tabs == 'budget_tab'",

          mod_filters_ui("filters_budget")
        ),

        conditionalPanel(
          condition = "input.tabs == 'drop_down_tab'",

          mod_filters_ui("filters_dropdown")
        ),

        conditionalPanel(
          condition = "input.tabs == 'main_tab'",

          mod_filters_ui("filters_summary")
        )
      ),

      width = 300
    ),

    dashboardBody(

      useShinyjs(),

      tags$head(

        tags$script(
          HTML("
            window.onbeforeunload = function(e) {
              e.preventDefault();
              e.returnValue = '';
            };
          ")
        )
      ),

      tabItems(

        tabItem(
          tabName = "main_tab",
          mod_main_ui("main")
        ),

        tabItem(
          tabName = "drop_down_tab",
          mod_drp_ui("drop_down", dropdown_df)
        ),

        tabItem(
          tabName = "expenses_tab",
          mod_exp_ui("expenses", dropdown_df)
        ),

        tabItem(
          tabName = "income_tab",
          mod_inc_ui("income")
        ),

        tabItem(
          tabName = "bank_tab",
          mod_bank_ui("bank")
        ),

        tabItem(
          tabName = "budget_tab",
          mod_budget_ui("budget")
        )
      )
    )
  )
}


ui <- fluidPage(

  useShinyjs(),

  uiOutput("app_content")

)