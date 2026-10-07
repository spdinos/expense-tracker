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

  width = 300,

  sidebarMenu(

    id = "tabs",
    selected = "main_tab",

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
    )
  ),

 div(
    class = "sidebar-filters",
    uiOutput("sidebar_filters")
  )
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


ui <- tagList(

  useShinyjs(),

  # =========================
  # DASHBOARD
  # =========================

  div(
    id = "dashboard_container",
    style = "display: none;",

    main_dashboard_ui()
  ),

  # =========================
  # LOGIN
  # =========================

  div(
    id = "login_container",

    tags$head(
      tags$style(
        HTML("
          body {
            background-color: #f5f5f5;
          }

          .login-container {
            width: calc(100% - 30px);
            max-width: 400px;
            margin: 100px auto;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 4px 20px rgba(0,0,0,0.15);
            background: white;
          }

          .login-title {
            text-align: center;
            margin-bottom: 30px;
          }

          .login-button {
            width: 100%;
          }

          .login-error {
            color: #d9534f;
            text-align: center;
            margin-top: 15px;
          }

          @media (max-width: 767px) {

            .login-container {
              width: calc(100% - 20px);
              margin: 30px auto;
              padding: 20px;
            }

          }
        ")
      )
    ),

    div(
      class = "login-container",

      h2(
        "Financial Management",
        class = "login-title"
      ),

      textInput(
        "login_username",
        "Username"
      ),

      passwordInput(
        "login_password",
        "Password"
      ),

      actionButton(
        "login_button",
        "Login",
        class = "btn-primary login-button"
      ),

      uiOutput("login_error")
    )
  )
)