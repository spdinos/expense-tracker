ui <- dashboardPage(skin = "green",
        dashboardHeader(
          title = div("Financial Management", style = "color: Yellow; font-weight: bold; font-size: 16px; padding: 5px 15px; border-radius: 5px;"),
        tags$li(class = "dropdown", actionButton("refresh_data", "Refresh Data", style = "background-color: green; color: white; font-weight: bold; font-size: 16px; height: 60px; border: 2px solid black;", icon = icon("arrows-rotate")))),
      dashboardSidebar(
        sidebarMenu(id = "tabs", 
         menuItem("Main", tabName = "main_tab", icon = icon("dashboard")),
         menuItem("Expenses", tabName = "expenses_tab", icon = icon("list-check")),
         menuItem("Income", tabName = "income_tab", icon = icon("list-check")),
         menuItem("Budget", tabName = "budget_tab", icon = icon("list-check")),
         menuItem("Bank balance", tabName = "bank_tab", icon = icon("list-check")),
         menuItem("Drop down", tabName = "drop_down_tab", icon = icon("chart-bar")),
         conditionalPanel(
          condition = "input.tabs == 'bank_tab' | input.tabs == 'expenses_tab'", 
            mod_filters_ui("filters_expense")),
         conditionalPanel(
          condition = "input.tabs == 'income_tab'", 
            mod_filters_ui("filters_income")),
         conditionalPanel(
          condition = "input.tabs == 'budget_tab'", 
            mod_filters_ui("filters_budget")),  
         conditionalPanel(
          condition = "input.tabs == 'drop_down_tab'", 
            mod_filters_ui("filters_dropdown")),  
         conditionalPanel(
          condition = "input.tabs == 'main_tab'", 
            mod_filters_ui("filters_summary"))
      ), width = 300),
        dashboardBody(
                   useShinyjs(),
      tags$head(
          tags$script(HTML("
            window.onbeforeunload = function(e) {
              e.preventDefault();
              e.returnValue = '';
            };
          "))
        ),
          tabItems(
            tabItem(tabName = "main_tab",
            mod_main_ui("main")
            ),
            tabItem(tabName = "drop_down_tab",
            mod_drp_ui("drop_down", dropdown)
            ),
            tabItem(tabName = "expenses_tab",
            mod_exp_ui("expenses", dropdown)
            ),
            tabItem(tabName = "income_tab",
            mod_inc_ui("income")
            ),
            tabItem(tabName = "bank_tab",
            mod_bank_ui("bank")
            ),
            tabItem(tabName = "budget_tab",
            mod_budget_ui("budget")
            )                                                
        )
      ))