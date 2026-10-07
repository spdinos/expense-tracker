mod_main_ui <- function(id) {
  ns <- NS(id)

  tagList(
      div(
        class = "financial-value-boxes",

        valueBoxOutput(ns("total_expenses")),
        valueBoxOutput(ns("total_income")),
        valueBoxOutput(ns("bank_balance")),
        valueBoxOutput(ns("total_savings")),
        valueBoxOutput(ns("total_investment_gain")),
        valueBoxOutput(ns("total_budget"))
      ),
    br(),

    # Collapsible Financial Charts section
    fluidRow(
        # A SINGLE tabBox with all plots as separate tabs
        tabBox(
          width = 12,    # Full width inside fluidRow
          tabPanel("Expense vs Budget",
          fluidRow(column(4, 
                    selectInput(ns("theme"), "Theme", choices = c("Date / Primary Category", "Date", "Year"), selected = "Date / Primary Category")),  
                   column(4, 
                    selectInput(ns("x_expense_vs_budget"), "X", choices = NULL, selected = NULL, multiple = TRUE)),
                   column(4, 
                    selectInput(ns("key_expense_vs_budget"), "Key", choices = NULL, selected = NULL, multiple = TRUE))),  
                   plotlyOutput(ns("expense_vs_budget"), height = "600px"),
                   br(),
                   reactableOutput(ns("data_table"))),
          tabPanel("Income vs Expense",
                   selectInput(ns("theme_inc_vs_exp"), "Theme", choices = c("Date", "Year"), selected = "Date"),  
                   plotlyOutput(ns("income_vs_expense"), height = "600px"),
                   br(),
                   reactableOutput(ns("income_expense_table"))),
          tabPanel("Savings",
                   plotlyOutput(ns("savings"), height = "600px")),
          tabPanel("Financial activities",
                   plotlyOutput(ns("financial_assets_plot"), height = "600px")),          
          tabPanel("Investement activities",
                   plotlyOutput(ns("investment_gain_plot"), height = "600px")),
          tabPanel("Year Comparison",
          radioButtons(
                ns("yoy_parameter"),
                "Compare",
                choices = c(
                  "Expenses",
                  "Income",
                  "Savings"
                ),
                selected = "Expenses",
                inline = TRUE
              ),

              plotlyOutput(ns("yoy_plot")),

              reactableOutput(ns("yoy_table")),
              br(),
              reactableOutput(ns("yoy_category_table")
          )
        ),
        tabPanel("Month Summary",
        fluidRow(
              column(
                12,
                h3("Rolling 12-Month Performance")
              )
            ),
            fluidRow(
              column(
                12,
                reactableOutput(
                  ns("rolling_12_summary")
                )
              )
            ),
            br(),
            fluidRow(
              column(
                12,
                plotlyOutput(
                  ns("rolling_12_plot")
                )
              )
            )
          )
      )
    )
  )
}
