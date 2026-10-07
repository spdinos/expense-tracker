theme_plot <- function(){
              theme(
       plot.title = element_text(size = 12, color = "red", face = "bold", hjust = 0.5),
       axis.text = element_text(size = 8),
       axis.title = element_text(size = 10, color = "red", face = "bold"),
       legend.position = "bottom",
       panel.grid = element_blank(),
       panel.background = element_rect(fill = "#f3f3f3"),
       plot.background = element_rect(fill = "#e9ffdd")
       )
}

format_sql_text <- function(value) {
  if (is.na(value) || is.null(value) || as.character(value) == "") {
    return("NULL")
  } else {
    # Escape single quotes by doubling them
    val_escaped <- gsub("'", "''", as.character(value))
    return(paste0("'", val_escaped, "'"))
  }
}

empty_like <- function(df) {df[0, ]}

connect_financial_db <- function() {

  DBI::dbConnect(
  RPostgres::Postgres(),
  host = "aws-1-eu-central-1.pooler.supabase.com",
  port = 5432,
  dbname = "postgres",
  user = "postgres.gcvpukeosfmicildrxsu",
  password = Sys.getenv("SUPABASE_DB_PASSWORD"),
  sslmode = "require"
)
}

get_data <- function(){
conn <- connect_financial_db()
# You can query any table
Income <- dbReadTable(conn, "Income", stringsAsFactors = FALSE) %>% mutate(Date = dmy(Date),
                                                                          Year = year(Date),
                                                                          Month = month(Date))
Budget <- dbReadTable(conn, "Budget", stringsAsFactors = FALSE)
Dropdown <- dbReadTable(conn, "Drop_Down_List", stringsAsFactors = FALSE)
Expenses <- dbReadTable(conn, "Expenses", stringsAsFactors = FALSE) %>% mutate(Date = dmy(Date),
                                                                          Year = year(Date),
                                                                          Month = month(Date))
Bank <- dbReadTable(conn, "Bank", stringsAsFactors = FALSE) %>% mutate(Date = dmy(Date))

dbDisconnect(conn)

names(Expenses) <- gsub("\\.", " ", names(Expenses))
names(Budget) <- gsub("\\.", " ", names(Budget))
names(Dropdown) <- gsub("\\.", " ", names(Dropdown))
names(Bank) <- gsub("\\.", " ", names(Bank))
names(Income) <- gsub("\\.", " ", names(Income))

drop_down_cat <- Dropdown %>% select(`Primary Category`,`Secondary Category`) %>% distinct() %>% filter(`Primary Category` == "Έκτακτα έξοδα" | `Primary Category` == "Πάγια έξοδα" | `Primary Category` == "Προτεραιότητες" | `Primary Category` == "Έξοδα καλοπέρασης")

Years <- data.frame(
  Year = rep(min(Expenses$Year):(max(Expenses$Year) + 1), times = nrow(drop_down_cat))) %>% 
arrange(Year)

expenses_clean <- Expenses %>%
  mutate(Date = as.Date(Date),
        Month = month.name[month(Date)]) %>%
  group_by(Year, Month, `Primary Category`) %>%
  summarize(Amount = sum(Expense, na.rm = TRUE), .groups = "drop")

  income_clean <- Income %>%
  mutate(Date = as.Date(Date),
          Month = month.name[month(Date)]) %>%
  group_by(Year, Month, `Primary Category`) %>%
  summarize(Amount = sum(Income, na.rm = TRUE), .groups = "drop")

  budget_clean <- Budget %>% 
                  pivot_longer(cols=-c(1:3), names_to = "Month", values_to = "Budget") %>% 
                  filter(Budget != "-") %>% mutate(Budget = as.numeric(Budget)) %>% select(-ID)

  bank_clean <- Bank %>% 
                    arrange(Year) %>%
                    mutate(`Actual savings` = `Bank balance` - lag(`Bank balance`),
                            Year = as.numeric(Year),
                            Date = dmy(Date)) %>%
                    rename(Month_num = Month) %>%
                    mutate(Month = month.name[Month_num]) %>%
                    select(-ID)

monthly_summary_per_cat <- rbind(expenses_clean, income_clean)

monthly_summary_per_cat <- monthly_summary_per_cat %>%
    full_join(budget_clean, by = c("Year", "Month", "Primary Category")) %>%         
    mutate(
      Month_num = match(Month, month.name), 
      Date = make_date(Year, Month_num)
    )

monthly_summary_per_cat <- bind_rows(
  monthly_summary_per_cat,
  bank_clean
) %>%
          arrange(Date) %>%
    mutate(Date = as.factor(format(Date, "%Y-%m")))

expenses_clean_bank <- Expenses %>%
  mutate(Date = as.Date(Date),
        Month = month.name[month(Date)]) %>%
  group_by(Year, Month, `Primary Category`, `Secondary Category`) %>%
  summarize(Amount = sum(Expense, na.rm = TRUE), .groups = "drop")

  income_clean_bank <- Income %>%
  mutate(Date = as.Date(Date),
          Month = month.name[month(Date)]) %>%
  group_by(Year, Month, `Primary Category`, `Secondary Category`) %>%
  summarize(Amount = sum(Income, na.rm = TRUE), .groups = "drop")

  savings_outcome <- rbind(expenses_clean_bank, income_clean_bank)

savings_outcome <- savings_outcome %>%
    full_join(budget_clean, by = c("Year", "Month", "Primary Category")) %>%         
    mutate(
      Month_num = match(Month, month.name), 
      Date = make_date(Year, Month_num)
    )

savings_outcome <- bind_rows(
  savings_outcome,
  bank_clean
) %>%
          arrange(Date) %>%
    mutate(Date = as.factor(format(Date, "%Y-%m")))

return(list(Income = Income,
              Expenses = Expenses,
              Budget = Budget,
              Dropdown = Dropdown,
              Bank = Bank,
              Monthly_summary = monthly_summary_per_cat,
              Savings_outcome = savings_outcome
              ))
}

bar_plot <- function(data, x, y, fill, title, key, source = "bar_plot") {

  # Convert y to symbol (always one)
  y_sym <- rlang::sym(y)

  # For x, fill, key: if multiple, create a combined column
  if (length(x) > 1) {
    data$..x <- interaction(data[, x], drop = TRUE)
    x_aes <- rlang::sym("..x")
  } else {
    x_aes <- rlang::sym(x)
  }

  fill_aes <- rlang::sym(fill)

  if (length(key) > 1) {
    data$..key <- interaction(data[, key], drop = TRUE)
    key_aes <- rlang::sym("..key")
  } else {
    key_aes <- rlang::sym(key)
  }

  p <- ggplot(
    data,
    aes(
      x = !!x_aes,
      y = !!y_sym,
      fill = !!fill_aes,
      group = !!fill_aes,
      key = !!key_aes
    )
  ) +
    geom_bar(stat = "identity", position = "stack")
    if("Primary Category" %in% names(data)) {
    
    p <- p + facet_wrap(~ `Primary Category`, scales = "free_y")}
    
    
    p <- p + theme_plot() +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  # Register click events
  ggplotly(p, source = source) %>% event_register("plotly_click")
}



# Create the plot
scatter_plot <- function(data, x, y, z, title, data_labels = TRUE) {
  
  x <- sym(deparse(substitute(x)))
  y <- sym(deparse(substitute(y)))
  z <- sym(deparse(substitute(z)))

  p <- ggplot(data, aes(x = !!x, y = !!y)) +
    geom_point(aes(color = !!z)) +
    geom_line(
      linewidth = 0.8,
      aes(group = !!z, color = !!z),
      linetype = "dotted"
    ) +
    labs(title = title) +
    theme_minimal() +
    theme_plot() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # Add labels only when requested
  if (data_labels) {
    
    p <- p +
      geom_text(
        aes(label = round(!!y, 1)),
        nudge_y = 0.02 * max(
          data[[as_string(y)]],
          na.rm = TRUE
        ),
        size = 3
      )
  }

  p <- ggplotly(p) %>%
    layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.1,
        title = list(text = "")
      )
    )

  p
}
