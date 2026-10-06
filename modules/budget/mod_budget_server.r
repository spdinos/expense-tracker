mod_budget_server <- function(id, request_input, filtered_df) {
  moduleServer(id, function(input, output, session) {
  ns <- NS(id)

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  
  last_selected_row <- reactiveVal(NULL)

    changes_to_store <- reactiveVal(NULL)


observe({
  req(request_input())  # ensures request_input() is available
  if(is.null(changes_to_store())){
    changes_to_store(empty_like(request_input()))
  }
})


  rv_table_state <- reactiveValues(
    page = 1,
    selected_rows = NULL,
    filters = NULL,
    scroll_top = 0
  )

  target_page_after_add <- reactiveVal(NULL)

    clear_sidebar <- function() {
      updateTextInput(session, "jan", value =  "-")
      updateTextInput(session, "feb", value =  "-")
      updateTextInput(session, "mar", value =  "-")
      updateTextInput(session, "apr", value =  "-")
      updateTextInput(session, "may", value =  "-")
      updateTextInput(session, "jun", value =  "-")
      updateTextInput(session, "jul", value =  "-")
      updateTextInput(session, "aug", value =  "-")
      updateTextInput(session, "sep", value =  "-")
      updateTextInput(session, "oct", value =  "-")
      updateTextInput(session, "nov", value =  "-")
      updateTextInput(session, "dec", value =  "-")
    }

output$data_table <- renderReactable({
    full_data_for_table <- filtered_df()


static_header_style <- list(
  whiteSpace = "normal",
  overflow = "hidden",
  textAlign = "left",
  verticalAlign = "bottom",
  backgroundColor = "#3563bf",
  color = "white",
  fontFamily = "Calibri",
  fontSize = "12px",
  fontWeight = "bold"
)


reactable(
  full_data_for_table,
  selection = "multiple",
  columns = static_header_style,
  pagination = TRUE,
  compact = TRUE,
  highlight = TRUE,
  resizable = TRUE,
  wrap = TRUE,
  bordered = TRUE,
  searchable = TRUE,
  defaultPageSize = 50,
  onClick = "select",
  height = 600, 
  defaultColDef = colDef(headerStyle = static_header_style),
  theme = reactableTheme(
    borderColor = "#3563bf",
    style = list(
      fontFamily = "Calibri",
      fontSize = "12px"
    ),
    rowSelectedStyle = list(
      backgroundColor = "#cce0ff"  # slightly darker blue for selected rows
    ),
    searchInputStyle = list(
      height = "40px",
      width = "300px"
    )
  )
) %>% htmlwidgets::onRender(sprintf("
      function(el, x) {
        Shiny.setInputValue('%s', Math.random());
      }
    ", ns("data_table_render_complete")),
) 
    })

  observeEvent(input$clear_selection, {
    updateReactable(
      "data_table",
      selected = NA
    )
  })

  rv_restore_state_flag <- reactiveVal(FALSE)


    edit_pending_modal <- reactiveValues(values = NULL, rows = NULL)

 
    apply_modal_edits <- function(rows_to_edit, values_from_modal, selected_aa_values, saved_filters) {
        current_master_df_copy <- copy(request_input())
        setkey(current_master_df_copy, `ID`)

        any_change_made <- FALSE
        
        updated_rows_dt <- request_input()[0, ]
        
        for (aa_val in selected_aa_values) {
          current_row_dt <- current_master_df_copy[`ID` == aa_val]
          if (nrow(current_row_dt) == 0) next
          
          # Create a copy of the row for modification
          updated_row <- copy(current_row_dt)
          row_changed <- FALSE

          for (col_name in names(values_from_modal)) {
            if (col_name %in% names(current_master_df_copy)) {
              new_val <- values_from_modal[[col_name]]
              current_val <- updated_row[[col_name]]

                if (!identical(as.character(new_val), as.character(current_val)) &&
                    !isTRUE(is.na(new_val) && is.na(current_val))) {
                  updated_row[, (col_name) := new_val]
                  row_changed <- TRUE
                  any_change_made <- TRUE
                }
            }
          }
          if (row_changed) {
            # Append the updated row to the temporary data table
            updated_rows_dt <- rbindlist(list(updated_rows_dt, updated_row), fill = TRUE)
          }
        }
        
        if (any_change_made) {
          # Update the main data reactive with the changes for display
          current_df <- copy(request_input())
          setkey(current_df, `ID`)
          
          changes_dt <- changes_to_store()
          setkey(changes_dt, `ID`)
          
          for(aa_val in updated_rows_dt$`ID`) {
            updated_row <- updated_rows_dt[`ID` == aa_val]
            current_df[`ID` == aa_val] <- updated_row
            
            # Store changes in changes_to_store, overwriting if needed
            if (aa_val %in% changes_dt$`ID`) {
              changes_dt[`ID` == aa_val] <- updated_row
            } else {
              changes_dt <- rbindlist(list(changes_dt, updated_row), fill = TRUE)
            }
          }
          request_input(current_df)
          changes_to_store(changes_dt)
          
            # 🔑 Set target page to where the first edited row is
          page_len <- getReactableState("data_table", "pageSize") %||% 25
          row_index <- which(current_df$`ID` == updated_rows_dt$`ID`[1])
          if (length(row_index) > 0) {
            page_to_go <- floor((row_index - 1) / page_len) + 1
            target_page_after_add(page_to_go)
          }

          # Restore filters
          for (filter_name in names(rv_table_state$filters)) {
            filter_value <- rv_table_state$filters[[filter_name]]
            if (!is.null(filter_value)) {
              updateSelectInput(session, filter_name, selected = filter_value)
            }
          }

          rv_restore_state_flag(TRUE)

          showNotification("Changes from modal applied.", type = "message", duration = 2)

          last_selected_row(NULL)
        } else {
          showNotification("No changes detected in modal inputs.", type = "message", duration = 2)
        }
      }

  change_info_logic <- function(){
    selected_id <- last_selected_row()
    df_displayed_current <- filtered_df()
    rows_to_edit <- which(df_displayed_current$ID %in% selected_id)
      if (is.null(rows_to_edit) || length(rows_to_edit) == 0) {
        showNotification("No row is selected in the table to apply changes.", type = "message", duration = 3)
        return()
      }

      rv_table_state$page <- getReactableState("data_table", "page")
      rv_table_state$selected_rows <- getReactableState("data_table", "selected")

      df_displayed_current <- filtered_df()

      if (any(rows_to_edit < 1 | rows_to_edit > nrow(df_displayed_current))) {
        showNotification("The previously selected row is no longer valid in the current data (possibly due to filtering or refresh). Please re-select a row.", type = "error", duration = 5)
        last_selected_row(NULL)
        return()
      }

      selected_aa_values <- df_displayed_current[rows_to_edit, `ID`]
      modal_values <- list(
          `January` = input$jan,
          `February` = input$feb,
          `March` = input$mar,
          `April` = input$apr,
          `May` = input$may,
          `June` = input$jun,
          `July` = input$jul,
          `August` = input$aug,
          `September` = input$sep,
          `October` = input$oct,
          `November` = input$nov,
          `December` = input$dec
                  )

      saved_filters <- list(
            Year = input$year,
            Month = input$month,
            `Primary Category` = input$prim_cat,
            `Secondary Category` = input$sec_cat
            )

    selected_aa_values <- filtered_df()[rows_to_edit, `ID`]
 
    apply_modal_edits(rows_to_edit, modal_values, selected_aa_values, saved_filters)

    }


observeEvent(input$data_table_render_complete, {
  isolate({
    page <- target_page_after_add()
    if (!is.null(page)) {
      updateReactable(
        "data_table",
        page = page
      )
      target_page_after_add(NULL)
    }
  })
})

  observeEvent(getReactableState("data_table", "selected"), {
  selected_idx <- getReactableState("data_table", "selected")
  df_current_displayed <- filtered_df()

  if (is.null(selected_idx) || length(selected_idx) == 0) {
    last_selected_row(NULL)
    clear_sidebar()
    return()
  }


if (length(selected_idx) > 1) {
  showNotification("Multiple rows selected. Sidebar inputs will not be updated.", type = "message", duration = 3)
  
  selected_ids <- df_current_displayed[selected_idx, ID]
  
  last_selected_row(selected_ids)
  return()
}

  if (selected_idx < 1 || selected_idx > nrow(df_current_displayed)) {
    showNotification("Selected row index is out of bounds.", type = "error", duration = 3)
    updateReactable("data_table", selected = NULL)
    last_selected_row(NULL)
    clear_sidebar()
    return()
  }

  # Use .SD to safely extract the row as a data.table
  selected_row <- df_current_displayed[selected_idx, .SD]

  # Extract ID safely
  selected_id <- selected_row[, ID]


last_selected_row(selected_id)

 isolate({
    updateSelectizeInput(session, "prim_cat", selected = selected_row[["Primary Category"]])
      updateTextInput(session, "jan", value =  selected_row[["January"]])
      updateTextInput(session, "feb", value =  selected_row[["February"]])
      updateTextInput(session, "mar", value =  selected_row[["March"]])
      updateTextInput(session, "apr", value =  selected_row[["April"]])
      updateTextInput(session, "may", value =  selected_row[["May"]])
      updateTextInput(session, "jun", value =  selected_row[["June"]])
      updateTextInput(session, "jul", value =  selected_row[["July"]])
      updateTextInput(session, "aug", value =  selected_row[["August"]])
      updateTextInput(session, "sep", value =  selected_row[["September"]])
      updateTextInput(session, "oct", value =  selected_row[["October"]])
      updateTextInput(session, "nov", value =  selected_row[["November"]])
      updateTextInput(session, "dec", value =  selected_row[["December"]])

  })
}, ignoreNULL = FALSE)

observeEvent(input$data_table_init_complete, {
  isolate({
    page <- target_page_after_add()
    if (!is.null(page)) {
      updateReactable(
        "data_table",
        page = page
      )
      target_page_after_add(NULL)
    }
  })
}, ignoreInit = TRUE)

save_logic <- function() {

  cat("Save Budget to PostgreSQL:\n")

  conn <- NULL

  # ============================================================
  # CONNECT TO POSTGRESQL
  # ============================================================

   conn <- connect_financial_db()


  if (is.null(conn)) {
    return()
  }


  # ============================================================
  # LOAD CURRENT BUDGET TABLE
  # ============================================================

  database <- tryCatch({

    DBI::dbReadTable(
      conn,
      "Budget"
    )

  }, error = function(e) {

    showNotification(
      paste("Read failed:", e$message),
      type = "error",
      duration = 5
    )

    NULL
  })


  if (is.null(database)) {

    DBI::dbDisconnect(conn)

    return()
  }


  # ============================================================
  # PREPARE DATA
  # ============================================================

  database <- data.table::as.data.table(database)

  data_to_check <- data.table::as.data.table(
    request_input()
  )


  # ============================================================
  # NORMALIZE COLUMN NAMES
  # ============================================================

  names(data_to_check) <- gsub(
    " ",
    ".",
    names(data_to_check)
  )

  names(database) <- gsub(
    " ",
    ".",
    names(database)
  )


  data.table::setkey(
    database,
    ID
  )

  data.table::setkey(
    data_to_check,
    ID
  )


  # ============================================================
  # DETERMINE CHANGED ROWS
  # ============================================================

  existing_rows <- data.table::fsetdiff(
    data_to_check,
    database
  )


  # ============================================================
  # NO CHANGES
  # ============================================================

  if (nrow(existing_rows) == 0) {

    showNotification(
      "No rows to change",
      type = "message",
      duration = 3
    )

    DBI::dbDisconnect(conn)

    return()
  }


  # ============================================================
  # R -> POSTGRESQL COLUMN MAPPING
  # ============================================================

  postgres_cols <- DBI::dbListFields(
    conn,
    "Budget"
  )


  r_to_sql_map <- setNames(
    postgres_cols,
    gsub(
      " ",
      ".",
      postgres_cols
    )
  )


  # ============================================================
  # TRANSACTION
  # ============================================================

  transaction_started <- FALSE
  transaction_committed <- FALSE


  tryCatch({

    DBI::dbBegin(conn)

    transaction_started <- TRUE


    # ==========================================================
    # UPDATE EXISTING ROWS
    # ==========================================================

    for (i in seq_len(nrow(existing_rows))) {

      key <- existing_rows$ID[i]


      database_row <- database[
        ID == key
      ]


      update_row <- existing_rows[
        i,
        ,
        drop = FALSE
      ]


      changed_fields <- c()


      # --------------------------------------------------------
      # FIND WHICH FIELDS ACTUALLY CHANGED
      # --------------------------------------------------------

      for (f in setdiff(
        names(update_row),
        "ID"
      )) {

        new_val <- update_row[[f]]
        old_val <- database_row[[f]]


        if (!identical(
          new_val,
          old_val
        )) {

          changed_fields <- c(
            changed_fields,
            f
          )
        }
      }


      # --------------------------------------------------------
      # UPDATE ONLY CHANGED FIELDS
      # --------------------------------------------------------

      if (length(changed_fields) > 0) {

        set_clause <- paste(
          vapply(
            seq_along(changed_fields),
            function(j) {

              sql_col <- r_to_sql_map[
                changed_fields[j]
              ]


              paste0(
                DBI::dbQuoteIdentifier(
                  conn,
                  sql_col
                ),
                " = $",
                j
              )
            },
            character(1)
          ),
          collapse = ", "
        )


        update_query <- paste0(
          'UPDATE "Budget" SET ',
          set_clause,
          ' WHERE "ID" = $',
          length(changed_fields) + 1
        )


        # ------------------------------------------------------
        # PARAMETERS
        # ------------------------------------------------------

        params <- lapply(
          changed_fields,
          function(f) {

            value <- update_row[[f]]

            if (
              length(value) == 0 ||
              is.na(value)
            ) {
              return(NA)
            }

            value
          }
        )


        params <- c(
          params,
          list(
            as.integer(key)
          )
        )


        DBI::dbExecute(
          conn,
          update_query,
          params = params
        )
      }
    }


    # ==========================================================
    # COMMIT
    # ============================================================

    DBI::dbCommit(conn)

    transaction_committed <- TRUE


    showNotification(
      "✅ Budget updated successfully.",
      type = "message"
    )


    # ==========================================================
    # REFRESH FROM POSTGRESQL
    # ============================================================

    latest_data <- DBI::dbReadTable(
      conn,
      "Budget"
    )


    names(latest_data) <- gsub(
      "\\.",
      " ",
      names(latest_data)
    )


    request_input(
      data.table::as.data.table(
        latest_data
      )
    )


    showModal(
      modalDialog(
        "✅ Budget synchronized with PostgreSQL!",
        easyClose = TRUE
      )
    )


  }, error = function(e) {


    # ==========================================================
    # ROLLBACK
    # ==========================================================

    if (
      transaction_started &&
      !transaction_committed
    ) {

      try(
        DBI::dbRollback(conn),
        silent = TRUE
      )
    }


    showModal(
      modalDialog(
        title = "Error",
        paste(
          "Failed to synchronize Budget:",
          e$message
        ),
        easyClose = TRUE
      )
    )


    message(
      paste(
        "Error during Budget save:",
        e$message
      )
    )


  }, finally = {

    if (!is.null(conn)) {

      try(
        DBI::dbDisconnect(conn),
        silent = TRUE
      )
    }
  })


  # ============================================================
  # RESET TRACKING
  # ============================================================

  changes_to_store(
    empty_like(
      request_input()
    )
  )
}

observeEvent(input$perform_action, {
  selected_action <- input$action_selector
  if (is.null(selected_action) || selected_action == "") {
    showNotification("Please select an action.", type = "warning")
    return()
  }

  switch(selected_action,
    "Change info" = {
      # Trigger your Change Info logic
    change_info_logic()
    },
    "Save" = {
      save_logic()
      },
  )
})

  })}
