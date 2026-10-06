mod_drp_server <- function(id, request_input, filtered_df) {
  moduleServer(id, function(input, output, session) {
  ns <- NS(id)

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  
  last_selected_row <- reactiveVal(NULL)

    rows_to_store <- reactiveVal(NULL)
    changes_to_store <- reactiveVal(NULL)
    deleted_rows_to_store <- reactiveVal(NULL)


observe({
  req(request_input())  # ensures request_input() is available
  if (is.null(rows_to_store())) {
    rows_to_store(empty_like(request_input()))
    }
  if(is.null(changes_to_store())){
    changes_to_store(empty_like(request_input()))
  }
  if(is.null(deleted_rows_to_store())){
    deleted_rows_to_store(empty_like(request_input()))
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
      updateSelectizeInput(session, "prim_cat", selected =  "")
      updateSelectizeInput(session, "sec_cat", selected =  "")
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

# Start with manually defined columns
static_columns <- list(
  ID = colDef(name = "ID", headerVAlign = "bottom", headerStyle = static_header_style),
  `Primary Category` = colDef(name = "Primary Category", headerVAlign = "bottom", headerStyle = static_header_style),
  `Secondary Category` = colDef(name = "Secondary Category", headerVAlign = "bottom", headerStyle = static_header_style)
)

reactable(
  full_data_for_table,
  selection = "multiple",
  columns = static_columns,
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
  defaultSorted = c("ID"),
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
          `Primary Category` = input$prim_cat,
          `Secondary Category` = input$sec_cat
        )
      
      saved_filters <- list(
            Year = input$year,
            Month = input$month,
            `Primary Category` = input$prim_cat
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
    updateSelectizeInput(session, "sec_cat", selected = selected_row[["Secondary Category"]])

  })
}, ignoreNULL = FALSE)

add_row_logic <- function(){
   n <- 1
    current_master_df <- copy(request_input())

    current_max_aa_in_df <- if (nrow(current_master_df) > 0) max(current_master_df$`ID`, na.rm = TRUE) else 0
    new_aas <- seq(from = current_max_aa_in_df + 1, length.out = n)

    new_rows <- data.table(
          `ID` = as.numeric(new_aas),
          `Primary Category` = character(n),
          `Secondary Category` = character(n)
    )
    new_rows[, `Primary Category` := input$prim_cat]
    new_rows[, `Secondary Category` := input$sec_cat]

    current_rows <- rows_to_store()
    rows_to_store(rbindlist(list(current_rows, new_rows), fill = TRUE, use.names = TRUE))
    request_input(rbindlist(list(current_master_df, new_rows), fill = TRUE, use.names = TRUE))
    
    # Calculate and set the target page
    page_len <- getReactableState("data_table", "pageSize") %||% 50
    total_rows_after_add <- nrow(request_input())
    page_to_go <- floor((total_rows_after_add - 1) / page_len) + 1
    if (page_to_go < 1) page_to_go <- 1
    
    # Use the reactive value to store the page
    target_page_after_add(page_to_go)
    showNotification("Rows added successfully.", type = "message", duration = 2)
      
    }

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

    delete_row_logic <- function() {
      req(last_selected_row())

      selected_id <- last_selected_row()
      df_displayed_current <- filtered_df()

      # Find rows to delete by ID
      del_id <- df_displayed_current$ID[df_displayed_current$ID %in% selected_id]

      # Remove from main dataset (not filtered)
      df_main <- request_input()
      df_main <- df_main[!df_main$ID %in% del_id, ]
      request_input(df_main)

      # Record deleted rows
      new_deletes <- data.frame(ID = del_id, check.names = FALSE)
      deleted_rows_to_store(bind_rows(deleted_rows_to_store(), new_deletes))

      # --- Pagination logic ---
      page_len <- getReactableState("data_table", "pageSize") %||% 25

      # Use the CURRENT filtered table after deletion
      df_filtered_after <- df_main[df_main$ID %in% filtered_df()$ID, ]

      if (nrow(df_filtered_after) > 0) {
        # Determine numeric proximity if IDs are numeric
        if (is.numeric(df_filtered_after$ID)) {
          remaining_ids <- sort(df_filtered_after$ID)
          del_id_mean <- mean(del_id)
          nearest_id <- remaining_ids[which.min(abs(remaining_ids - del_id_mean))]
        } else {
          # If IDs are not numeric, use row positions instead
          row_index_deleted <- which(df_displayed_current$ID %in% del_id)
          nearest_row_index <- max(1, min(row_index_deleted[1], nrow(df_filtered_after)))
          nearest_id <- df_filtered_after$ID[nearest_row_index]
        }

        # find the index of that ID in the filtered data
        row_index <- which(df_filtered_after$ID == nearest_id)

        if (length(row_index) > 0) {
          page_to_go <- floor((row_index - 1) / page_len) + 1
          target_page_after_add(page_to_go)
        }
      }
    }

save_logic <- function() {

  cat("Save Drop_Down_List to PostgreSQL:\n")

  conn <- NULL

  # ============================================================
  # CONNECT TO POSTGRESQL
  # ============================================================

   conn <- connect_financial_db()


  if (is.null(conn)) {
    return()
  }


  # ============================================================
  # LOAD CURRENT DATABASE TABLE
  # ============================================================

  database <- tryCatch({

    DBI::dbReadTable(
      conn,
      "Drop_Down_List"
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

  data_to_check <- request_input()

  added_rows <- rows_to_store()

  deleted_rows <- deleted_rows_to_store()


  # ============================================================
  # NORMALIZE COLUMN NAMES
  # ============================================================

  names(data_to_check) <- gsub(
    " ",
    ".",
    names(data_to_check)
  )

  names(added_rows) <- gsub(
    " ",
    ".",
    names(added_rows)
  )

  names(database) <- gsub(
    " ",
    ".",
    names(database)
  )

  names(deleted_rows) <- gsub(
    " ",
    ".",
    names(deleted_rows)
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
  # DETERMINE NEW / UPDATED / DELETED ROWS
  # ============================================================

  new_rows_insert <- dplyr::semi_join(
    data_to_check,
    added_rows,
    by = "ID"
  )


  data_for_update_check <- dplyr::anti_join(
    data_to_check,
    new_rows_insert,
    by = "ID"
  )


  existing_rows <- data.table::fsetdiff(
    data_for_update_check,
    database
  )


  # PostgreSQL generates the ID automatically
  if ("ID" %in% names(new_rows_insert)) {

    new_rows_insert <- new_rows_insert %>%
      dplyr::select(-ID) %>%
      dplyr::distinct()

  } else {

    new_rows_insert <- new_rows_insert %>%
      dplyr::distinct()
  }


  rows_to_delete <- as.data.frame(
    dplyr::anti_join(
      deleted_rows,
      added_rows,
      by = "ID"
    )
  )


  new_rows_insert <- as.data.frame(
    new_rows_insert
  )


  # ============================================================
  # NO CHANGES
  # ============================================================

  if (
    nrow(new_rows_insert) == 0 &&
    nrow(existing_rows) == 0 &&
    nrow(rows_to_delete) == 0
  ) {

    showNotification(
      "No rows to add, change, or delete",
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
    "Drop_Down_List"
  )


  r_to_sql_map <- setNames(
    postgres_cols,
    gsub(
      " ",
      ".",
      postgres_cols
    )
  )


  required_fields <- gsub(
    " ",
    ".",
    postgres_cols[
      postgres_cols %in%
        c(
          "Primary Category",
          "Secondary Category"
        )
    ]
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
    # INSERT NEW ROWS
    # ==========================================================

    if (nrow(new_rows_insert) > 0) {

      for (i in seq_len(nrow(new_rows_insert))) {

        row <- new_rows_insert[
          i,
          ,
          drop = FALSE
        ]


        # --------------------------------------------------------
        # CHECK REQUIRED FIELDS
        # --------------------------------------------------------

        missing_fields <- required_fields[
          !required_fields %in% names(row) |
            sapply(
              row[required_fields],
              function(x) {
                is.na(x) ||
                  as.character(x) == ""
              }
            )
        ]


        if (length(missing_fields) > 0) {

          showNotification(
            paste0(
              "Skipping row ",
              i,
              " due to missing fields: ",
              paste(
                missing_fields,
                collapse = ", "
              )
            ),
            type = "warning",
            duration = 5
          )

          next
        }


        # --------------------------------------------------------
        # MAP COLUMN NAMES
        # --------------------------------------------------------

        sql_colnames <- r_to_sql_map[
          names(row)
        ]


        columns_sql <- paste(
          DBI::dbQuoteIdentifier(
            conn,
            sql_colnames
          ),
          collapse = ", "
        )


        placeholders <- paste(
          paste0(
            "$",
            seq_along(sql_colnames)
          ),
          collapse = ", "
        )


        # --------------------------------------------------------
        # INSERT
        # --------------------------------------------------------

        insert_query <- paste0(
          'INSERT INTO "Drop_Down_List" (',
          columns_sql,
          ") VALUES (",
          placeholders,
          ') RETURNING "ID"'
        )


        params <- lapply(
          row,
          function(x) {

            value <- x[[1]]

            if (
              length(value) == 0 ||
              is.na(value)
            ) {
              return(NA)
            }

            value
          }
        )


        new_id <- DBI::dbGetQuery(
          conn,
          insert_query,
          params = params
        )$ID


        cat(
          "Inserted Drop_Down_List ID:",
          as.character(new_id),
          "\n"
        )
      }


      showNotification(
        "✅ New rows inserted successfully.",
        type = "message"
      )
    }


    # ==========================================================
    # UPDATE EXISTING ROWS
    # ==========================================================

    if (nrow(existing_rows) > 0) {

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
            'UPDATE "Drop_Down_List" SET ',
            set_clause,
            ' WHERE "ID" = $',
            length(changed_fields) + 1
          )


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


      showNotification(
        "✅ Existing rows updated successfully.",
        type = "message"
      )
    }


    # ==========================================================
    # DELETE ROWS
    # ==========================================================

    if (nrow(rows_to_delete) > 0) {

      ids <- as.integer(
        rows_to_delete$ID
      )


      placeholders <- paste(
        paste0(
          "$",
          seq_along(ids)
        ),
        collapse = ", "
      )


      delete_query <- paste0(
        'DELETE FROM "Drop_Down_List" ',
        'WHERE "ID" IN (',
        placeholders,
        ")"
      )


      DBI::dbExecute(
        conn,
        delete_query,
        params = as.list(ids)
      )


      showNotification(
        "✅ Rows deleted successfully.",
        type = "message"
      )
    }


    # ==========================================================
    # COMMIT
    # ==========================================================

    DBI::dbCommit(conn)

    transaction_committed <- TRUE


    # ==========================================================
    # REFRESH FROM POSTGRESQL
    # ==========================================================

    latest_data <- DBI::dbReadTable(
      conn,
      "Drop_Down_List"
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
        "✅ Data synchronized with PostgreSQL!",
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
          "Failed to synchronize data:",
          e$message
        ),
        easyClose = TRUE
      )
    )


    message(
      paste(
        "Error during Drop_Down_List save:",
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

  rows_to_store(
    empty_like(
      request_input()
    )
  )

  changes_to_store(
    empty_like(
      request_input()
    )
  )

  deleted_rows_to_store(
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
    "Add new row" = {
    add_row_logic()
    },
    "Save" = {
      save_logic()
      },
    "Delete rows" = {
      delete_row_logic()
      }
  )
})

  })}
