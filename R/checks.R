# Check missingness

check_arg_missingness <- function(database, col, col_value){
  if(rlang::is_missing(database) | rlang::is_missing(col) | rlang::is_missing(col_value))
    cli::cli_abort(c(
      "x" = "`database`, `col` or `col_value` must be supplied!"
    )
    )
}

# Check if table name exists 
# Note that users can supply the $traits table to database, for that reason table can be NA
# First check if table is NA

check_table_name_exists <- function(database, table){
  if(!is.na(table) & !tibble::is_tibble(database)){ # database is NOT $traits and `table` is supplied
    if(! names(database) %in% table |> any()){ # Does any names of the tables in database contain `table` 
      cli::cli_abort(
        c(
          "x" = "`{table}` is not a valid table name",
          "i" = "Check `names(database)` and try again!"
        )
      )
    } 
  } 
}

# Check if col exists in specified table when database is traits.build object
check_col_exists_in_table <- function(database, table, col){
  # If traits table supplied and no table is specified
  if(tibble::is_tibble(database)){
    if(! names(database) %in% col |> any()){ # Does any names in table contain `col`
      cli::cli_abort(c(
        "x" = "`{col}` is not a valid column name in the `traits` table",
        "i" = "Check `names(database$traits)` and try again!"
      )
      )
    }
  } else(
    if(! names(database[[table]]) %in% col |> any()){ # Does any names in table contain `col`
      cli::cli_abort(c(
        "x" = "`{col}` is not a valid column name in the `{table}` table",
        "i" = "Check `names(database${table})` and try again!"
      )
      )
    }
  )
}

# Check if col_value exists in the col after attempted extraction
# Accommodating for multiple values supplied AND partial matching

check_col_value_exists <- function(ret, table, col, col_value){
  if(tibble::is_tibble(ret)){
    if(nrow(ret) == 0)
      cli::cli_abort(c(
        "x" = "`{col_value}` is not a valid value in `{col}` column of the `traits` table",
        "i" = "Check spelling of `{col_value}` and try again!"
      )
      )
  } else(
  
  if(nrow(ret$traits) == 0)
    cli::cli_abort(c(
      "x" = "`{col_value}` is not a valid value in `{col}` column of the `{table}` table",
      "i" = "Check spelling of `{col_value}` and try again!"
    )
    )
  )
}

# 
# # Get possible col values
# available_values <- database[[table]][col] |> dplyr::pull() |> unique()
# 
# # Check if there are non-matches
# if(length(col_value) > 1) 
#   concat_col_value <- paste(col_value, collapse = "|")
# 
# partial_matches <- stringr::str_detect(available_values, concat_col_value)
# 
# if(length(partial_matches) > 1)
# 
# # Prompt user which one is non-match
# if(length(no_match) >= 1){
#   cli::cli_warn(c("x" = "`{no_match}` is not a valid value in `{col}` of the `{table}` table"))
# }

# # Check if col_value exists in the col
# # Accommodating for multiple values supplied
# 
# # Get possible col values
# available_values <- database[[table]][col] |> dplyr::pull() |> unique()
# 
# # Check if there are non-matches
# no_match <- col_value[which(! col_value %in% available_values)]
# 
# # Identify matches
# matches <- col_value[which(col_value %in% available_values)]
# 
# # Prompt user which one is non-match
# if(length(no_match) > 0){
#   cli::cli_warn("`{no_match}` is not a valid value in `{col}` of the `{table}` table")
#   cli::cli_alert_success("Continuing data extraction for {.val {matches}}")
# }
