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
  if(! names(database[[table]]) %in% col |> any()){ # Does any names in table contain `col`
    cli::cli_abort(c(
      "x" = "`{col}` is not a valid column name in the `{table}` table",
      "i" = "Check `names(database${table})` and try again!"
    )
    )
  }
}

# # Validate user input values in arguments
# # Table name
# if(! names(database) %in% table |> any()){
#   cli::cli_abort(
#     c(
#       "x" = "`{table}` is not a valid table name",
#       "i" = "Check `names(database)` and try again!"
#     )
#   )
# }
# 
# # Col name
# if(! names(database[[table]]) %in% col |> any()){
#   cli::cli_abort(c(
#     "x" = "`{col}` is not a valid column name in the `{table}` table",
#     "i" = "Check `names(database${table})` and try again!"
#   )  
#   )
# }
# 
# # Col value
# # Accomodating for multiple values supplied
# if(length(col_value) > 1) 
#   col_value <- paste(col_value, collapse = "|")
# 
# if(! stringr::str_detect(unique(database[[table]][col]) |> dplyr::pull(), col_value) |> any()){
#   cli::cli_abort(c(
#     "x" = "`{col_value}` is not a valid value in `{col}` of the `{table}` table",
#     "i" = "Check `unique(database${table}${col})` and try again!"
#   )
#   )
# }
