#' Display the fields in a table.
#'
#' @param db_path The file path to the database location.
#' @param table_name The name of the table to be interrogated.
#'
#' @export
#'
table_fields <- function(db_path,
                         table_name){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(table_name)){
    stop("Specify table name")}
  if(!any(is.character(c(db_path,table_name)))){
    stop("Specify db_path, table_name as.char")}
  sqlite <-DBI:: dbDriver("SQLite")
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  loaded_tables <- tolower(DBI::dbListTables(connex))
  if(tolower(table_name) %in% loaded_tables==F){
    stop("Review table specified")}
  col_names <- DBI::dbListFields(connex,table_name)
  DBI::dbDisconnect(connex)
  out_list <- list("database_location" = db_path,
                   "fields" = col_names
  )
  return(out_list)
  rm(connex,sqlite,db_path,table_name,col_names,loaded_tables)
}
