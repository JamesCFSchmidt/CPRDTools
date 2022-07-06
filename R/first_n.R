#' Display the first n rows (records) in a table.
#'
#' @param db_path string, the file path to the database location.
#' @param table_name string, the name of the table to be interrogated.
#' @param n integer, the number of rows to be displayed, default n = 10.
#'
#' @export
#'
first_n <- function(db_path,
                    table_name,
                    n){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(table_name)){
    stop("Specify table name")}
  if(!any(is.character(c(db_path,table_name)))){
    stop("Specify db_path, table_name as.char")}
  sqlite <- DBI::dbDriver("SQLite")
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  loaded_tables <- tolower(DBI::dbListTables(connex))
  if(tolower(table_name) %in% loaded_tables==F){
    stop("Review table spcified")}
  if(missing(n)){n=10}
  query <- paste0("SELECT * FROM ",table_name," LIMIT ",n,";")
  query_data <- data.frame(DBI::dbGetQuery(connex,query))
  DBI::dbDisconnect(connex)
  out_list <- list("database_location" = db_path,
                   "query" = query,
                   "first_n_rows" = query_data
  )
  return(out_list)
  rm(connex,sqlite,db_path,table_name,n,query,query_data,loaded_tables)
}
