#' Execute direct SQL query code.
#'
#' Allows for all the complexity and customisability available in an SQLite SELECT statement.
#'
#' @param db_path string, the file path to the database location.
#' @param query string, the SQL query code.
#'
#' @export
#'
query_direct <- function(db_path,
                         query){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(query)){
    stop("Specify query to executed")}
  if(!any(is.character(c(db_path,query)))){
    stop("Specify db_path, query as.char")}
  sqlite <- DBI::dbDriver("SQLite")
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  start_time <- Sys.time()
  query_data <- data.frame(DBI::dbGetQuery(connex,query))
  end_time <- Sys.time()
  time_diff <- end_time-start_time
  loaded_tables <- DBI::dbListTables(connex)
  DBI::dbDisconnect(connex)
  out_list <- list("database_location" = db_path,
                   "database_tables" = loaded_tables,
                   "query" = query,
                   "query_data" = query_data,
                   "query_time" = time_diff
  )
  return(out_list)
  rm(connex,sqlite,db_path,query,query_data,loaded_tables,start_time,end_time,time_diff)
}
