#' Execute direct SQL statement.
#'
#' Used for data manipulation, database maintenance and improvements.
#' i.e. UPDATE, DELETE, INSERT INTO, DROP TABLE
#' Note does not output data.
#'
#' @param db_path string, the file path to the database location.
#' @param statement string, the SQL statement to be submitted.
#'
#' @export
#'
statement_direct <- function(db_path,
                             statement){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(statement)){
    stop("Specify statement to be executed")}
  if(!any(is.character(c(db_path,statement)))){
    stop("Specify db_path, statement as.char")}
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  start_time <- Sys.time()
  query <- DBI::dbSendStatement(connex,statement)
  DBI::dbClearResult(query)
  end_time <- Sys.time()
  time_diff <- end_time-start_time
  loaded_tables <- DBI::dbListTables(connex)
  DBI::dbDisconnect(connex)
  out_list <- list("database_location" = db_path,
                   "database_tables" = loaded_tables,
                   "statement" = statement,
                   "query_time" = time_diff
  )
  return(out_list)
  rm(connex,db_path,statement,query,loaded_tables,start_time,end_time,time_diff)
}
