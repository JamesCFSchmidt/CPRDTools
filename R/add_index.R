#' Add an index to a database.
#'
#' Indexing allows for query speed improvements through the generation of an index on a specified field.
#' This is performed singularly for each field.
#'
#' @param db_path The file path to the database location.
#' @param index_name The unique name given to the index.
#' @param index_table The table in which the field to be indexed is located.
#' @param index_field The field to be indexed.
#'
#' @export
#'
add_index <- function(db_path,
                      index_name,
                      index_table,
                      index_field){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(index_name)){
    stop("Specify name of the index")}
  if(missing(index_table)){
    stop("Specify table of the index")}
  if(missing(index_field)){
    stop("Specify field corresponding to the index")}
  if(!any(is.character(c(db_path,index_name,index_table,index_field)))){
    stop("Specify db_path, index_name, index_table, index_field as.char")}
  sqlite <- DBI::dbDriver("SQLite")
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  start_time <- Sys.time()
  statement = paste0("CREATE INDEX ",
                     index_name,
                     " ON ",
                     index_table,
                     " (",index_field,");")
  query <- DBI::dbSendStatement(connex,statement)
  DBI::dbClearResult(query)
  DBI::dbDisconnect(connex)
  end_time <- Sys.time()
  time_diff <- end_time-start_time
  out_list <- list("database_location" = db_path,
                   "index" = index_name,
                   "index_statement" = statement,
                   "query_time" = time_diff
  )
  return(out_list)
  rm(connex,sqlite,db_path,index_name,index_field,index_table,start_time,
     end_time,time_diff,query)
}
