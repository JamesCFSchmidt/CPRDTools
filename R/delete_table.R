#' Delete a single or multiple tables in a database.
#'
#' @param db_path string, the file path to the database location.
#' @param remove_tables string vector, the table(s) to be removed.
#'
#' @export
#'
delete_table <- function(db_path,
                         remove_tables){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(remove_tables)){
    stop("Specify tables to remove")}
  if(!any(is.character(c(db_path,remove_tables)))){
    stop("Specify db_path, remove_tables as.char")}
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  before_tables <- tolower(DBI::dbListTables(connex))
  if(!any(tolower(remove_tables)%in%before_tables)){stop("Review table(s) to be removed")}
  for(i in 1:length(remove_tables)){
    DBI::dbRemoveTable(connex,remove_tables[i])
  }
  loaded_tables <- DBI::dbListTables(connex)
  out_list <- list("database_location" = db_path,
                   "before_drop" = before_tables,
                   "dropped_table"= remove_tables,
                   "after_drop" = loaded_tables
  )
  return(out_list)
  DBI::dbDisconnect(connex)
  rm(connex,db_path,remove_tables,before_tables,loaded_tables,i)
}
