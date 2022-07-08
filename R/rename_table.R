#' Rename a table in a database.
#'
#' @param db_path string, the file path to the database location.
#' @param old_name string, the table to be renamed.
#' @param new_name string, the new table name.
#'
#' @export
#'
rename_table <- function(db_path,
                         old_name,
                         new_name){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(old_name)){
    stop("Specify current table to be renamed")}
  if(missing(new_name)){
    stop("Specify new table name")}
  if(!any(is.character(c(db_path,old_name,new_name)))){
    stop("Specify db_path, old_name or new_name as.char")}
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  before_tables <- tolower(DBI::dbListTables(connex))
  if(!any(tolower(old_name)%in%before_tables)){stop("Review table to be renamed")}
  query <- DBI::dbSendStatement(connex,paste0("ALTER TABLE ",old_name," RENAME TO ",new_name,";"))
  DBI::dbClearResult(query)
  loaded_tables <- DBI::dbListTables(connex)
  out_list <- list("database_location" = db_path,
                   "before_rename" = before_tables,
                   "old_table_name"= old_name,
                   "new_table_name"= new_name,
                   "after_rename" = loaded_tables
  )
  return(out_list)
  DBI::dbDisconnect(connex)
  rm(connex,db_path,old_name, new_name,loaded_tables)
}
