#' Display table(s) loaded in database.
#'
#' @param db_path The file path to the database location.
#'
#' @export
#'
db_tables <- function(db_path){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(!any(is.character(c(db_path)))){
    stop("Specify db_path as.char")}
  sqlite <- DBI::dbDriver("SQLite")
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  loaded_tables <- DBI::dbListTables(connex)
  DBI::dbDisconnect(connex)
  out_list <- list("database_location" = db_path,
                   "tables" = loaded_tables
  )
  return(out_list)
  rm(connex,sqlite,db_path,loaded_tables)
}
