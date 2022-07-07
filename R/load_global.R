#' Load table with a file from the global environment.
#'
#' Best used when importing or manipulating data prior to loading into database.
#'
#' @param db_path string, the file path to the database location.
#' @param file_to_load string, the file in the global environment to be loaded.
#' @param table_name string, the name of the table to be loaded (or already loaded) in the database.
#' @param overwrite logical, if table already exist in database and requires overwriting.
#'  *'TRUE': table exists, delete table and write in new data.
#'  *'FALSE' (default): new table to be loaded.
#'
#' @export
#'
load_global <- function(db_path,
                        file_to_load,
                        table_name,
                        overwrite=FALSE){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(file_to_load)){
    stop("Specify file to load")}
  if(missing(table_name)){
    stop("Specify name of table to load")}
  if(!any(is.character(c(db_path,table_name)))){
    stop("Specify db_path,table_name as.char")}
  sqlite <- DBI::dbDriver("SQLite")
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  loaded_tables <- DBI::dbListTables(connex)
  if(all(table_name%in%loaded_tables) & (missing(overwrite)==T|overwrite==F)){stop("Table already exists, (re)consider overwrite")}
  if(all(table_name%in%loaded_tables) & overwrite==T){DBI::dbRemoveTable(connex,table_name)
    message(cat(crayon::red(paste0("-------Overwrite = TRUE, DELETION OF ",table_name," COMPLETE-------\n"))))}
  RSQLite::dbWriteTable(connex,name=table_name,
               value=file_to_load,
               append=T)
  message(cat(crayon::green(paste0("----------LOAD OF TABLE ",table_name," SUCCESSFUL----------\n"))))
  loaded_tables <- DBI::dbListTables(connex)
  out_list <- list("database_location" = db_path,
                   "table_loaded" = table_name,
                   "database_tables" = loaded_tables
  )
  DBI::dbDisconnect(connex)
  return(out_list)
  rm(connex,sqlite,file_to_load,db_path,loaded_tables,table_name)
}
