#' Delete an entire database.
#'
#' Prompted with y/n input on deletion.
#'
#' @param db_path The file path to the database location.
#'
#' @export
#'
delete_db <- function(db_path){
  if(missing(db_path)){
    stop("Specify database")}
  if(!is.character(db_path)){
    stop("Specify db_path as.char")}
  sqlite <- DBI::dbDriver("SQLite")
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  loaded_tables <- DBI::dbListTables(connex)
  message(cat(crayon::red("----------WARNING, DELETION IN PROGRESS----------\n")))
  ans <- readline(prompt="Press y+[enter] to proceed, n+[enter] to stop: ")
  ifelse(ans %in% c("y","Y")==T,{
    file.remove(paste0(db_path,'/database.db'))},{
      stop("Deletion Stopped!")
    })
  DBI::dbDisconnect(connex)
  out_list <- list("database_location" = db_path,
                   "tables" = loaded_tables
  )
  return(out_list)
  rm(connex,sqlite,db_path,loaded_tables,ans)
}
