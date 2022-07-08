#' Load additional data into a database.
#'
#' Load in CPRD/non-CPRD specific files.
#' Accommodates varied file types.
#'
#' Note non-automated, loads only singular file into a table.
#' Note simple data import, no additional data importing specification available.
#' If more complex data importing specification needed, use CPRDTools::load_global.
#'
#' @param db_path string, the file path to the database location.
#' @param file_location string, the file path location of files to be loaded.
#' @param table_name string, the name of the table to be loaded (or already loaded) in the database.
#' @param type character file type: '.txt', '.csv', '.excel', '.xl', '.xls', '.xlsx', '.dta' or '.rds'
#' @param overwrite logical, if table already exist in database and requires overwriting.
#'  *'TRUE': table exists, delete table and write in new data.
#'  *'FALSE' (default): new table to be loaded.
#'
#' @export
#'
load_additional <- function(db_path,
                            file_location,
                            table_name,
                            type,
                            overwrite=FALSE){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(file_location)){
    stop("Specify loaction of file")}
  if(missing(table_name)){
    stop("Specify name of table to load")}
  if(!any(is.character(c(db_path,file_location,type,table_name)))){
    stop("Specify db_path, file_location, type, table_name as.char")}
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  start_time <- Sys.time()
  loaded_tables <- DBI::dbListTables(connex)
  if(all(table_name%in%loaded_tables) & (overwrite==F|missing(overwrite))){stop("Table already exists, (re)consider overwrite")}
  if(all(table_name%in%loaded_tables) & overwrite==T){DBI::dbRemoveTable(connex,table_name)
    message(cat(crayon::red(paste0("-------Overwrite = TRUE, DELETION OF ",table_name," COMPLETE-------\n"))))}
  ifelse(type==".txt",
         {RSQLite::dbWriteTable(connex,name=table_name,
                       value=readr::read_tsv(file_location),
                       append=T)},{
                         ifelse(type==".dta",
                                {RSQLite::dbWriteTable(connex,name=table_name,
                                              value=haven::read_dta(file_location),
                                              append=T)},{
                                                ifelse(type==".csv",
                                                       {RSQLite::dbWriteTable(connex,name=table_name,
                                                                     value=readr::read_csv2(file_location),
                                                                     append=T)},{
                                                                       ifelse(type==".rds",
                                                                              {RSQLite::dbWriteTable(connex,name=table_name,
                                                                                            value=readRDS(file_location),
                                                                                            append=T)},{
                                                                                              ifelse(type %in%c(".excel",".xl",".xls",".xlsx"),
                                                                                                     {RSQLite::dbWriteTable(connex,name=table_name,
                                                                                                                   value=readxl::read_excel(file_location),
                                                                                                                   append=T)},{
                                                                                                                     stop("Review file type. Only .txt, .csv, .dta, .rds, .excel, .xl, .xls , .xlsx allowed")}
                                                                                              )}
                                                                       )}
                                                )}
                         )}
  )
  message(cat(crayon::green(paste0("----------LOAD OF TABLE ",table_name," SUCCESSFUL----------\n"))))
  end_time <- Sys.time()
  time_diff <- end_time-start_time
  loaded_tables <- DBI::dbListTables(connex)
  out_list <- list("database_location" = db_path,
                   "table_loaded" = table_name,
                   "database_tables" = loaded_tables,
                   "load_start" = start_time,
                   "load_end" = end_time,
                   "load_time" = time_diff
  )
  DBI::dbDisconnect(connex)
  return(out_list)
  rm(connex,db_path,file_location,type,table_name,overwrite,loaded_tables,
     start_time,end_time,time_diff)
}
