#' Update an existing CPRD GOLD table.
#'
#' Appends a single CPRD GOLD specific .txt (.zip) file to an existing table.
#'
#' @param db_path string, the file path to the database location.
#' @param file_location string, the file path location of the file to be loaded.
#' @param table_name string, the name of the table in the database to be updated.
#'
#' @export
#'
update_table <- function(db_path,
                         file_location,
                         table_name){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(file_location)){
    stop("Specify location of files")}
  if(missing(table_name)){
    stop("Specify names of tables to load")}
  if(!any(is.character(c(db_path,file_location,table_name)))){
    stop("Specify db_path, file_location, table_name as.char")}
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  start_time <- Sys.time()
  date_cols <- c("chsdate","frd","crd","tod","deathdate","lcd","uts","eventdate","sysdate")
  loaded_tables <- DBI::dbListTables(connex)
  cprd_files <- c("Additional","Clinical","Consultation","Immunisation","Immunization",
                  "Patient","Practice","Referral", "Staff", "Test", "Therapy")
  file_name <- substr(file_location,
                      regexpr("\\/[^\\/]*$",file_location)+1,
                      nchar(as.character(file_location)))
  load <- data.frame()
  if(!any(tolower(table_name)%in%tolower(loaded_tables))){
    stop("Review table specified for updating, table not in database")}
  else{
    tmp <- readr::read_delim(file_location,"\t",escape_double=F,col_names=T,t=T)
    date_col <- intersect(names(tmp),date_cols)
    if(length(date_col)){
      for(m in date_col){
        tmp[[m]] <- as.character(as.Date(tmp[[m]],"%d/%m/%Y"))}
    }
    if("pracid" %in% names(tmp)==F & "patid" %in% names(tmp)){
      tmp$pracid <- as.numeric(substr(tmp$patid,nchar(tmp$patid)-2,nchar(tmp$patid)))
    }
    RSQLite::dbWriteTable(connex,name=paste(table_name),value=tmp,append=T)
    message(cat(crayon::green(paste0("----------LOAD OF TABLE ",table_name,", FILE ",file_name," SUCCESSFUL----------\n"))))
    l <- data.frame("Tab"=table_name,"file"=file_name, "byte"=as.numeric(file.size(file_location)))
    load <- rbind(load,l)
    rm(tmp)
  }
  rm(l,m)
  loaded_files <- data.frame(cbind(load$Tab,load$file,round(load$byte/1048576,4),round(load$byte/1073741824,4)))
  names(loaded_files) <- c("table","file_name","size_Mb","size_Gb")
  end_time <- Sys.time()
  time_diff <- end_time-start_time
  loaded_tables2 <- DBI::dbListTables(connex)
  DBI::dbDisconnect(connex)
  out_list <- list("database_location" = db_path,
                   "file_to_load" = file_location,
                   "tables_in_location" = loaded_tables,
                   "loaded_table" = table_name,
                   "load_report" = loaded_files,
                   "database_tables" = loaded_tables2,
                   "load_start" = start_time,
                   "load_end" = end_time,
                   "load_time" = time_diff
  )
  return(out_list)
  rm(connex,db_path,table_name,loaded_tables,loaded_tables2,date_col,
     load,start_time,end_time,time_diff,loaded_files,cprd_files)
}
