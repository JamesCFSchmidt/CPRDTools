#' Load single CPRD GOLD table.
#'
#' Used to load a single CPRD GOLD table compiled and appended of multiple raw .txt (.zip) files.
#'
#' Can overwrite table already contained in database.
#'
#' Note: can be used when CPRD GOLD tables (not files) are spread across multiple file locations.
#'
#' @param db_path string, the file path to the database location.
#' @param file_location string, the file path location of files to be loaded.
#' @param table_name string, the name of the table to be loaded (or already loaded) in the database.
#' @param zip logical, if the files are compressed.
#'  *'TRUE' (default): files compressed.
#'  *'FALSE': files uncompressed.
#' @param overwrite logical, if table already exist in database and requires overwriting.
#'  *'TRUE': table exists, delete table and write in new data.
#'  *'FALSE' (default): new table to be loaded.
#'
#' @export
#'
load_table <- function(db_path,
                       file_location,
                       table_name,
                       zip=TRUE,
                       overwrite=FALSE){
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
  n_cprd <- length(cprd_files)
  if(missing(zip)){stop("Specify file compression")}else{
    if(!is.logical(zip)){stop("Specify zip as logical T/F")}else{
      if(zip==T){
        cprd_files_list <- data.frame("files"=list.files(path=file_location,
                                                         pattern="*.zip",
                                                         full.names=T))}
      else{
        cprd_files_list <- data.frame("files"=list.files(path=file_location,
                                                         pattern="*.txt",
                                                         full.names=T))}}}
  n <- length(cprd_files_list[,1])
  cprd_files_list$table <- 0
  for(i in 1:n_cprd){
    for(j in 1:n){
      if(stringr::str_detect(tolower(
        substr(cprd_files_list[j,1],regexpr("\\/[^\\/]*$",
                                            cprd_files_list[j,1])+1,
               nchar(as.character(cprd_files_list[j,1])))),
        tolower(cprd_files[i]))){
        cprd_files_list[j,2] = cprd_files[i]}
    }
  }
  tables <- data.frame("Table"=sort(unique(cprd_files_list$table)),
                       "File_Count"=stats::aggregate(cprd_files_list$table,
                                              by=list(cprd_files_list$table),
                                              FUN=length)[,2])
  rm(i,j,file_location,n)
  cprd_files_list <- cprd_files_list[order(cprd_files_list$table),]
  if(any(tolower(table_name)%in%tolower(loaded_tables))&missing(overwrite)){stop("Table already exists, consider overwrite")}else{
    if(any(tolower(table_name)%in%tolower(loaded_tables))&overwrite==F){stop("Table already exists, reconsider overwrite")}else{
      if(any(tolower(table_name)%in%tolower(loaded_tables))&overwrite==T){
        DBI::dbRemoveTable(connex,table_name)
        message(cat(crayon::red(paste0("-------Overwrite = TRUE, DELETION OF ",table_name," COMPLETE-------\n"))))}
    }
  }
  load <- data.frame()
  if(!tolower(table_name)%in%tolower(tables$Table)){
    stop("Review table specified for loading")}
  else{
    files <- as.matrix(cprd_files_list[which(tolower(cprd_files_list[,2])==tolower(table_name)),][,1])
    nfiles <- length(files)
    for(j in 1:nfiles){
      tmp <- readr::read_delim(files[j],"\t",escape_double=F,col_names=T,t=T)
      date_col <- intersect(names(tmp),date_cols)
      if(length(date_col)){
        for(m in date_col){
          tmp[[m]] <- as.character(as.Date(tmp[[m]],"%d/%m/%Y"))}
      }
      if("pracid" %in% names(tmp)==F & "patid" %in% names(tmp)){
        tmp$pracid <- as.numeric(substr(tmp$patid,nchar(tmp$patid)-2,nchar(tmp$patid)))
      }
      RSQLite::dbWriteTable(connex,name=paste(table_name),value=tmp,append=T)
      message(cat(crayon::green(paste0("----------LOAD OF TABLE ",table_name,", FILE No. ",j," SUCCESSFUL----------\n"))))
      l <- data.frame("Tab"=tables$Table,"Num"=as.numeric(j),"byte"=as.numeric(file.size(files[j])),"nfile"=nfiles)
      load <- rbind(load,l)
      rm(tmp)
    }
    rm(j,l,files,nfiles)
  }
  loaded_files <- data.frame(cbind(stats::aggregate(load$Tab,by=list(load$Tab),FUN=length)[1],
                                   stats::aggregate(load$Num,by=list(load$Tab),FUN=length)[2],
                                   stats::aggregate(load$nfile,by=list(load$Tab),FUN=max)[2],
                                   round(stats::aggregate(load$byte,by=list(load$Tab),FUN=sum)[2]/1048576,4),
                                   round(stats::aggregate(load$byte,by=list(load$Tab),FUN=sum)[2]/1073741824,4)))
  names(loaded_files) <- c("table","load_total","file_total","size_Mb","size_Gb")
  end_time <- Sys.time()
  time_diff <- end_time-start_time
  loaded_tables <- DBI::dbListTables(connex)
  DBI::dbDisconnect(connex)
  out_list <- list("database_location" = db_path,
                   "files_in_location" = cprd_files_list,
                   "tables_in_location" = tables,
                   "loaded_table" = table_name,
                   "load_report" = loaded_files,
                   "database_tables" = loaded_tables,
                   "load_start" = start_time,
                   "load_end" = end_time,
                   "load_time" = time_diff
  )
  return(out_list)
  rm(connex,db_path,table_name,loaded_tables,tables,date_cols,
     overwrite,zip,load,start_time,end_time,time_diff)
}
