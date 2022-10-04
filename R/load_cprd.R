#' Load all CPRD GOLD data.
#'
#' Loads all CPRD GOLD datasets: Additional, Clinical, Consultation, Immunisation, Patient,
#' Practice, Referral, Staff, Test and Therapy into a SQLite database.
#'
#' Able to automatically load in all or selected tables.
#' Appends multiple .txt (.zip) files together to form a CPRD GOLD tables.
#' Makes use the CPRDTools::list_cprd() function to define a list of all CPRD GOLD datasets available in a given location.
#'
#' Note mapping is used to map CPRD GOLD description codes to a description file, the 'lookup' file.
#' This file is derived from CPRD
#'
#' @param db_path string, the file path to the database location.
#' @param file_location string, the file path location of files to be loaded.
#' @param table_name string vector, the name of the table to be loaded (or already loaded) in the database.
#'  *can specify 'all', 'All' or 'ALL' or c('patient','practice')
#' @param folder logical, the use of sub-folders.
#'  *'TRUE': Sub-folders used, the data are in sub-folders specific to a table.
#'  *'FLASE' (default): no sub-folders are used, the data in single location.
#' @param zip logical, if the files are compressed.
#'  *'TRUE' (default): files compressed.
#'  *'FALSE': files uncompressed.
#' @param load_mapping Logical indicator if the mapping/look-up files are to be loaded.
#'  *'TRUE': no lookup mapping table required.
#'  *'FALSE' (default): load CPRD GOLD lookup mapping table.
#' @param overwrite logical, if table already exist in database and requires overwriting.
#'  *'TRUE': table exists, delete table and write in new data.
#'  *'FALSE' (default): new table to be loaded.
#'
#' @export
#'
load_cprd <- function(db_path,
                      file_location,
                      tables_to_load,
                      folder = FALSE,
                      zip = TRUE,
                      load_mapping = FALSE,
                      overwrite = FALSE){
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(file_location)){
    stop("Specify location of files")}
  if(missing(tables_to_load)){
    stop("Specify names of tables to load")}
  if(!any(is.character(c(db_path,file_location,tables_to_load)))){
    stop("Specify db_path, file_location, tables_to_load as.char")}
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  start_time <- Sys.time()
  date_cols <- c("chsdate","frd","crd","tod","deathdate","lcd","uts","eventdate","sysdate")
  loaded_tables <- DBI::dbListTables(connex)
  cprd_files <- c("Additional","Clinical","Consultation","Immunisation","Immunization",
                  "Patient","Practice","Referral", "Staff", "Test", "Therapy")
  n_cprd <- length(cprd_files)
  if(missing(folder)){
    stop("Specify if files seperated into folders")}else{
    if(missing(zip)){
      stop("Specify file compression")}else{
      if(!is.logical(folder)){
        stop("Specify folder as logical T/F")}else{
        if(!is.logical(zip)){
          stop("Specify zip as logical T/F")}else{
          if(folder==T){
            cprd_folder_list <- data.frame("files"=list.dirs(path=file_location,
                                                             recursive=F,
                                                             full.names=T))
            n_folders <- length(cprd_folder_list$files)
            cprd_files_list <- data.frame()
            for(i in 1:n_folders){
              if(zip==T){
                c_f_l <- data.frame("files"=list.files(path=paste(cprd_folder_list$files[i]),
                                                       pattern="*.zip",
                                                       full.names=T))}
              else{
                c_f_l <- data.frame("files"=list.files(path=paste(cprd_folder_list$files[i]),
                                                       pattern="*.txt",
                                                       full.names=T))}
              cprd_files_list <- rbind(cprd_files_list,c_f_l)}
            rm(i,n_folders,cprd_folder_list,c_f_l)}else{
              if(zip==T){
                cprd_files_list <- data.frame("files"=list.files(path=file_location,
                                                                 pattern="*.zip",
                                                                 full.names=T))}else{
                  cprd_files_list <- data.frame("files"=list.files(path=file_location,
                                                                   pattern="*.txt",
                                                                   full.names=T))}
            }
        }
      }
    }
  }

  if(dim(cprd_f)[1]==0){
    stop("Error in file_location or subfolder specification, folder=T")}
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
                       "File_Count"=aggregate(cprd_files_list$table,
                                              by=list(cprd_files_list$table),
                                              FUN=length)[,2])
  rm(i,j,n)
   if(any(tolower(tables_to_load)=="all")){
    if(any(tolower(cprd_files)%in%tolower(loaded_tables))){
      stop("tables_to_load='all': some tables already loaded into database, speficy tables_to_load as list or individually")}else{
        tables_to_load=tables$Table}}else{
          for(i in 1:length(tables_to_load)){
            if(any(tolower(tables_to_load)%in%tolower(loaded_tables)) & missing(overwrite)){stop("Table already exists, consider overwrite")}else{
              if(any(tolower(tables_to_load)%in%tolower(loaded_tables)) & overwrite==F){stop("Table already exists, reconsider overwrite")}else{
                if(any(tolower(tables_to_load)%in%tolower(loaded_tables)) & overwrite==T){
                  DBI::dbRemoveTable(connex,tables_to_load[i])
                  message(cat(crayon::red(paste0("-------Overwrite = TRUE, DELETION OF ",tables_to_load[i]," COMPLETE-------\n"))))}
              }
            }
          }
        }
  load <- data.frame()
  if(!all(tables_to_load %in% c("ALL","all","All"))){
    for(i in 1:length(tables_to_load)){
      if(tolower(tables_to_load[i])%in%tolower(tables$Table)==F){
        stop("Review tables specified for loading or subfoldering of file, folder=T")}else{
          files <- as.matrix(cprd_files_list[which(tolower(cprd_files_list[,2])==tolower(tables_to_load[i])),][,1])
          nfiles <- length(files)
          for(j in 1:length(files)){
            tmp <- readr::read_delim(files[j],"\t",escape_double=F,col_names=T,t=T)
            date_col <- intersect(names(tmp),date_cols)
            if(length(date_col)){
              for(m in date_col){
                tmp[[m]] <- as.character(as.Date(tmp[[m]],"%d/%m/%Y"))}
            }
            if("pracid" %in% names(tmp)==F){
              tmp$pracid <- as.numeric(substr(tmp$patid,nchar(tmp$patid)-2,nchar(tmp$patid)))
            }
            DBI::dbWriteTable(connex,name=paste(tables_to_load[i]),value=tmp,append=T)
            message(cat(crayon::green(paste0("----------LOAD OF TABLE ",tables_to_load[i],", FILE No. ",j," SUCCESSFUL----------\n"))))
            l <- data.frame("Tab"=tables$Table[i],"Num"=as.numeric(j),"byte"=as.numeric(file.size(files[j])),"nfile"=nfiles)
            load <- rbind(load,l)
            rm(tmp)
          }
          rm(j,l,files,nfiles)
        }
    }
  }else{
    for(i in 1:length(tables$Table)){
      files <- as.matrix(cprd_files_list[which(tolower(cprd_files_list[,2])==tolower(tables$Table[i])),][,1])
      nfiles <- length(files)
      for(j in 1:length(files)){
        tmp <- readr::read_delim(files[j],"\t",escape_double=F,col_names=T,t=T)
        date_col <- intersect(names(tmp),date_cols)
        if(length(date_col)){
          for(m in date_col){
            tmp[[m]] <- as.character(as.Date(tmp[[m]],"%d/%m/%Y"))}
        }
        if("pracid" %in% names(tmp)==F){
          tmp$pracid <- as.numeric(substr(tmp$patid,nchar(tmp$patid)-2,nchar(tmp$patid)))
        }
        DBI::dbWriteTable(connex,name=paste(tables$Table[i]),value=tmp,append=T)
        message(cat(crayon::green(paste0("----------LOAD OF TABLE ",tables$Table[i],", FILE No. ",j," SUCCESSFUL----------\n"))))
        l <- data.frame("Tab"=tables$Table[i],"Num"=as.numeric(j),"byte"=as.numeric(file.size(tmp)),"nfile"=nfiles)
        load <- rbind(load,l)
        rm(tmp,date_col)
      }
      rm(j,l,files,nfiles)
    }
  }
  rm(i)
  if(load_mapping==T){
    DBI::dbWriteTable(connex,
                          name='lookup_table',
                          value=lookup,
                          append=T)
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

  cprd_f <- data.frame()
  for(i in 1:nrow(cprd_files_list)){
    f <- substr(cprd_files_list[i,1], nchar(file_location)+1,nchar(cprd_files_list[i,1]))
    cprd_f <- rbind(cprd_f,f)
  }
  cprd_f <- cbind(cprd_f,cprd_files_list[,2])
  colnames(cprd_f) <- c("files","table")
  out_list <- list("database_location" = db_path,
                   "files_in_location" = cprd_f,
                   "tables_in_location" = tables,
                   "loaded_tables" = tables_to_load,
                   "load_report" = loaded_files,
                   "database_tables" = loaded_tables,
                   "load_start" = start_time,
                   "load_end" = end_time,
                   "load_time" = time_diff
  )
  DBI::dbDisconnect(connex)
  return(out_list)
  rm(connex,db_path,tables_to_load,loaded_tables,tables,date_cols,cprd_f,f,file_location,
     overwrite,load_mapping,zip,load,start_time,end_time,time_diff,n_cprd)
}
