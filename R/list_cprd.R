#' List all CPRD GOLD files in a specified location.
#'
#' List CPRD GOLD files in only a single location, can accept the use of sub-folders:
#'     i.e.
#'     C:/CPRD_Extract/patient_data, C:/CPRD_Extract/practice_data,
#'     vs
#'     C:/CPRD_Extract/Patient/patient_data, C:/CPRD_Extract/Practice/practice_data,
#'
#' @param file_location string, the file path location of files to be listed.
#' @param folder logical, the use of sub-folders.
#'  *'TRUE': Sub-folders used, the data are in sub-folders specific to a table.
#'  *'FLASE' (default): no sub-folders are used, the data in single location.
#' @param zip logical, if the files are compressed.
#'  *'TRUE' (default): files compressed.
#'  *'FALSE': files uncompressed.
#'
#' @export
#'
list_cprd <- function(file_location,
                      folder=FALSE,
                      zip=TRUE){
  if(missing(file_location)){
    stop("Specify location of files")}
  if(!is.character(file_location)){
    stop("Specify file_location as.char")}
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
  files <- data.frame()
  for(i in 1:nrow(cprd_files_list)){
    f <- substr(cprd_files_list[i,1], nchar(file_location)+1,nchar(cprd_files_list[i,1]))
    files <- rbind(files,f)
  }
  if(dim(files)[1]==0){
    stop("Error in file_location or subfolder specification, folder=T")}
  n <- length(files[,1])
  files$table <- 0
  for(i in 1:n_cprd){
    for(j in 1:n){
      if(stringr::str_detect(tolower(
        substr(files[j,1],regexpr("\\/[^\\/]*$",
                                            files[j,1])+1,
               nchar(as.character(files[j,1])))),
        tolower(cprd_files[i]))){
        files[j,2] = cprd_files[i]}
    }
  }
  tables <- data.frame("Table"=sort(unique(files$table)),
                       "File_Count"=stats::aggregate(files$table,
                                              by=list(files$table),
                                              FUN=length)[,2])
  files <- files[order(files$table),]
  colnames(files) <- c("Files","Table")
  out_list <- list("file_location" = file_location,
                   "all_files_tables" = files,
                   "tables" = tables
  )
  return(out_list)
  rm(i,j,file_location,n,n_cprd,files,f)
}
