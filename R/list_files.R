#' List all files of a specified type in a specified location.
#'
#' @param file_location string, the file path location of files to be listed.
#' @param file_type string, the extension type of the files to be listed.
#'
#' @export
#'
#' @examples
list_files <- function(file_location,
                       file_type){
  if(missing(file_location)){
    stop("Specify location of files")}
  if(missing(file_type)){
    stop("Specify file type")}
  if(!any(is.character(c(file_location,file_type)))){
    stop("Specify file_location, file_type as.char")}
  if(tolower(file_type)=="all"){
    files_list <- data.frame("files"=list.files(path=file_location,
                                                full.names=T))
  }else{
    files_list <- data.frame("files"=list.files(path=file_location,
                                                pattern=file_type,
                                                full.names=T))
  }
  files <- data.frame()
  for(i in 1:nrow(files_list)){
    f <- substr(files_list[i,1], nchar(file_location)+1,nchar(files_list[i,1]))
    files <- rbind(files,f)
  }
  colnames(files) <- "files"
  out_list <- list("file_location" = file_location,
                   "files" = files
  )
  return(out_list)
  rm(file_location,files_list,files,f)
}
