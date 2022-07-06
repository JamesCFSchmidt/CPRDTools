#' Convert a specified field in a dataset to a date field.
#'
#' Note date format provided is input format of the field to be converted.
#'
#' @param data The dataset in which a field must be converted.
#' @param date_fields The field to be converted.
#' @param format The input format of the field to be converted.
#'
#' @export
#'
convert_dates <- function(data,
                          date_fields,
                          format){
  if(missing(data)){
    stop("Specify data for date conversion")}
  if(!all(date_fields%in%names(data))){
    stop("Review date column(s) specified")}
  if(!is.character(date_fields)){
    stop("Specify date_fields, format as.char")}
  na_before <- sum(is.na(data))
  if(missing(format)){
    format="%d/%m/%Y"
    message(cat(crayon::green(paste0("----------FORMAT %d/%m/%Y USED----------\n"))))
  }else{
    if(!is.character(date_fields)){
      stop("Specify format as.char")}
  }
  data_new = data
  if(length(date_fields)){
    for(m in date_fields){
      data_new[[m]] <- as.Date(data[[m]],format=paste0(format),origin="01/01/1970")
    }
  }
  na_after <- sum(is.na(data_new))
  ifelse(na_after>na_before,{
    message(cat(crayon::red(paste0("----------WARNING, some values have been converted to N/A----------\n"))))
    ans <- readline(prompt="Press y+[enter] to proceed, n+[enter] to stop: ")
    ifelse(ans %in% c("n"," N"),{stop("Date covertion stopped!")},{data_out=data_new})},{
      data_out=data_new})
  out_list <- list("converted_fields" = date_fields,
                   "convert_data" = data_out
  )
  return(out_list)
  rm(data,date_fields,na_before,na_after,data_new,data_out,m,format)
}
