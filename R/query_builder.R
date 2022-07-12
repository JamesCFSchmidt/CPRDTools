#' Data query using defined user inputs.
#'
#' The user inputs are used to generate a SQL code, used to extract the specified data.
#'
#' Note this function should only be applied in simple data query situations.
#' It only allows for a singular join between two tables with limited customisability.
#'
#' @param db_path string, the file path to the database location.
#' @param unique_obs Logical, for distinct (by all extracted fields) final observations.
#' @param field_list string vector, the list of fields extracted from the main table.
#' @param from_table string, the main table from which the fields will be extracted. Data from a secondary table can be joined to this tables.
#' @param join_table string, the secondary table that will be joined to the main table.
#' @param join_fields string vector, the list of fields extracted from the secondary table.
#' @param join_type string, the join type: inner, left or cross.
#'  *Default LEFT JOIN.
#' @param join_on string, the variable found in both the main table (from_table) and secondary table (join_table) used to join the tables together.
#' @param where_table string, Where clause used to subset or filter data, specify table where the clause will apply.
#' @param where_filter string, the filtering value or condition.
#' @param order_field string, field on which the order clause applies.
#' @param order_type string, ascending (ASC) or descending (DESC) order.
#'  *Default DESC.
#' @param limit_to integer, number to limit the return of data to.
#'
#' @export
#'
query_builder <- function(db_path,
                          unique_obs,
                          field_list,
                          from_table,
                          join_table,
                          join_fields,
                          join_type,
                          join_on,
                          where_table,
                          where_filter,
                          order_field,
                          order_type,
                          limit_to){
  #===============================
  #errors--------------------------
  if(missing(db_path)){
    stop("Specify database file path location")}
  if(missing(field_list)){
    stop("Specify columns to extract")}
  if(missing(from_table)){
    stop("Specify main table for extract")}
  if(!any(is.character(c(db_path,field_list,from_table)))){
    stop("Specify db_path, field_list, from_table as.char")}
  if(!missing(join_table)){
    if(!any(is.character(join_table))){
      stop("Specify join_table as.char")}}
  if(!missing(join_fields)){
    if(!any(is.character(join_fields))){
      stop("Specify join_fields as.char")}}
  if(!missing(join_type)){
    if(!any(is.character(join_type))){
      stop("Specify join_type as.char")}}
  if(!missing(join_on)){
    if(!any(is.character(join_on))){
      stop("Specify join_on as.char")}}
  if(!missing(where_filter)){
    if(!any(is.character(where_filter))){
      stop("Specify where_filter as.char")}}
  if(!missing(order_field)){
    if(!any(is.character(order_field))){
      stop("Specify order_field as.char")}}
  if(!missing(order_type)){
    if(!any(is.character(order_type))){
      stop("Specify order_type as.char")}}
  if(!missing(limit_to)){
    if(!any(is.numeric(limit_to))){
      stop("Specify limit_to as.num")}}
  connex <- DBI::dbConnect(RSQLite::SQLite(),dbname=paste0(db_path,"/database.db"))
  start_time <- Sys.time()
  loaded_tables <- tolower(DBI::dbListTables(connex))
  #unique observations-------------
  distinct <- ifelse(missing(unique_obs),"",{
    if(!is.logical(unique_obs)){
      stop("Specify unique_obs as logical T/F")}
    ifelse(unique_obs==T," DISTINCT ","")})
  #extract table-------------------
  if(tolower(from_table)%in%loaded_tables==F){
    stop("Review main table for extract")}
  #column extract list-------------
  col_names_from <- tolower(DBI::dbListFields(connex,from_table))
  if(any(field_list%in%c("ALL","All","all","*"))){field_list=col_names_from}else{
    if(!all(tolower(field_list)%in%col_names_from)){
      stop("Review extract column(s) specified")}
  }
  #columns in join table-----------
  col_names_join = NULL
  j_l = NULL
  if( missing(join_table)&!missing(join_on)&!missing(join_fields)|
      !missing(join_table)& missing(join_on)&!missing(join_fields)|
      !missing(join_table)&!missing(join_on)& missing(join_fields)){
    stop("Review join - table, column or joining variable")}
  if(!missing(join_table)&!missing(join_on)&!missing(join_fields)){
    if(length(join_table)>1)stop("Only single joins permitted, for more complex joins use query_direct()")
    if(tolower(join_table)%in%loaded_tables==F){
      stop("Review join table for extract")}
    col_names_join <- tolower(DBI::dbListFields(connex,join_table))
    if(any(join_fields%in%c("ALL","All","all","*"))){
      join_fields=col_names_join
      j_l <- matrix(nrow=length(join_fields),ncol=1)
      for(j in 1:length(j_l)){j_l[j] <- paste0(join_table,".",join_fields[j])}}else{
        if(!all(tolower(join_fields)%in%col_names_join)){
          stop("Review join extract column(s) specified")}else{
          j_l <- matrix(nrow=length(join_fields),ncol=1)
          for(j in 1:length(j_l)){j_l[j] <- paste0(join_table,".",join_fields[j])}}
      }
  }
  col_names <- unique(append(col_names_from,col_names_join))
  rm(j)
  #join----------------------------
  join <- ifelse(missing(join_table)&missing(join_on),"",{
    ifelse(missing(join_table)&missing(join_on)==F|missing(join_table)==F&missing(join_on),{
      stop("Review join columns or table specified for join")},{
        if(length(join_on)>2)stop("join_on can take a maximum of 2 fields")
        if(length(join_on)>1){j1=tolower(join_on[1])
        j2=tolower(join_on[2])}else{
          j1=tolower(join_on)
          j2=tolower(join_on)}
        ifelse(tolower(join_table)%in%loaded_tables&any(j1%in%col_names_from)&any(j2%in%col_names_join),{
          ifelse(missing(join_type),
                 paste0("LEFT JOIN ",join_table," ON ",join_table,".",j2," = ",from_table,".",j1),{
                   join_type <- stringi::stri_trans_toupper(join_type,locale="en")
                   ifelse(any(join_type%in%c("INNER JOIN","LEFT JOIN","CROSS JOIN")),{
                     paste0(join_type," ",join_table," ON ",join_table,".",j2," = ",from_table,".",j1)},{
                       ifelse(any(join_type%in%c("INNER","LEFT","CROSS")),{
                         paste0(join_type," JOIN ",join_table," ON ",join_table,".",j2," = ",from_table,".",j1)
                       },stop("review join type, only INNER, LEFT or CROSS permitted"))
                     })
                 })
        },stop("Review join column specified for join"))
      })
  })
  rm(j1,j2)
  #where condition-----------------
  where <- ifelse(missing(where_filter),
                  "",
                  paste0("WHERE ",
                         ifelse(!tolower(where_table)%in%tolower(c(from_table,join_table)),
                                stop("review where_table, not found in main or joining tables"),
                                where_table),
                         ".",
                         where_filter))
  #order condition-----------------
  order <- ifelse(missing(order_field),"",{
    ifelse(tolower(order_field)%in%col_names_from & missing(order_type),{
      order=paste0("ORDER BY ",from_table,".",order_field," DESC")},{
        ifelse(tolower(order_field)%in%col_names_from & missing(order_type)==F,{
          order_type <- ifelse(substr(order_type,0,1)%in%c('A', 'a'),
                               substr(stri_trans_toupper(order_type,locale="en"),0,3),
                               substr(stri_trans_toupper(order_type,locale="en"),0,4))
          if(!any(order_type%in%c('ASC','DESC'))){
            stop('review order type, only ASC or DESC allowed')}
          paste0("ORDER BY ",from_table,".",order_field," ",order_type)},{
            ifelse(tolower(order_field)%in%col_names_join & missing(order_type),{
              order=paste0("ORDER BY ",join_table,".",order_field," DESC")},{
                ifelse(tolower(order_field)%in%col_names_from & missing(order_type)==F,{
                  order_type <- ifelse(substr(order_type,0,1)%in%c('A', 'a'),
                                       substr(stri_trans_toupper(order_type,locale="en"),0,3),
                                       substr(stri_trans_toupper(order_type,locale="en"),0,4))
                  if(!any(order_type%in%c('ASC','DESC'))){
                    stop('review order type, only ASC or DESC allowed')}
                  paste0("ORDER BY ",join_table,".",order_field," ",order_type)},
                  stop("Review order column spcified")
                )}
            )}
        )}
    )}
  )
  #limit number of rows------------
  limit <- ifelse(missing(limit_to),"",{
    ifelse(!is.numeric(limit_to),{
      stop("Specify limit_to as numeric")},{
      paste0("LIMIT ",limit_to)})
  })
  f_l <- matrix(nrow=length(field_list),ncol=1)
  for(j in 1:length(f_l)){
    f_l[j] <- paste0(from_table,".",field_list[j])
  }
  rm(j)
  fields <- append(f_l,j_l)
  rm(f_l,j_l)
  f <- matrix(nrow=length(fields),ncol=1)
  for(k in 1:length(fields)){
    if(k!=length(fields)){
      f[k] <- paste0(fields[k],", ")}else{
        f[k] <- paste0(fields[k])
      }
  }
  rm(k,fields)
  query <- paste(
    "SELECT",distinct,paste(f,collapse=""),
    "FROM",from_table,
    join,
    where,
    order,
    limit,
    ";")
  query_data <- data.frame(DBI::dbGetQuery(connex,query))
  end_time <- Sys.time()
  time_diff <- end_time-start_time
  loaded_tables <- DBI::dbListTables(connex)
  DBI::dbDisconnect(connex)
  out_list <- list("database_location" = db_path,
                   "database_tables" = loaded_tables,
                   "query" = query,
                   "query_data" = query_data,
                   "query_time" = time_diff
  )
  return(out_list)
  rm(connex,db_path,unique_obs,field_list,from_table,join_type,join_table,join_on,
     where_filter,order_field,order_type,limit_to,loaded_tables,col_names,join_fields,
     distinct,join,where,order,limit,query,query_data,f,col_names_from,col_names_join,
     start_time,end_time,time_diff)
}
