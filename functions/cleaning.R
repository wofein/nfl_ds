require(lubridate)

#' Set Col Classes
#'
#' @param df : data frame to edit
#' @param classes : vector with one element for each column 
#'                  in df declaring what class that column
#'                  should be. If classes[i] is NA then column
#'                  i will not be edited.
set_col_classes <- function(df, classes){
  
  if(length(classes) != ncol(df)){
    stop("There must be as many elements in 'classes' as columns in 'df'. 
         You can pad 'classes' with NA's in the indeces of columns you don't need to change.")
  }
  
  for(i in 1:length(classes)){
    if(!is.na(classes[i])){
      if(tolower(classes[i]) %in% c("numeric", "num", "int","integer")){
        df[,i] <- as.numeric(df[,i])
      }else if(tolower(classes[i]) %in% c("char", "character")){
        df[,i] <- as.character(df[,i])
      }else if(tolower(classes[i]) %in% c("factor")){
        df[,i] <- as.factor(df[,i])
      }else if(tolower(classes[i]) %in% c("date")){
        df[,i] <- ymd(df[,i])
      }else if(tolower(classes[i]) %in% c("logical", "boolean", "bool", "t/f")){
        df[,i] <- as.logical(df[,i])
      }else{
        stop(paste("Can't parse", classes[i], "to a data type."))
      }
    }
  }
  return(df)
}


#' Fix NA's
#'
#' @param df : data frame
#' @param columns : column names to fix NA's in
#' @param fill : value to insert where you find NA's (default to 0)
fix_NAS <- function(df, columns, fill = 0 ){
  if(!all(columns %in% names(df))){
    stop("column names must all be columns in df")
  }
  
  for(col_name in columns){
    col_index <- which(names(df) == col_name)
    df[which(is.na(df[,col_index])),col_index] <- fill  
    
  }
  
  return(df)
}
