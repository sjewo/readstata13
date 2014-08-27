#' Construct File Path
#'
#' @param path path to dta file
#' @export
get.filepath <- function(path=""){
      if(substring(path, 1, 1)=="~")
        filepath <- path.expand(path)
      else
          filepath <- path
      
      if(!file.exists(filepath))
        return("File does not exist.")
      
      return(filepath)
    }