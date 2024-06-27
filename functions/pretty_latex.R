pretty_latex <- function(str){
  
  str = gsub("([><])", "$\\1$", str, fixed=FALSE)
  str = gsub("%", "\\\\%", str, fixed=FALSE)
  str = gsub("&", "\\\\&", str, fixed=FALSE)
  str = gsub("NaN", "-", str, fixed=FALSE)
  str = gsub("`", "", str, fixed=FALSE)
  
  return(str)
}