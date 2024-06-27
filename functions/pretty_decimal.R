pretty_decimal <- function(x, dec_var=var){
  
  if(dec_var %in% c("pim3rom","prism3pod","rate")){
    
    return(paste0(prettyNum(round(100*x, 2), big.mark=","), "%"))
    
  }else{
    
    return(prettyNum(round(x, 2), big.mark=","))  
  
  }
  
}