read_beat <- function(path) {
  
  d <- 
    read_excel(
      path,
      sheet        = 2,
      .name_repair = make_clean_names
    )
  
  setDT(d)
  return(d)
  
}

