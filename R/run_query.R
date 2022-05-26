run_query <- function(panel, response, query) {
  #' write query using substitute()
  
  # return appropriate time period for response
  end <- ceiling_date(max(response$dt), unit = "day")
  
  # reduce panel to time period
  panel <- setDT(panel)[dt_end < end][, dt_end := NULL][]

  # get outcome vector
  r <- response[eval(get(query))][
    , .(y = uniqueN(eid, na.rm = TRUE)), by = aid]
  
  # join outcome vector
  panel <- r[panel, on = "aid"]
  setnafill(panel, cols = "y", fill = 0)
  
  setkey(panel, oid)
  return(panel)

}
