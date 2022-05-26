read_ticket <- function(path) {
  
  d <-
    fread(
      path, 
      select = c("ticket_number", "issue_date", "officer",
                 "fine_level1_amount", "ticket_queue","violation_code",
                 "geocoded_lng", "geocoded_lat", "geocode_accuracy",
                 "unit_description")
    )[issue_date >= ymd("2014-01-01") & unit_description == "CPD"][
      , .(eid      = ticket_number,
          dt       = issue_date,
          star     = as.character(officer),
          fine     = fine_level1_amount,
          lon      = geocoded_lng,
          lat      = geocoded_lat,
          queue    = ticket_queue,
          accuracy = geocode_accuracy,
          violation_code)
    ]
  
  return(d)
  
}
