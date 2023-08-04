
#' Preprocess ticket data
#'
#' @import data.table
#'
#' @param file File name in working directory or path to .csv file containing
#' ticket data
#' @param zip logical (default is \code{FALSE}). Set to \code{TRUE} if the .csv
#' file is zipped and file will be unzipped prior to reading.
#'
#'
#' @return A data.table containing ticket data
#' @export
#'
#' @examples
#' file <- "parking_tickets.csv"
#' tidy_ticket(
#'   system.file("extdata", file, package = "cpdata", mustWork = TRUE)
#' )
tidy_ticket <- function(file, zip = FALSE) {
  
  fnm <- file
  cmd <- NULL
  
  if (zip) {
    cmd  <- paste("unzip -p ", file)
    file <- NULL
  }
  
  d <- fread(
    file         = file,
    cmd          = cmd,
    header       = TRUE,
    showProgress = FALSE,
    na.strings   = "",
    select       = list(
      character = c(
        "officer",
        "ticket_number",
        "issue_date",
        "violation_code",
        "violation_description",
        "unit_description",
        "unit",
        "ticket_queue",
        "ticket_queue_date",
        "hearing_disposition",
        "geocoded_lng",
        "geocoded_lat",
        "geocode_accuracy"
      ),
      numeric   = c(
        "fine_level1_amount",
        "fine_level2_amount",
        "current_amount_due",
        "total_payments"
      )
    ),
    col.names    = c(
      "star",
      "eid",
      "dt",
      "violation_code",
      "violation",
      "department",
      "unit",
      "queue",
      "queue_date",
      "disposition",
      "longitude",
      "latitude",
      "accuracy",
      "fine_1",
      "fine_2",
      "current_amount_due",
      "total_payments"
    )
  )
  
  d[, `:=`(
    dt          = fasttime::fastPOSIXct(dt, tz = "GMT"),
    queue_date  = fasttime::fastPOSIXct(queue_date, tz = "GMT"),
    disposition = fifelse(disposition == "", NA, disposition),
    unit        = fifelse(unit == "NULL", NA, unit),
    longitude   = utils::type.convert(longitude, as.is = TRUE),
    latitude    = utils::type.convert(latitude,  as.is = TRUE),
    accuracy    = utils::type.convert(accuracy,  as.is = TRUE)
  )][]
  
  return(d)
  
}

