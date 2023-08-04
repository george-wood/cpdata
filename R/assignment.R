#' Preprocess assignment data
#'
#' @import data.table
#'
#' @param file A vector of file names in working directory or path
#' to .csv files containing assignment and attendance data
#'
#' @return A data.table containing assignment data
#' @export
#'
#' @examples
#'
tidy_assignment <- function(file) {
  
  d <-
    rbindlist(
      lapply(
        file,
        \(x)
        fread(
          file         = x,
          header       = TRUE,
          showProgress = FALSE,
          na.strings   = "",
          colClasses   = list(
            character = c(1:5, 8:14, 16, 18:21),
            numeric   = c(6, 7, 15),
            logical   = c(17)
          ),
          col.names    = c(
            "date",
            "unit",
            "watch",
            "beat",
            "car",
            "start",
            "end",
            "last_name",
            "first_name",
            "initial",
            "rank",
            "star",
            "gender",
            "race",
            "birth",
            "appointed",
            "present_for_duty",
            "absence_code",
            "absence",
            "modified_by_last",
            "modified_by_first",
            "modified_date"
          )
        )
      )
    )
  
  setkey(
    d,
    "last_name",
    "first_name",
    "appointed",
    "birth",
    "date",
    "start"
  )
  
  d <- d[
    d[, .I[which.max(modified_date)], by = eval(key(d))]$V1
  ]
  
  d[, oid := .GRP, by = setdiff(key(d), c("date", "start"))]
  
  d <-
    d[,
      .(
        aid      = as.character(1:.N),
        oid      = as.character(oid),
        dt_start = as.POSIXct(
          x      = paste(date, sprintf("%04s", start)),
          format = "%d-%b-%Y %H%M",
          tz     = "UTC"
        ),
        dt_end   = as.POSIXct(
          x      = paste(date, sprintf("%04s", end)),
          format = "%d-%b-%Y %H%M",
          tz     = "UTC"
        ),
        unit,
        watch,
        beat,
        car,
        last_name,
        first_name,
        initial,
        rank,
        star,
        gender    = recode(gender, type = "gender"),
        race      = recode(race, type = "race"),
        birth,
        appointed = fasttime::fastDate(appointed),
        present_for_duty,
        absence_code,
        absence
      )
    ]
  
  d <- d[!is.na(dt_start) & !is.na(dt_end)]
  d[, dt_end := fifelse(dt_end < dt_start, dt_end + (1*60*60*24), dt_end)][]
  
  return(d)
  
}

