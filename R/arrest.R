#' Preprocess arrest data
#'
#' @import data.table
#'
#' @param file_report A vector of file names in working directory or path
#' to .csv files containing primary arrest data, e.g. FOIA p701162
#' @param file_officer A vector of file names in working directory or path
#' to .csv file containing corresponding officer data, e.g. FOIA p708085
#'
#' @return A data.table containing joined arrest data
#' @export
#'
#' @examples
#'
tidy_arrest <- function(file_report, file_officer) {

  a <-
    rbindlist(
      lapply(
        file_report,
        \(x)
        fread(
          file         = x,
          header       = TRUE,
          showProgress = FALSE,
          na.strings   = "",
          select       = list(
            character = c(7, 8, 9, 6, 1, 2, 3, 10, 12, 13, 14),
            numeric   = c(11)
          ),
          col.names    = c(
            "eid",
            "rd",
            "dt",
            "role",
            "first_name",
            "initial",
            "last_name",
            "civilian_race",
            "civilian_gender",
            "statute",
            "charge_type",
            "civilian_age"
          )
        )
      )
    )

  a[, `:=`(
    dt              = as.POSIXct(dt, format = "%d-%b-%Y %H:%M", tz = "GMT"),
    civilian_race   = str_consistency(civilian_race, feature = "race"),
    civilian_gender = str_consistency(civilian_gender, feature = "gender")
  )]

  b <-
    rbindlist(
      lapply(
        file_officer,
        \(x)
        fread(
          file         = x,
          header       = TRUE,
          showProgress = FALSE,
          na.strings   = "",
          colClasses   = list(
            character = c(1, 2, 3, 4, 5, 6),
            numeric   = c(7)
          ),
          col.names    = c(
            "eid",
            "dt",
            "role",
            "first_name",
            "last_name",
            "appointed",
            "birth"
          )
        )
      )
    )

  b[, `:=`(
    dt        = as.POSIXct(dt, format = "%d-%b-%Y %H:%M", tz = "GMT"),
    appointed = fasttime::fastDate(appointed)
  )]

  # join and return
  join_on <- intersect(colnames(a), colnames(b))
  d <- unique(b[eid != "J"])[unique(a[eid != "J"]), on = join_on]

  return(d)

}

