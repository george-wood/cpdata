#' Preprocess use of force data
#'
#' @import data.table
#'
#' @param file_report File name in working directory or path to .csv file
#' containing primary tactical response report data, e.g. date, time, officer
#' @param file_action File name in working directory or path to .csv file
#' containing corresponding action data, e.g. escort holds
#'
#' @return A data.table containing joined tactical response report data
#' @export
#'
#' @examples
#'
tidy_force <- function(file_report, file_action) {

  a <-
    lapply(
      file_report,
      \(x)
      fread(
        x,
        header           = TRUE,
        showProgress     = FALSE,
        na.strings       = "",
        blank.lines.skip = TRUE,
        col.names        = tolower,
        select           = list(
          character = c(
            "TRR_REPORT_ID",
            "DTE",
            "TMEMIL",
            "DATETIME",
            "POLAST",
            "POFIRST",
            "APPOINTED_DATE",
            "SUBRACE",
            "SUBGNDR"
          ),
          numeric  = c(
            "POAGE",
            "POYRofBIRTH",
            "SUBYEARDOB"
          ),
          logical  = c(
            "MEMBER_IN_UNIFORM",
            "DUTYSTATUS",
            "SUBJECT_INJURED"
          )
        )
      )
    )

  a <-
    rbindlist(a, fill = TRUE)[
      , dt := fifelse(is.na(dte), NA, paste(dte, sprintf("%04s", tmemil)))
    ][, `:=`(
      dt = fcase(
        nchar(dt) == 15,
        as.POSIXct(dt,       format = "%Y-%m-%d %H%M", tz = "GMT"),
        nchar(dt) <= 14,
        as.POSIXct(dt,       format = "%m/%d/%y %H%M", tz = "GMT"),
        is.na(dt),
        as.POSIXct(datetime, format = "%Y-%b-%d %H%M", tz = "GMT")
      ),
      appointed = fcase(
        nchar(appointed_date) == 11,
        as.POSIXct(appointed_date, format = "%Y-%b-%d", tz = "GMT"),
        nchar(appointed_date) == 10,
        as.POSIXct(appointed_date, format = "%Y-%m-%d", tz = "GMT"),
        nchar(appointed_date) <= 9,
        as.POSIXct(appointed_date, format = "%m/%d/%y", tz = "GMT")
      )
    )
    ][, `:=`(
      birth_upper = fifelse(
        is.na(poage), poyrofbirth, year(dt) - poage
      ),
      birth_lower = fifelse(
        is.na(poage), poyrofbirth, year(dt) - poage - 1
      )
    )
    ][,
      .(
        eid              = trr_report_id,
        dt,
        on_duty          = dutystatus,
        uniform          = member_in_uniform,
        last_name        = polast,
        first_name       = pofirst,
        appointed        = fasttime::fastDate(appointed),
        birth_lower,
        birth_upper,
        civilian_injured = subject_injured,
        civilian_race    = str_consistency(subrace, feature = "race"),
        civilian_gender  = str_consistency(subgndr, feature = "gender"),
        civilian_birth   = utils::type.convert(subyeardob, as.is = TRUE)
      )
    ]

  b <-
    rbindlist(
      lapply(
        file_action,
        \(x)
        fread(
          x,
          header       = TRUE,
          showProgress = FALSE,
          na.strings   = "",
          select       = list(
            character = c(
              "TRR_REPORT_ID",
              "PERSON",
              "ACTION"
            )
          ),
          col.names    = tolower
        )
      )
    )

  b <-
    b[person == "Member Action"][
      ,
      .(
        eid = trr_report_id,
        action
      )
    ]

  d <- unique(b)[unique(a), on = "eid"][!is.na(dt)]

  return(d)

}
