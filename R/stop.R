#' Preprocess investigatory stop report data
#'
#' @import data.table
#'
#' @param file File name in working directory or path to .csv file containing
#' investigatory stop report data
#'
#' @return A data.table containing investigatory stop report data
#' @export
#'
#' @examples
#'
tidy_isr <- function(file) {

  d <- fread(
    file,
    header       = TRUE,
    showProgress = FALSE,
    na.strings   = "",
    select = list(
      character = c(
        "CONTACT_CARD_ID",
        "CONTACT_DATE_TIME",
        "FO_LAST",
        "FO_FIRST",
        "FO_APPOINTED_DT",
        "SO_LAST",
        "SO_FIRST",
        "SO_APPOINTED_DATE",
        "RACE",
        "SEX_CODE_CD"
      ),
      numeric   = c(
        "FO_BIRTH_YR",
        "SO_BIRTH_YR",
        "AGE"
      ),
      logical = c(
        "ENFORCEMENT_ACTION_TAKEN_I",
        "OTHER_REASONABLE_SUSPICION_I",
        "SUSPECT_NARCOTIC_ACTIVITY_I",
        "SUSPICIOUS_OBJECT_I",
        "PAT_DOWN_I",
        "SEARCH_I",
        "VEHICLE_INVOLVED_I"
      )
    ),
    col.names    = c(
      "eid",
      "dt",
      "first.last_name",
      "first.first_name",
      "first.appointed",
      "second.last_name",
      "second.first_name",
      "second.appointed",
      "civilian_race",
      "civilian_gender",
      "first.birth",
      "second.birth",
      "civilian_age",
      "enforcement_action",
      "other_reasonable_suspicion",
      "suspect_narcotic_activity",
      "suspicious_object",
      "pat_down",
      "search",
      "vehicle_involved"
    )
  )

  d <-
    dcast(
      melt(
        data = unique(d),
        measure.vars = measure(role, feature, sep = "."),
        variable.factor = FALSE
      ),
      formula = ... ~ feature,
      value.var = "value"
    )

  d[, `:=`(
    dt              = as.POSIXct(x = dt, format = "%d-%b-%Y %H:%M", tz = "GMT"),
    appointed       = fasttime::fastDate(appointed),
    civilian_race   = str_consistency(civilian_race,   type = "race"),
    civilian_gender = str_consistency(civilian_gender, type = "gender"),
    birth           = utils::type.convert(birth, as.is = TRUE)
  )]

  d <- d[!is.na(last_name)]

  return(d)

}
