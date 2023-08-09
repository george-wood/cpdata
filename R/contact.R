#' Preprocess contact card data
#'
#' @import data.table
#'
#' @param file A vector of file names in working directory or path
#' to .csv files containing contact card data
#'
#' @return A data.table containing contact card data
#' @export
#'
#' @examples
#'
tidy_contact <- function(file) {

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
          select       = list(
            character = c(
              "Contact Date",
              "DATE",
              "Time of Stop",
              "TIME",
              "FirstPOLN",
              "1st P.O. LAST NAME",
              "FirstPOFN",
              "1st P.O. FIRST NAME",
              "SecPOLN",
              "2nd P.O. LAST NAME",
              "SecPOFN",
              "2nd P.O.FIRST NAME",
              "SubRace",
              "SUBJECT RACE",
              "SubSex",
              "SUBJECT SEX",
              "Contact Type",
              "CONTACT TYPE DESCRIPTION"
            ),
            numeric   = c(
              "FirstPOAge (on date of stop)",
              "1st P.O. AGE",
              "SecPOAge (on date of stop)",
              "2nd P.O. AGE",
              "SubAge",
              "SUBJECT AGE"
            )
          ),
          col.names    = c(
            "date",
            "time",
            "first.last_name",
            "first.first_name",
            "second.last_name",
            "second.first_name",
            "civilian_race",
            "civilian_gender",
            "type",
            "first.age",
            "second.age",
            "civilian_age"
          )
        )
      )
    )


  d <-
    dcast(
      melt(
        data = unique(d)[, eid := as.character(1:.N)],
        measure.vars = measure(role, feature, sep = "."),
        variable.factor = FALSE
      ),
      formula = ... ~ feature,
      value.var = "value"
    )

  d <-
    d[,
      .(
        eid,
        dt = fifelse(
          grepl(pattern = "2014|2015", date),
          as.POSIXct(
            x      = paste(date, time),
            format = "%Y-%m-%d %H:%M:%S",
            tz     = "GMT"
          ),
          as.POSIXct(
            x      = paste(date, sprintf("%04s", time)),
            format = "%d-%b-%y %H:%M",
            tz     = "GMT"
          )
        ),
        role,
        last_name,
        first_name,
        age             = utils::type.convert(age, as.is = TRUE),
        civilian_race   = str_consistency(civilian_race, type = "race"),
        civilian_gender = str_consistency(civilian_gender, type = "gender"),
        civilian_age    = utils::type.convert(civilian_age, as.is = TRUE),
        type
      )
    ][, `:=`(
      birth_upper = year(dt) - age,
      birth_lower = year(dt) - age - 1
    )
    ][!is.na(dt)]

  return(d)

}


