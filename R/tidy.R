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
            character = c(7, 8, 9, 6, 1, 3, 10, 12, 13, 14),
            numeric   = c(11)
          ),
          col.names    = c(
            "eid",
            "rd",
            "dt",
            "role",
            "first_name",
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
    civilian_race   = recode(civilian_race, type = "race"),
    civilian_gender = recode(civilian_gender, type = "gender")
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

  setkey(d,
         "last_name",
         "first_name",
         "appointed",
         "birth",
         "date",
         "start")

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

  return(d)

}


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
        civilian_race   = recode(civilian_race, type = "race"),
        civilian_gender = recode(civilian_gender, type = "gender"),
        civilian_age    = utils::type.convert(civilian_age, as.is = TRUE),
        type
      )
    ][!is.na(dt)]

  return(d)

}


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
        header       = TRUE,
        showProgress = FALSE,
        na.strings   = "",
        select       = list(
          character = c(
            "TRR_REPORT_ID",
            "DTE",
            "TMEMIL",
            "DATETIME",
            "POLAST",
            "POFIRST",
            "APPOINTED_DATE",
            "SUBRACE",
            "SUBGNDR",
            "SUBYEARDOB"
          ),
          logical   = c(
            "MEMBER_IN_UNIFORM",
            "DUTYSTATUS",
            "SUBJECT_INJURED"
          )
        ),
        col.names    = tolower
      )
    )

  a <-
    lapply(
      a,
      \(x)
      if (any(grepl(pattern = "datetime", colnames(x)))) {
        x[, `:=`(
          dt        = as.POSIXct(datetime,
                                 format = "%Y-%b-%d %H%M",
                                 tz = "GMT"),
          appointed = as.POSIXct(appointed_date,
                                 format = "%Y-%b-%d",
                                 tz = "GMT")
        )]
      } else {
        x[, `:=`(
          dt        = as.POSIXct(paste(dte, sprintf("%04s", tmemil)),
                                 format = "%Y-%m-%d %H%M",
                                 tz = "GMT"),
          appointed = as.POSIXct(appointed_date,
                                 format = "%Y-%m-%d",
                                 tz = "GMT")
        )]
      }
    )

  a <-
    rbindlist(
      lapply(
        a,
        \(x)
        x[,
          .(
            eid              = trr_report_id,
            dt,
            on_duty          = dutystatus,
            uniform          = member_in_uniform,
            last_name        = polast,
            first_name       = pofirst,
            appointed        = fasttime::fastDate(appointed),
            civilian_injured = subject_injured,
            civilian_race    = recode(subrace, type = "race"),
            civilian_gender  = recode(subgndr, type = "gender"),
            civilian_birth   = utils::type.convert(subyeardob, as.is = TRUE)
          )
        ]
      )
    )

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

  b <- b[person == "Member Action"][
    ,
    .(
      eid = trr_report_id,
      action
    )
  ]

  d <- unique(b)[unique(a), on = "eid"][!is.na(dt)]

  return(d)

}



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
    civilian_race   = recode(civilian_race,   type = "race"),
    civilian_gender = recode(civilian_gender, type = "gender"),
    birth           = utils::type.convert(birth, as.is = TRUE)
  )]

  return(d)

}


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

