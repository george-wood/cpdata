#' Preprocess arrest data
#'
#' @import data.table
#'
#' @param file_report File name in working directory or path to .csv file
#' containing primary arrest data, e.g. date, time, civilian attributes
#' @param file_officer File name in working directory or path to .csv file
#' containing corresponding officer data, e.g. appointed date
#'
#' @return A data.table containing joined arrest data
#' @export
#'
#' @examples
#'
tidy_arrest <- function(file_report, file_officer) {

  a <- fread(
    file         = file_report,
    header       = TRUE,
    showProgress = FALSE,
    na.strings   = "",
    select       = c(7, 8, 9, 6, 1, 3, 10, 12, 11, 13, 14),
    col.names    = c(
      "eid",
      "rd",
      "dt",
      "role",
      "first_name",
      "last_name",
      "civilian_race",
      "civilian_gender",
      "civilian_age",
      "statute",
      "charge_type"
    )
  )

  a[, dt := as.POSIXct(dt, format = "%d-%b-%Y %H:%M", tz = "UTC")]

  b <- fread(
    file         = file_officer,
    header       = TRUE,
    showProgress = FALSE,
    na.strings   = "",
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

  b[, dt := as.POSIXct(dt, format = "%d-%b-%Y %H:%M", tz = "UTC")]

  join_on <- c("eid", "dt", "first_name", "last_name", "role")
  d <- unique(b)[unique(a), on = join_on]

  message("Finished preprocessing: ", file_report, " & ", file_officer)
  return(d)

}


#' Preprocess assignment data
#'
#' @import data.table
#'
#' @param file File name in working directory or path to .csv file containing
#' assignment and attendance data
#'
#' @return A data.table containing assignment data
#' @export
#'
#' @examples
#'
tidy_assignment <- function(file) {

  d <- fread(
    file         = file,
    header       = TRUE,
    showProgress = FALSE,
    na.strings   = "",
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
    ),
    key          = c(
      "last_name",
      "first_name",
      "appointed",
      "birth",
      "date",
      "start"
    )
  )

  d <- d[
    d[, .I[which.max(modified_date)], by = eval(key(d))]$V1
  ]

  d[, oid := .GRP, by = setdiff(key(d), c("date", "start"))]

  d <-
    d[,
      .(
        oid,
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
        gender,
        race,
        birth,
        appointed,
        present_for_duty,
        absence_code,
        absence
      )
    ]

  d <- d[!is.na(dt_start) & !is.na(dt_end)]

  message("Finished preprocessing: ", file)
  return(d)

}


#' Preprocess contact card data
#'
#' @import data.table
#'
#' @param file File name in working directory or path to .csv file containing
#' contact card data
#'
#' @return A data.table containing contact card data
#' @export
#'
#' @examples
#'
tidy_contact <- function(file) {

  d <- fread(
    file,
    header       = TRUE,
    showProgress = FALSE,
    na.strings   = "",
    select       = c(
      "Contact Date",
      "DATE",
      "Time of Stop",
      "TIME",
      "FirstPOLN",
      "1st P.O. LAST NAME",
      "FirstPOFN",
      "1st P.O. FIRST NAME",
      "FirstPOAge (on date of stop)",
      "1st P.O. AGE",
      "SecPOLN",
      "2nd P.O. LAST NAME",
      "SecPOFN",
      "2nd P.O.FIRST NAME",
      "SecPOAge (on date of stop)",
      "2nd P.O. AGE",
      "SubRace",
      "SUBJECT RACE",
      "SubSex",
      "SUBJECT SEX",
      "Contact Type",
      "CONTACT TYPE DESCRIPTION"
    ),
    col.names = c(
      "date",
      "time",
      "first.last_name",
      "first.first_name",
      "first.age",
      "second.last_name",
      "second.first_name",
      "second.age",
      "race",
      "sex",
      "type"
    )
  )

  d <-
    dcast(
      melt(
        data = unique(d)[, eid := 1:.N],
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
            tz     = "UTC"
          ),
          as.POSIXct(
            x      = paste(date, sprintf("%04s", time)),
            format = "%d-%b-%y %H:%M",
            tz     = "UTC"
          )
        ),
        role,
        last_name,
        first_name,
        age,
        race,
        sex,
        type
      )
    ][!is.na(dt)]

  message("Finished preprocessing: ", file)
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

  a <- fread(
    file_report,
    header       = TRUE,
    showProgress = FALSE,
    na.strings   = "",
    select       = toupper(
      c("trr_report_id",
        "dte",
        "tmemil",
        "datetime",
        "dutystatus",
        "polast",
        "pofirst",
        "appointed_date",
        "subject_injured",
        "subrace",
        "subgndr")
    ),
    col.names    = tolower
  )

  if (any(grepl(pattern = "datetime", colnames(a)))) {
    a[, dt := as.POSIXct(datetime,
                         format = "%Y-%b-%d %H%M",
                         tz = "UTC")][]
  } else {
    a[, dt := as.POSIXct(paste(dte, sprintf("%04s", tmemil)),
                         format = "%Y-%m-%d %H%M",
                         tz = "UTC")][]
  }

  a <-
    a[,
      .(
        eid              = trr_report_id,
        dt,
        on_duty          = dutystatus,
        last_name        = polast,
        first_name       = pofirst,
        appointed        = appointed_date,
        civilian_injured = subject_injured,
        civilian_race    = subrace,
        civilian_gender  = subgndr
      )
    ][]

  b <- fread(
    file_action,
    header       = TRUE,
    showProgress = FALSE,
    na.strings   = "",
    select       = toupper(
      c("trr_report_id",
        "person",
        "action")
    ),
    col.names    = tolower
  )

  b <- b[person == "Member Action"][
    ,
    .(
      eid = trr_report_id,
      action
    )
  ]

  d <- unique(b[a, on = "eid"])

  message("Finished preprocessing: ", file_report, " & ", file_action)
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
    select       = toupper(
      c("contact_card_id",
        "contact_date_time",
        "fo_last",
        "fo_first",
        "fo_appointed_dt",
        "fo_birth_yr",
        "so_last",
        "so_first",
        "so_appointed_date",
        "so_birth_yr",
        "race",
        "sex_code_cd",
        "enforcement_action_taken_i",
        "other_reasonable_suspicion_i",
        "suspect_narcotic_activity_i",
        "suspicious_object_i",
        "pat_down_i",
        "search_i",
        "vehicle_involved_i")
    ),
    col.names    = c(
      "eid",
      "dt",
      "first.last_name",
      "first.first_name",
      "first.appointed",
      "first.birth",
      "second.last_name",
      "second.first_name",
      "second.appointed",
      "second.birth",
      "race",
      "gender",
      "enforcement",
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

  d[, dt := as.POSIXct(x = dt, format = "%d-%b-%Y %H:%M", tz = "UTC")][]

  message("Finished preprocessing: ", file)
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
    select       = c(
      "officer",
      "ticket_number",
      "issue_date",
      "violation_code",
      "violation_description",
      "unit_description",
      "unit",
      "fine_level1_amount",
      "fine_level2_amount",
      "current_amount_due",
      "total_payments",
      "ticket_queue",
      "ticket_queue_date",
      "hearing_disposition",
      "geocoded_lng",
      "geocoded_lat",
      "geocode_accuracy"
    ),
    col.names    = c(
      "star",
      "ticket",
      "dt",
      "violation_code",
      "violation",
      "department",
      "unit",
      "fine_1",
      "fine_2",
      "current_amount_due",
      "total_payments",
      "queue",
      "queue_date",
      "hearing_disposition",
      "longitude",
      "latitude",
      "accuracy"
    )
  )

  message("Finished preprocessing: ", fnm)
  return(d)

}

