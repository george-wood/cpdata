#' Recode race and ethnicity or gender feature
#'
#' @import data.table
#'
#' @param x Vector of race and ethnicity or gender values
#' @param type Type of feature; currently accepts "race" or "gender"
#'
#' @return Consistent values of race and ethnicity or gender feature
#' @export
#'
#' @examples
#' x <- c("M")
#' recode(x, type = "gender")
recode <- function(x, type = NULL) {

  if (is.null(type)) {
    stop("No type specified. Must specify type, e.g. gender")
  }

  x <- tolower(x)

  if (type == "race") {
    x <-
      data.table::fcase(
        x %in% c("asian/pacific islander",
                 "asian / pacific islander",
                 "native hawaiian or other pacific islander"),
        "asian_pacific_islander",
        x %in% c("black",
                 "black hispanic"),
        "black",
        x %in% c("hispanic",
                 "white hispanic",
                 "spanish (do not use)"),
        "hispanic",
        x %in% c("amer ind/alaskan native",
                 "amer indian / alaskan native"),
        "indigenous",
        x %in% c("white"),
        "white",
        default = NA
      )
  }

  if (type == "gender") {
    x <-
      data.table::fcase(
        x %in% c("female",
                 "f"),
        "female",
        x %in% c("male",
                 "m"),
        "male",
        default = NA
      )
  }

  return(x)

}


