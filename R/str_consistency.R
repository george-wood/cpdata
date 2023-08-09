#' Ensure string consistency for race and ethnicity or gender
#'
#' @import data.table
#'
#' @param x Vector of race and ethnicity or gender values
#' @param feature Name of feature; currently accepts "race" or "gender"
#'
#' @return Consistent values of race and ethnicity or gender feature
#' @export
#'
#' @examples
#' x <- c("M")
#' str_consistency(x, type = "gender")
str_consistency <- function(x, feature = NULL) {

  if (is.null(feature)) {
    stop("No feature specified. Must specify feature, e.g. gender")
  }

  x <- tolower(x)

  if (feature == "race") {
    x <-
      data.table::fcase(
        x %in% c("asian/pacific islander",
                 "asian / pacific islander",
                 "native hawaiian or other pacific islander"),
        "ASIAN_PACIFIC_ISLANDER",
        x %in% c("black"),
        "BLACK",
        x %in% c("black hispanic"),
        "BLACK HISPANIC",
        x %in% c("hispanic",
                 "spanish (do not use)"),
        "HISPANIC",
        x %in% c("white hispanic"),
        "WHITE HISPANIC",
        x %in% c("amer ind/alaskan native",
                 "amer indian / alaskan native"),
        "AMERICAN_INDIAN_ALASKAN_NATIVE",
        x %in% c("white"),
        "WHITE",
        default = NA
      )
  }

  if (feature == "gender") {
    x <-
      data.table::fcase(
        x %in% c("female",
                 "f"),
        "FEMALE",
        x %in% c("male",
                 "m"),
        "MALE",
        default = NA
      )
  }

  x

}


