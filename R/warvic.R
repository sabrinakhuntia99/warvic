#' Categorize war victim data by sex
#'
#' @param infile A data file with a sex column.
#' @return The number of \code{M}, \code{F}, and \code{U} deaths.
#' @examples
#' sexcount('example.csv')
#' @export
sexcount <- function(infile){
  datset <- data.table::fread(infile, header = TRUE)
  M_count <- dplyr::count(datset, sex == "M")
  F_count <- dplyr::count(datset, sex == "F")
  U_count <- dplyr::count(datset, sex == "U")
  M_count <- M_count[2,2]
  F_count <- F_count[2,2]
  U_count <- U_count[2,2]
  D_sexcount <- data.frame(
    name=c("Male", "Female", "Unknown"),
    value=c(M_count, F_count, U_count)
  )
  barplot(height=D_sexcount$value, names=D_sexcount$name,
          xlab="Sex",
          ylab="Number of Deaths",
          main="Death Toll by Sex"
  )
}

#' Categorize war victim data by death identification
#'
#' @param infile A data file with an ID column.
#' @return The number of deaths classified by ABA (\code{aba}) and HRW (\code{HRW}) or exhumed (\code{EXH}).
#' @examples
#' deathcount('example.csv')
#' @export
deathcount <- function(infile){
  datset <- data.table::fread(infile, header = TRUE)
  aba_count <- dplyr::count(datset, vars = aba)
  exh_count <- dplyr::count(datset, vars = exh)
  hrw_count <- dplyr::count(datset, vars = hrw)
  aba_count <- aba_count[2,2]
  exh_count <- exh_count[2,2]
  hrw_count <- hrw_count[2,2]
  D_deathcount <- data.frame(
    name=c("ABA", "Exhumation", "HRW"),
    value=c(aba_count, exh_count, hrw_count)
  )
  barplot(height=D_deathcount$value, names=D_deathcount$name,
          xlab="Death Identification Type",
          ylab="Death Toll",
          main="Deaths by Identification Type"
  )
}

#' Categorize war victim data by death date records.
#'
#' @param infile A data file with a death weight column.
#' @return The number of deaths with a complete date (\code{1}) and incomplete date (\code{0.33}).
#' @examples
#' deathwt('example.csv')
#' @export
deathwt <- function(infile){
  datset <- data.table::fread(infile, header = TRUE)
  whole_count <- dplyr::count(datset, weight = "0.33")
  part_count <- dplyr::count(datset, weight = "1")
  whole_count <- whole_count[2,2]
  part_count <- part_count[2,2]
  D_wtcount <- data.frame(
    name=c("Complete", "Imputed"),
    value=c(whole_count, part_count)
  )
  barplot(height=D_wtcount$value, names=D_wtcount$name,
          xlab="Weight",
          ylab="Toll",
          main="Deaths by Record Date"
  )
}
