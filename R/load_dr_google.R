library(googlesheets4)
library(googledrive)
library(crosswalkr)
library(dplyr)
library(tidyr)
library(CORE)
library(readr)
library(readxl)

#' Title
#'
#' @param google_sheet_read
#'
#' @return
#' @export
#'
#' @examples
cleanGoogle <- function(google_sheet_read) {
  # googlesheets4::gs4_auth()
  # googleSheetExtract <- googlesheets4::read_sheet(google_url, col_types = "c")

  # googleSheetExtract$`Student ID Number` = as.numeric(googleSheetExtract$`Student ID Number`)
  # googleSheetExtract$email_domain <- gsub(".*@","",googleSheetExtract$`Email Address`)

  google_sheet_read$`Student ID Number` <- as.numeric(google_sheet_read$`Student ID Number`)
  # google_sheet_read$email_domain <- gsub(".*@","",google_sheet_read$`Email Address`)

  long_file <- pivot_longer(google_sheet_read,7:174) %>%
    dplyr::rename_all(list(~make.names(.)))

  # extract the language and the question number
  long_file$language_extract <- substr(long_file$name,1,1)
  long_file$name <- gsub("\\..*","",long_file$name)
  long_file$question_extract <- mapply(sub,long_file$language_extract,x=long_file$name,replacement = "")

  # Identify which language each student surveyed in
  student_language <- long_file %>%
    filter(!is.na(value)) %>%
    group_by(Email.Address) %>%
    count(language_extract)

  # create a function that extracts the values we want i.e. number for the multiple
  # choice questions and full value for the free response
  formatAnswers <- function(qNum,qVal) {
    # this line is a little confusing, but since we parsed the numbers in
    # qnum out of a string, we have to convert to integer so the comparison works properly

    if (as.integer(qNum) < 21) {
      return(as.integer(substr(qVal,1,1)))
    }
    else {
      return(qVal)
    }
  }

  formatQuestions <- function(qNum) {
    if (as.integer(qNum) < 22) {
      return(paste("q",qNum,sep = ""))
    }
    else {
      return(paste("fr",qNum,sep = ""))
    }
  }

  # keep only the rows with the right language for each kid
  long_file_language_filtered <- inner_join(long_file,student_language)
  # format the questions and the answers
  long_file_language_filtered$value <- mapply(formatAnswers,long_file_language_filtered$question_extract,long_file_language_filtered$value)
  long_file_language_filtered$question_extract <- lapply(long_file_language_filtered$question_extract,formatQuestions)

  # print(names(long_file_language_filtered))
  # print("language filtered file is...")
  # print(head(long_file_language_filtered))

  # pivot the data back to wide
  pivoted <-  long_file_language_filtered %>%
    select(-name) %>%
    group_by(Student.ID.Number) %>%
    pivot_wider(names_from = question_extract, values_from = value) %>%
    ungroup() %>%
    select(Email.Address, Student.ID.Number, language_extract, Timestamp, School.Name,
           q1, q2, q3, q4, q5, q6, q7, q8, q9, q10,
           q11, q12, q13, q14, q15, q16, q17, q18, q19, q20, q21,
           fr22, fr23, fr24) %>%
    rename(student_email = Email.Address,
           local_id = Student.ID.Number,
           response_language = language_extract,
           timestamp_utc = Timestamp,
           fr1 = fr22,
           fr2 = fr23,
           fr3 = fr24)

  # rename Yes/No from different languages
  yes_no_xwalk <- read.csv("./crosswalk/yes_no_xwalk.csv", encoding = 'UTF-8')

  pivoted <- pivoted %>%
    left_join(yes_no_xwalk)

  pivoted$q21 <- pivoted$yes_no_clean
  pivoted$yes_no_clean <- NULL

  # rename response_language
  google_language_xwalk <- read.csv("./crosswalk/google_language_xwalk.csv", encoding = 'UTF-8')

  pivoted <- pivoted %>%
    left_join(google_language_xwalk)

  pivoted$response_language <- pivoted$response_language_clean
  pivoted$response_language_clean <- NULL

  # merge cds
  cds_xwalk <- read_xlsx("./crosswalk/cds_xwalk.xlsx")

  # pivoted <- pivoted %>%
  #   left_join(cds_xwalk, join_by(School.Name == School)) %>%
  #   select(-c(District, School.Name)) %>%
  #   rename(cds = CDSCode)

  # identical School Name showed up for multiple districts, leading to duplicate rows when joined;
  # manual fix to add district name to join correctly
  pivoted$District.Name <- ''
  pivoted <- pivoted %>%
    left_join(cds_xwalk, join_by(School.Name == School, District.Name == District)) %>%
    select(-c(District.Name, School.Name)) %>%
    rename(cds = CDSCode)

  # google uses email as the merge identifier
  # pivoted$merge_identifier = pivoted$Email.Address
  # pivoted$n <- pivoted$n - (as.numeric(!is.na(pivoted$FR21)) + as.numeric(!is.na(pivoted$FR22)) + as.numeric(!is.na(pivoted$FR23)) + as.numeric(!is.na(pivoted$FR24)))


  return(pivoted)

}
