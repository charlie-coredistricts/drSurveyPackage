library("googlesheets4")
library("googledrive")
library("crosswalkr")
library("dplyr")
library("tidyr")
library("CORE")
library("readr")

#' Title
#'
#' @param googleSheetExtract
#'
#' @return
#' @export
#'
#' @examples
cleanGoogle <- function(google_url) {
  googlesheets4::gs4_auth()
  googleSheetExtract <- googlesheets4::read_sheet(google_url, col_types = "c")

  googleSheetExtract$`Student ID Number` = as.numeric(googleSheetExtract$`Student ID Number`)
  googleSheetExtract$email_domain <- gsub(".*@","",googleSheetExtract$`Email Address`)


  long_file <- pivot_longer(googleSheetExtract,7:174) %>%
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
  print("language filtered file is...")
  print(head(long_file_language_filtered))

  # pivot the data back to wide
  pivoted <-  long_file_language_filtered %>%
    select(-name) %>%
    group_by(Student.ID.Number) %>%
    pivot_wider( names_from = question_extract, values_from = value) %>%
    ungroup()

  # google uses email as the merge identifier
  # pivoted$merge_identifier = pivoted$Email.Address
  # pivoted$n <- pivoted$n - (as.numeric(!is.na(pivoted$FR21)) + as.numeric(!is.na(pivoted$FR22)) + as.numeric(!is.na(pivoted$FR23)) + as.numeric(!is.na(pivoted$FR24)))
  return(pivoted)

}
