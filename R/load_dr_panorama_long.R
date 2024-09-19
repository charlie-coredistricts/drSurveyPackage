library(googlesheets4)
library(googledrive)
library(crosswalkr)
library(dplyr)
library(tidyr)
library(CORE)
library(readr)
library(readxl)
library(gsubfn)
options(scipen = 999)

cleanPanoramaLong <- function(dr_panorama_long_df) {
  # print("original df:")
  # print(dr_panorama_long_df)

  formatQuestions <- function(qNum) {
    if (strtoi(qNum) < 22) {
      return(paste("q",qNum,sep = ""))
    }
    else {
      return(paste("fr",qNum,sep = ""))
    }
  }
  # print("only results within window:")
  # print(dr_panorama_long_df)

  format_pano <- function(df){
    head(df)
    df <- df %>%
      mutate(converted_date = format(as.POSIXct(timestamp_utc, format="%Y-%m-%dT%H:%M:%OS", tz="UTC"), "%Y-%m-%d %H:%M:%S")) %>%
      filter(converted_date >= staticValues$survey_window$start)

    # dr_panorama_long_df <- dr_panorama_long_df %>%
    #   mutate(converted_date = gsubfn('.',list('T' = ' ', 'Z' = ''), submission_time),
    #          converted_date = substr(converted_date,1,nchar(converted_date,4)))
    #   filter(converted_date >= staticValues$survey_window$start)

    df$questions <- lapply(df$question_order,formatQuestions)
    df$select_one_score[is.na(df$select_one_score)] <- ""
    df$values <- paste(df$free_response_text,na.omit(df$select_one_score),sep = "")

    # pano_student_answered <- dr_panorama_long_df %>%
    #   filter(select_one_score != "") %>%
    #   group_by(student_id,converted_date) %>%
    #   count(student_id)

    # print(pano_student_answered)

    pano_wide_data <- df %>%
      select(local_id, response_language, cds, district_name, converted_date, questions, values) %>%
      group_by(local_id) %>%
      filter(converted_date == max(converted_date)) %>%
      ungroup() %>%
      pivot_wider(names_from = questions,values_from = values) %>%
      distinct(local_id, .keep_all = TRUE) %>%
      rename(fr1 = fr22,
             fr2 = fr23,
             fr3 = fr24) %>%
      mutate(cds = gsubfn('.',list('"' = '', '[' = '', ']' = ''), cds),
             cds = str_remove_all(cds, " School")) %>%
      mutate(district_name = str_remove_all(district_name, " School District"))

    # print("pivot wide:")
    # print(pano_wide_data)
    # names(pano_wide_data)

    return(pano_wide_data)

  }

  dr_panorama_long_formatted <- format_pano(dr_panorama_long_df)

  # rename Yes/No from different languages
  yes_no_xwalk <- read.csv("./crosswalk/yes_no_xwalk.csv", encoding = 'UTF-8')

  dr_panorama_long_formatted <- dr_panorama_long_formatted %>%
    left_join(yes_no_xwalk)

  dr_panorama_long_formatted$q21 <- dr_panorama_long_formatted$yes_no_clean
  dr_panorama_long_formatted$yes_no_clean <- NULL

  # rename response_language
  panorama_language_xwalk <- read.csv("./crosswalk/panorama_language_xwalk.csv", encoding = 'UTF-8')

  dr_panorama_long_formatted <- dr_panorama_long_formatted %>%
    left_join(panorama_language_xwalk)

  dr_panorama_long_formatted$response_language <- dr_panorama_long_formatted$response_language_clean
  dr_panorama_long_formatted$response_language_clean <- NULL

  # merge cds
  # cds_xwalk <- read_xlsx("./crosswalk/cds_xwalk.xlsx")

  dr_panorama_long_formatted <- dr_panorama_long_formatted %>%
    # left_join(cds_xwalk, join_by(cds == School, district_name == District)) %>%
    # select(-c(district_name, cds)) %>%
    select(-district_name) %>%
    rename(timestamp_utc = converted_date) %>%
    mutate(student_email = NA)

  return(dr_panorama_long_formatted)

  # print("formatted:")
  # print(dr_panorama_long_formatted)
}
