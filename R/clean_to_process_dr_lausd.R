clean_to_process_dr_lausd <- function(district,period,raw_file_xwalk) {
  # print(raw_file_xwalk$CORE_Value)
  if(staticValues$clean_files$dr_lausd_survey %in% raw_file_xwalk$CORE_Value){

    writeLines("\n Calling DR LAUSD - clean_to_process")

    dr_lausd_df <- open_file(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_clean,
      file_type = staticValues$clean_files$dr_lausd_survey
    )

    # print(dr_lausd_df)
    dr_lausd_df <- dr_lausd_df %>%
      filter(response != 0) %>%
      mutate(student_email = NA,
             timestamp_utc = NA,
             response_language = NA) %>%
      select(-response)

    # recode Yes/No
    yes_no_xwalk <- read.csv("./crosswalk/yes_no_xwalk.csv", encoding = 'UTF-8')

    dr_lausd_df <- dr_lausd_df %>%
      left_join(yes_no_xwalk)

    dr_lausd_df$q21 <- dr_lausd_df$yes_no_clean
    dr_lausd_df$yes_no_clean <- NULL

    dr_lausd_df_process_path <- universal_file_path_helper(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_process,
      ### TO DO across all clean_to_process
      file = staticValues$data_elements$dr_survey # data_elements instead of clean_files; dr_survey should be the only clean_files name
    )
    saveOrArchive(dr_lausd_df,dr_lausd_df_process_path)
  }
}
